(ns skylark.effect-analysis
  (:use [clojure.core.match :only [match]]
        [skylark.debug :exclude [analyze-effects]]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.runtime]))

(comment
"TODO? rename this pass appropriately when it's done. type_analysis_1 ?
TODO? treat tail positions specially? they mightn't need capture return / loop in the same way.
TODO? head positions also are surefire, not maybe, in not needing to capture an earlier exception.

This pass will do a forward analysis to annotate each node with
which effects are present in its *past*:
binding variables (to non-null values, or maybe only null),
?forcing some conditions to be true and/or variables to be bound to values of some given types.

Invariant: if the pass returns without an error, only metadata is changed.

Information is stored in metadata for each node.

The analysis state is a map with keys being:
:vars (map variable names (strings) to FURAL values for 'this variable is initialized')
?TODO :preconditions (map the following to FURAL values):
:effects (map the following to FURAL value for 'this effect happened in the past')
  :suite (continue to the current (input) or next (output) expression or statement)
  :return (the continuation captures whether this part of the code returns),
  :raise (the continuation captures whether this part of the code raises exceptions),
  :break (we're in a loop, and the code may break)
  :continue (we're in a loop, and the code continue)
  :yield (we're in a generator, and the code may capture continuations, boolean).

Problem: effects to the end of the branch vs all effects including beyond the current branch.
")

(defn env-combine [fun E E']
  (into {} (map #(do [% (mapcombine fun (% E) (% E'))]) [:vars :effects])))
(defn env-either [E E'] (env-combine f-either E E'))
(defn env-both [E E'] (env-combine f-both E E'))
(defn env-then [E E'] (if (get-in E [:effects :suite]) (env-both E E') E))
(defn env-map [E fun] (into {} (map #(do [% (map fun (% E))]) [:vars :effects])))

(def null-env {:effects {:suite :linear}})


(declare &A)

(defn &A* [xs] (&map &A xs))
(def &Aargs (&args &A))

(defn reachable? [E] (get-in E [:effects :suite]))
(defn &unreachable-afterwards [E] [nil {:vars nil :effects nil}])
(defn with-effects [E x a] (with-meta a (assoc (meta x) :effecting E)))
(defn &with-effects [x a] (fn [E] [(with-effects E x a) E]))

(defn &A1 [x] ;; analyze the effects of one reachable form
  (letfn [(&r [a] (&with-effects x a))
          (&v [h & as] (&bind (&A* as) #(&r (into h %))))]
    (match [x]
      [nil] &nil
      [[:id s]] (&do (&update-in [:vars s] f-at-least-once) (&r x)) ;; after that, we know it's not nil!
      [[:bind [:id s] :as n a]]
      (&let [a (&A a)
             _ (&assoc-in [:vars s] :linear) ;; after that, defined once
             * (&r [:bind n a])])
      [[:unbind [:id s]]]
      (&do (&assoc-in [:var s] false) (&r x)) ;; after that, it's unbound
      [[:constant c]] (&r x)
      [[:builtin f & a]] (&let [a (&A* a) * (&r (vec* :builtin f a))])
      [[:call f a]] (&let [f (&A f) a (&Aargs a) * (&r [:call f a])])
      [[:suite & a]] (&let [a (&A* a) * (&r (vec* :suite a))])
      [[h :guard #{:return :raise} a]]
      (&let [a (&A a)
             _ (&assoc-in [:effects h] :linear)
             _ &unreachable-afterwards
             * (&r [h a])])
      [[:if test body else]]
      (fn [E] (let [[test E'] ((&A test) E)
                    [body Eb] ((&A body) null-env)
                    [else Ee] ((&A else) null-env)
                    E'' (env-then E' (env-either Eb Ee))]
                ((&r [:if test body else]) E'')))
      [[:while test body else]]
      (fn [E] (let [[test Et] ((&A test) null-env)
                    [body Eb] ((&A body) null-env)
                    bmul (get-in Eb [:effects :break])
                    cmul (get-in Eb [:effects :continue])
                    El (-> (env-then Et Eb)
                           (update-in [:effects] #(merge % {:break nil, :continue nil, :suite :linear}))
                           (env-map (f-multiplier bmul)))
                    [else E'] ((&A else) El)]
                ((&r [:while test body else]) E')))
       [[h :guard #{:yield :yield-from} a]]
       (&let [a (&A a)
              _ (&update-in [:effects :yield] #(f-both % (if (= h :yield-from) true :linear)))
              * (&r [h a])])
      [[:argument id type default]]
      ;; handles argument in the *outer* scope where the function is defined,
      ;; *NOT* in the inner scope that the function defines (see :function for that)
      (&let [type (&A type) default (&A default) * (&r [:argument id type default])])
      [[(:or ':from ':import ':constant ':break ':continue) & _]] (&r x)
      [[:function args return-type body]]
      (&let [args (&Aargs args)
             return-type (&A return-type) ; handle type and default
             * (let [[body innerE] ((&A body) null-env)]
                 (&r [:function args return-type body]))])
      ;; TODO: handle the things below properly
      [[:unwind-protect body protection]]
      (fn [E] (let [[body Eb] ((&A body) null-env) ;; TODO: the very first thing in body isn't optional
                    [protection Ep] ((&A protection) null-env)
                    E' (env-both (env-map f-maybe Eb) Ep)]
                ((&r [:unwind-protect body protection]) (env-then E E'))))
      [[:handler-bind body handler]]
      (fn [E] (let [[body Eb] ((&A body) null-env)
                    [handler Eh] (&A handler null-env) ;; TODO: the very first thing in body isn't optional
                    E' (env-map f-maybe (env-both Eb Eh))]
                ((&r [:handler-bind body handler]) (env-then E E'))))
      [[:module a]] (&let [a (&A a) * (&r [:module a])])
      [[:class [:id s] :as name args body]]
      (&let [args (&Aargs args)
             _ (&assoc-in [:vars s :bound?] true)]
            (let [[body innerE] ((&A body) null-env)]
              (&r [:class name args body])))
      :else ($syntax-error x "unexpected expression %s during effect analysis pass"))))

(defn &A [x]
  (fn [E] (if (reachable? E) ((&A1 x) E) [(with-effects E x x) E])))

(defn analyze-effects [x]
  (first ((&A x) null-env)))
