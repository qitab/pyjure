(ns pyjure.continuation-analysis
  (:use [clojure.core.match :only [match]]
        [pyjure.debug]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.runtime]))

(comment
"
TODO remember for each expression whether the resulting value is used or not.
TODO? rename this pass appropriately when it's done. liveness_analysis_1 ?
TODO? treat tail positions specially? they mightn't need capture return / loop in the same way.
TODO? head positions also are surefire, not maybe, in not needing to capture an earlier exception.

This pass will do a backward analysis to annotate each node
with which effects its *future* is capturing:
reading variables, catching returns, catching exceptions,
catching breaks and continues, catching yields.
The further code generator can then properly generate code for it.

Invariant: if the pass returns without an error, only metadata is changed.

Information is stored in metadata for each node.

The analysis state is a map with keys being:
:vars (map variable names (strings) to FURAL values)
:continuations (map the following to FURAL values for 'this effect may happen in the continuation'
  actually FAL for all but yield, since the language has no multiple-use continuations;
  we could include :raise, but at this point, before type analysis,
  too few things are guaranteed NOT to raise for that to bring useful information.)
  :suite (nil, this is in tail position; :linear, this is not in a loop; true, this is in a loop)
  :return (:linear, this will return; :affine, this will maybe return; nil this never returns)
  :raise (:linear, this raise an exception; :affine, this will maybe raise; nil this never raises)
  :break
  :continue
  :yield
:effects (map the following to FL value for 'if this effect happens, it needs to be reified')
  ??? :value (the continuation uses the value of this particular expression)
  :return (the continuation captures whether this part of the code returns),
  :raise (the continuation captures whether this part of the code raises exceptions),
  :break (we're in a loop, and the code may break)
  :continue (we're in a loop, and the code continue)
  :yield (we're in a generator, and the code may capture continuations, boolean).
  :finally (the continuation captures bindings for non-local exits),

FURAL values for variables may be which may be
false (variable shadowed before it is read) or equivalently nil (variable not captured),
:linear (variable read exactly once), :affine (variable read zero or one time),
:required (variable read one or more times), true (Unconstrained, variable read zero or more times).

Problem: effects to the end of the branch vs all effects including beyond the current branch.
")

(defn env-combine [fun E E']
  (into {} (map #(do [% (mapcombine fun (% E) (% E'))]) [:vars :continuations :effects])))
(defn env-either [E E'] (env-combine f-either E E'))
(defn env-both [E E'] (env-combine f-both E E'))

;;(def value-env {:effects {:value :linear}})
;;(defn &using-value
;;  ([&x fural] (&let [x &x] (with-meta x (assoc-in (meta x) [:capturing :effects :value] fural))))
;;  ([&x] (&using-value &x true)))

(declare &A &A* &Aargs)

(defn &A* [xs] (&let [r (&map &A (reverse xs))] (vec (reverse r))))
(defn &Aargs [x] ;; &using-value everywhere
  (if-let [[args star-arg more-args kw-arg] x]
    (&let [kw-arg (&A kw-arg)
           more-args (&A* more-args)
           star-arg (&A star-arg)
           args (&A* args)]
          [(vec args) star-arg (vec more-args) kw-arg])
    &nil))

(defn &A [x]
  ;; NB: reverse order matters in listing effect capturing
  (&let [E (&get-in []) *
    (letfn [(&r [a] (&return (with-meta a (assoc (meta x) :capturing E))))
            (&v [h & as] (&bind (&A* as) #(&r (into h %))))]
      (match [x]
        [nil] &nil
        [[:id s]] (&do (&update-in [:vars s] f-once-more) (&r x))
        [[:bind [:id s] :as n a]] (&let [_ (&assoc-in [:vars s] false)
                                         a (&A a) ;; (&using-value ...)
                                         * (&r [:bind n a])])
        [[:unbind [:id s]]] (&do (&assoc-in [:vars s] false) (&r x))
        [[:constant c]] (&r x)
        [[:builtin f & a]] (&let [a (&A* a) * (&r (vec* :builtin f a))])
        [[:call f a]] (&let [a (&Aargs a) f (&A f) * (&r [:call f a])])
        [[:suite & a]] (&let [a (&A* a) * (&r (vec* :suite a))])
        [[:module a]] (&let [a (&A a) * (&r [:module a])])
        [[h :guard #{:return :raise} a]]
        (&let [_ (fn [E] [nil {:vars nil :continuations {h :linear} :effects (:effects E)}])
               a (&A a)
               * (&r [h a])])
        [[:if test body else]]
        (fn [E] (let [E' (-> E (assoc-in [:vars] nil)
                             (assoc-in [:effects :return] :linear))
                      [body Eb] ((&A body) E')
                      [else Ee] ((&A else) E')
                      E'' (assoc-in (env-both (env-either Eb Ee) E)
                                    [:effects :return] (get-in E [:effects :return]))
                      [test E'''] ((&A test) E'')]
                  ((&r [:if test body else]) E''')))
        [[:while test body else]]
        (fn [E]
          (let [[else E] ((&A else) E)
                E0 {:vars nil, :continuations nil,
                    :effects (merge (:effects E) {:return :linear, :break :linear, :continue :linear})}
                [body Eb] ((&A body) (assoc-in E0 [:continuations :continue] :linear))
                [test Et] ((&A test) (env-either Eb (assoc-in E0 [:continuations :break] :linear)))
                bmul (get-in Et [:continuations :break])
                cmul (get-in Et [:continuations :continue])
                varsk (get E :vars)
                varst (get Et :vars)
                varsb (get Eb :vars)
                varsb* (mapcombine f-both
                                   (mapmap (f-multiplier bmul) varsk)
                                   (if cmul (mapcombine #(f-repeat (f-both %1 (f-* cmul %2)))
                                                        varsb varst)
                                       varsb))
                varst+ (mapcombine f-both varst (mapcombine f-either varsk varsb*))
                E1 (assoc-in E [:vars] varst+)]
            ((&r [:if test body else]) E1)))
        [[h :guard #{:yield :yield-from} a]] (&let [a (&A a) * (&r [h a])])
        [[:argument id type default]]
        ;; handles argument in the *outer* scope where the function is defined,
        ;; *NOT* in the inner scope that the function defines (see :function for that)
        (&let [default (&A default) type (&A type) * (&r [:argument id type default])])
        [[(:or ':from ':import ':constant ':break ':continue) & _]] (&r x)
        [[:function args return-type body]]
        (&let [return-type (&A return-type) ; handle type and default
               args (&Aargs args)
               * (let [[body innerE] ((&A body) nil)]
                   (&r [:function args return-type body]))])
        ;; TODO: handle the things below properly
        [[:unwind-protect body protection]]
        (let [[protection Ep] ((&A protection) nil)
              [body Eb] (&A body nil) ;; TODO: the very first thing in body isn't optional
              ;; _ (&update-in [:capturing] nil)
              E' (env-both (assoc-in Ep [:effects :raise] nil) Eb)]
          ((&r [:unwind-protect body protection]) E'))
        [[:handler-bind body handler]]
        (&let [handler (&A handler nil)
               body (&A body (f-maybe nil))] ;; TODO: the very first thing in body isn't optional
              (&r [:unwind-protect body handler]))
        [[:class [:id s] :as name args body]]
        (&let [args (&Aargs args)
               _ (&assoc-in [:vars s :bound?] true)]
              (let [[body innerE] ((&A body) nil)]
                (if (:generator? innerE)
                  ($syntax-error x "invalid yield in class %a" [:name] {:name s})
                  (&r [:class name args body]))))
        :else ($syntax-error x "unexpected expression %s during continuation analysis pass")))]))

(defn analyze-continuations- [x]
  (first ((&A x) nil)))
