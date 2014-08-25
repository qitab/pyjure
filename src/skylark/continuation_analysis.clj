(ns skylark.continuation-analysis
  (:use [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.runtime]))

(comment
"TODO: rename this pass appropriately when it's done.

This pass will do a backward analysis to annotate each node
with which effects its *future* is capturing:
reading variables, catching returns, catching exceptions,
catching breaks and continues, catching yields.
The further code generator can then properly generate code for it.

Invariant: if the pass returns without an error, only metadata is changed.

Information is stored in metadata for each node.

The analysis state is a map with keys being:
:vars (map variable names (strings) to FURAL values)
:effects (map the following to boolean)
  :return (the continuation captures whether this part of the code returns),
  :except (the continuation captures whether this part of the code raises exceptions),
  :finally (the continuation captures bindings for non-local exits),
  :loop (we're in a loop, and the code may break or continue, boolean)
  :continuation (we're in a generator, and the code may capture continuations, boolean).

FURAL values for variables may be
which may be nil (variable not captured), false (variable shadowed before it is read),
:linear (variable read exactly once), :affine (variable read zero or one time),
:required (variable read one or more times), true (unconstrained, variable read zero or more times).

Problem: effects to the end of the branch vs all effects including beyond the current branch.
")

(defn map-capturing-vars [f E] (update-in E [:capturing :vars] #(mapmap f %)))

(declare &A &A* &Aargs)

(defn &A* [xs] (&let [r (&map &A (reverse xs))] (vec (reverse r))))
(defn &Aargs [x]
  (if-let [[args star-arg more-args kw-arg] x]
    (&let [kw-arg (&A kw-arg)
           more-args (&A* more-args)
           star-arg (&A star-arg)
           args (&A* args)]
          [(vec args) star-arg (vec more-args) kw-arg])
    &nil))

(defn capvars [E] (get-in E [:capturing :vars]))

(defn &A [x]
  ;; NB: reverse order matters in listing effect capturing
  (letfn [(&r [a] (fn [E] (with-meta a (assoc (meta x) :capturing (:capturing E)))))
          (&v [h & as] (&bind (&A* as) #(&r (into h %))))]
    (match [x]
      [nil] &nil
      [[:id s]] (&do (&update-in [:capturing :vars s] use-once-more) (&r x))
      [[:bind [:id s] :as n a]] (&let [_ (&assoc-in [:capturing :vars s] false)
                                       a (&A a)
                                       * (&r [:bind n a])])
      [[:unbind [:id s]]] (&do (&assoc-in [:capturing s] false) (&r x))
      [[:builtin f & a]] (&let [a (&A* a) * (&r (vec* :builtin f a))])
      [[:call f a]] (&let [a (&Aargs a) f (&A f) * (&r [:call f a])])
      [[:suite & a]] (&let [a (&A* a) * (&r (vec* :suite a))])
      [[h :guard #{:return :raise} a]]
      (&let [_ (fn [E] [nil (if (or (get-in E [:capturing :finally])
                                    (and (= h :raise) (get-in E [:capturing :except])))
                              E
                              (assoc-in E [:capturing :vars] nil))])
             a (&A a)
             * (&r [h a])])
      [[:if test body else]]
      (fn [E] (let [E' (-> E (assoc-in [:capturing :vars] nil)
                           (assoc-in [:capturing :effects :return] true))
                    [body Eb] ((&A body) E')
                    [else Ee] ((&A else) E')
                    vars' (mapcombine use-either (capvars Eb) (capvars Ee))
                    vars'' (mapcombine use-both vars' (capvars E))
                    E'' (assoc-in E [:capturing :vars] vars'')
                    [test E'''] ((&A test) E'')]
                ((&r [:if test body else]) E''')))
      [[:while test body else]]
      ;; TODO: don't do this double traversal here: it makes this pass exponential!
      ;; instead, have another pass (explicit or merged with next) for the repeating.
      (comment
      (fn [E]
        (let [[else E] ((&A else) E)
              E0 (-> E
                     (assoc-in [:capturing :vars] nil)
                     (update-in [:capturing :effects] #(merge % {:return true, :loop true}))
                     (assoc-in [:continuations] {:break {}, :continue {}}))
              [body Eb1] ((&A body) E0)
              [test Et1] ((&A test) E0)
              varsk (capvars E)
              varsb (mapmap use-maybe-repeat (capvars Eb1))
              varst (mapmap use-repeat (capvars Et1))
              varsbk (mapcombine use-both varsb varsk)
              varstbk (mapcombine use-both varst varsbk)
              E1 (-> E0
                     (assoc-in [:capturing :vars] varstbk)
                     (assoc-in [:continuations] {:break varsk, :continue varstbk}))
              [body Eb*] ((&A body) E1)
              E2 (assoc-in E [:capturing :vars] (mapcombine use-either (capvars Eb*) varsbk))
              [test E3] ((&A test) E2)
              E4 (assoc-in E [:capturing :vars] (capvars E3))]
          ((&r [:if test body else]) E4)))
      [[h :guard #{:yield :yield-from} a]] (&let [a (&A a) * (&r [h a])])
      [[:argument id type default]]
      ;; handles argument in the *outer* scope where the function is defined,
      ;; *NOT* in the inner scope that the function defines (see :function for that)
      (&let [default (&A default) type (&A type) * (&r [:argument id type default])])
      [[(:or ':from ':import ':constant ':break ':continue) & _]] (&r x)
      [[:function args return-type body]]
      (&let [return-type (&A return-type) ; handle type and default
             args (&Aargs args)]
            (let [[body innerE] ((&A body) nil)]
              (&r [:function args return-type body])))
      [[:unwind-protect body protection]]
      (&let [[protection Ep] ((&A protection) nil)
             [body Eb] (&A body nil) ;; TODO: the very first thing in body isn't optional
             ;; _ (&update-in [:capturing] #())
             * (&r [:unwind-protect body protection])])
      [[:handler-bind body handler]]
      (&let [handler (&A handler use)
             body (&A body (use-maybe use))] ;; TODO: the very first thing in body isn't optional
         (&r [:unwind-protect body handler]))
      [[:class [:id s] :as name args body]]
      (&let [args (&Aargs args)
             _ (&assoc-in [:vars s :bound?] true)]
            (let [[body innerE] ((&A body) nil)]
              (if (:generator? innerE)
                ($syntax-error x "invalid yield in class %a" [:name] {:name s})
                (&r (check-effects x innerE false) :class name args body))))
      :else ($syntax-error x "unexpected expression %s during continuation analysis pass")))))

(defn analyze-continuations [[x E]]
  ((&A x) E))
