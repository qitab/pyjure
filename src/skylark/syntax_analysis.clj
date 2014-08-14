(ns skylark.syntax-analysis
  (:use [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.runtime]))

;; This pass will
;; * identify for each scope (function or class)
;;  and each variables which is its scoping status:
;;  one of param, local, nonlocal, global.
;;  Issue an error if a variable has conflicting uses.
;;  TODO: if we allow for lexical macros, the previous pass needs have noted them.
;; * detect which functions are generators
;;  (a yield statement taints the function even if it's going to be optimized away later)
;; * TODO detect other syntactically present side-effects in the function
;;  (e.g. skylark rule definition, incremental class definition, etc.)
;;
;; Information is stored in metadata for the function or class node.
;; further passes must be careful to NOT blindly copy that meta-data,
;; but only the meta data they know they preserve... ahem.
;; TODO: maybe store analysis in another field?

;; TODO: adapt what environments we store to what we actually need later
;; TODO: maybe store for each expression the state of the environment before it?
;;   can be slightly tricky in a branch inside a loop:
;;   which bindings exactly are visible from there?

;; The analysis state tracks a local and global environment
;; and a record of the effects that (may) happen in the current scope.
(defrecord AnalysisState [locals globals effects]) ;; environment is for macro-expansion

;; A (local) Environment maps identifiers to values, and has an optional parent Environment
;; We only need it for macros. Actually, if we stick to global macros that can't be shadowed,
;; we don't need it at this stage.
(defrecord Environment [vars parent])

;; effects are things that may happen within a scope.
;; at every point, we compute the effects before and after that point in the current scope;
;; effects are thus a monoid that can be composed forward and backward.
(defrecord Effects [vars yield? return? raise?])
(def null-effects (->Effects #{} false false false))

(defrecord ScopeVarStatus [locality assigned? refered?])
;; locality can be :param, :local, :nonlocal, :global
;; assigned? can be false, true, :⊤
;; refered? can be false, true, :macro, :⊤
;; An error will be raised at compile-time if there is a conflict

(def null-syntax-analysis-state (->AnalysisState nil $initial-environment null-effects))


(declare A Alhs)

(defn A* [head xs E] (thread-args head A xs E))
(defn Alhs* [head fields xs E] (thread-args head #(Alhs fields % %2) xs E))

(defn Alhs [fields x E]
  (cond
   (symbol? x)
   [x (update-in-multiple E fields conj x)]
   (seq? x)
   (let [[h & t] x
         rec (fn [] (Alhs* (list h) fields t E))]
     (case h
       (:tuple :list :del) ;; del is not pure
       (rec)
       (:star)
       (rec)
       (:subscript) ;; not pure
       (let [[o ss] t
             [o E] (Alhs fields o E)]
         (A* (list h o) ss E))
       (:attribute) ;; not pure
       (let [[o n] t
             [o E] (Alhs fields o E)]
         [(list h o n) E])
       (:argument) ;; for use in def
       (let [[name type default] t
             [name E] (Alhs fields name E)]
         [[name type default] E])
       ($syntax-error)))
   :else ($syntax-error)))

(defn Aargs [[args star-arg more-args kw-arg] E]
  (let [[args E] (vec (A* () args E))
        [star-arg E] (A star-arg E)
        [more-args E] (vec (A* () more-args E))
        [kw-arg E] (A kw-arg E)]
    [[args star-arg more-args kw-arg] E]))

(defn A
  ([x] (A x null-syntax-analysis-state))
  ([x E]
     (let [i (meta x)
           w (fn [[x E]] [(with-meta x i) E])]
       (cond
        ;; (:syntax-analysis i) x ;; already computed! (useful for macros, if they don't blindly copy it)
        (seq? x)
        (let [[h & t] x]
          (case h
            (:identity :Expression :Interactive :Module :progn
             :and :or :add :sub :mul :div :floordiv :mod :lshift :rshift
             :and_ :or_ :xor :pow :not :pos :neg :invert
             :lt :gt :eq :ge :le :ne :in :is :not-in :is_not ;; magic
             :assert :pass :return :break :continue :raise
             :dict :list :set :tuple :star
             :imaginary :comparison :subscript :if :while :comp-if)
            (w (A* (list h) t E))
            (:call)
            (let [[f & args] t
                  [f E] (A f E)
                  [args E] (Aargs args E)]
              (w [(list* h f args) E]))
            (:decorator)
            (let [[name & args] t
                  [args E] (Aargs args E)]
              (w [(list* h name args) E]))
            (:with)
            (let [[clauses body] t
                  [clauses E] (thread-args
                               () (fn [[expr target] E]
                                    (let [[expr E] (A expr E)
                                          [target E] (Alhs [:assigned] target E)]
                                      [[expr target] E])) clauses E)
                  [body E] (A body E)]
              (w [(list h clauses body) E]))
            (:keyarg)
            (let [[name & value] t
                  [value E] (A value E)]
              (w [(list* h name value) E]))
            (:def)
            (let [[name [args star-arg more-args kw-arg] rettype body decorators] t
                  [decorators E] (A* () decorators E)
                  [name E] (Alhs [:assigned] name E)
                  ;; Process arguments once in environment E for their type and default
                  [args E] (vec (A* () args E))
                  [star-arg E] (A star-arg E)
                  [more-args E] (vec (A* () more-args E))
                  [kw-arg E] (A kw-arg E)
                  [rettype E] (A rettype E)
                  ;; Process arguments once as lhs in inner environment EE for their name
                  [args EE] (vec (Alhs* () [:assigned] args null-syntax-analysis-state))
                  [star-arg EE] (Alhs [:assigned] star-arg EE)
                  [more-args EE] (vec (Alhs* () [:assigned] more-args EE))
                  [kw-arg EE] (Alhs [:assigned] kw-arg EE)
                  [body EE] (A body EE)]
              (w [(list h name [args star-arg more-args kw-arg] rettype body decorators EE) E]))
            (:argument) ;; for use in def
            (let [[name type default] t]
              (w (A* (list h name) [type default] E)))
            (:class)
            (let [[name superclasses body decorators] t
                  [decorators E] (A* () decorators E)
                  [name E] (Alhs [:assigned] name E)
                  [superclasses E] (A* () superclasses E)
                  [body EE] (A body null-syntax-analysis-state)]
              (w [(list h name superclasses body decorators EE) E]))
            (:lambda)
            (let [[[args star-arg more-args kw-arg] body] t
                  [args EE] (vec (Alhs* () [:assigned] args null-syntax-analysis-state))
                  [star-arg EE] (Alhs [:assigned] star-arg EE)
                  [more-args EE] (vec (Alhs* () [:assigned] more-args EE))
                  [kw-arg EE] (Alhs [:assigned] kw-arg EE)
                  [body EE] (A body EE)]
              (w [(list h name [args star-arg more-args kw-arg] body EE) E]))
            (:yield :yield-from)
            (w (A* (list h) t (assoc E :yield true)))
            (:nonlocal :global)
            (let [other (E ({:nonlocal :global :global :nonlocal} h))]
              (doseq [x t]
                (when (other x) ($syntax-error))
                (when ((E :referenced) x) ($syntax-error))) ;; TODO: make it a warning only
              [x (update-in E [h] #(apply conj % t))])
            (:del) ;; maybe we need a special category for :del ?
            (Alhs [:assigned] x E)
            (:import :from)
            [x E] ;; untouched in this pass
            (:assign-expr)
            (let [[ls r] t
                  [rhs E] (A r E)
                  [lhs E] (thread-args () #(Alhs [:assigned] % %2) ls E)]
              (w [(list h (vec lhs) rhs) E]))
            (:iadd :isub :imul :idiv :ifloordiv :imod :iand :ior :ixor
                   :irshift :ilshift :ipow :imatmul)
            (let [[l r] t
                  [rhs E] (A r E)
                  [lhs E] (Alhs [:referenced :assigned] l E)]
              (w [(list h lhs rhs) E]))
            (:select)
            (let [[y n] x
                  [z E] (A y E)]
              (w [(list h z n) E]))
            (:for)
            (let [[lhs rhs body else] t
                  [lhs E] (Alhs [:assigned] lhs E)]
              (w (A* (list h lhs) [rhs body else] E)))
            (:comprehension)
            (let [[kind expr comp] t]
              (w (A* (list h kind) [expr comp] E)))
            (:comp-for)
            (let [[lhs rhs comp] t
                  [lhs E] (Alhs [:assigned] lhs E)]
              (w (A* (list h lhs) [comp] E)))
            (:try)
            (let [[body excepts else finally] t
                  [body E] (A body E)
                  [excepts E] (A* () excepts E)]
              (w (A* (list h body excepts) [else finally] E)))
            (:except)
            (let [[exc var body] t
                  [exc E] (A exc E)
                  [var E] (Alhs [:assigned] var E)]
              (w (A* (list h exc var) [body] E)))
            ($syntax-error)))
        (symbol? x)
        [x (update-in E [:referenced] #(conj % x))]
        (or (literal? x) (#{:False :True :None :zero-uple :empty-list :empty-dict} x) (nil? x))
        [x E]
        :else ($syntax-error x)))))

(defn analyze-syntax [x] ((A x) null-syntax-analysis-state))
