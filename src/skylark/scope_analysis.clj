(ns skylark.scope-analysis
  (:use [clojure.core.match :only [match]]
        ;;[skylark.parsing] ;; ACTUALLY, State Monad, not just parsing!
        [skylark.utilities]
        [skylark.runtime :only [literal? $syntax-error]]))

;; Annotate each scoping level with
;; 1- which identifiers it either: (1) marks as global, (2) marks as nonlocal, (3) introduces.
;; 2- whether it yields

(defrecord Environment [global nonlocal assigned referenced yield])
(def null-env (->Environment #{} #{} #{} #{} false))
;;(defmethod fail-message Environment "scoping error")

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
       (:select) ;; not pure
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
  ([x] (A x null-env))
  ([x E]
     (DBG :A x E)
     (let [i (meta x)
           w (fn [[x E]] [(with-meta x i) E])]
       (cond
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
                  [args EE] (vec (Alhs* () [:assigned] args null-env))
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
                  [body EE] (A body null-env)]
              (w [(list h name superclasses body decorators EE) E]))
            (:lambda)
            (let [[[args star-arg more-args kw-arg] body] t
                  [args EE] (vec (Alhs* () [:assigned] args null-env))
                  [star-arg EE] (Alhs [:assigned] star-arg EE)
                  [more-args EE] (vec (Alhs* () [:assigned] more-args EE))
                  [kw-arg EE] (Alhs [:assigned] kw-arg EE)
                  [body EE] (A body EE)]
              (w [(list h name [args star-arg more-args kw-arg] body EE) E]))
            (:yield :yield-from)
            (w (A* (list h) t (assoc E :yield true)))
            (:nonlocal :global)
            [x (update-in E [h] #(apply conj % t))]
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
