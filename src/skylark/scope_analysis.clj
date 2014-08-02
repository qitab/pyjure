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

(declare A)

(defn A* [head xs E] (thread-args (list head) A xs E))

;;(defn copy-meta [x y] (with-meta x (meta y)))

(defn update-in-multiple [record fields f & args]
  (reduce #(apply update-in % [%2] f args) record fields))

(defn Alhs [fields x E]
  (<- (if (symbol? x)
        [x (update-in-multiple E fields conj x)])
      (if (seq? x)
        (let [[h & t] x]
          (case h
            (:tuple :list :del)
            (thread-args (list h) #(Alhs fields % %2) t E)
            (:subscript)
            (NIY)
            (NIY))))
      (NIY)))

(defn A
  ([x] (A x null-env))
  ([x E]
     (let [i (meta x)
           w (fn [[x E]] [(with-meta x i) E])]
       (<- (if (seq? x)
             (let [[h & t] x]
               (case h
                 (:identity :Expression :Interactive :Module :progn
                            :and :or
                            :add :sub :mul :div :floordiv :mod :lshift :rshift
                            :and_ :or_ :xor :pow
                            :not :pos :neg :invert
                            :lt :gt :eq :ge :le :ne :in :is :not-in :is_not ;; magic
                            :assert :pass
                            :dict :list :set :tuple
                            :return :star
                            :break :continue :raise
                            :imaginary
                            :comparison
                            :subscript
                            :if :while)
                 (w (A* h t E))
                 (:yield :yield-from)
                 (w (A* h t (assoc E :yield true)))
                 (:nonlocal :global)
                 [x (update-in E [h] #(apply conj % t))]
                 (:del) ;; maybe we need a special category for :del ?
                 (Alhs [:assigned] x E)
                 (:import :from)
                 (w (NIY))
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
                 (NIY))))
           (if (symbol? x)
             [x (update-in E [:referenced] #(conj % x))])
           (if (or (literal? x) (#{:False :True :None :zero-uple :empty-list :empty-dict} x))
             [x E])
           ($syntax-error x)))))

(comment
  :comprehension :comp-for :comp-if
  :try :except
  :for
  :decorator
  :def
  :class
  :call
  =
  :with)
