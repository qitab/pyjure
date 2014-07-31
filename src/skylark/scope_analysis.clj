(ns skylark.scope-analysis
  (:use [clojure.core.match :only [match]]
        [skylark.utilities]))

;; Annotate each scoping level with
;; 1- which identifiers it either: (1) marks as global, (2) marks as nonlocal, (3) introduces.
;; 2- whether it yields

;; We map Python list to Clojure vector for fast-ish append,
;; Python tuple to Clojure list,
;; Python dict to Clojure map, Python set to Clojure set.

;; Just like macropy, we have three kinds of macros:
;; expr-macro, block-macro and decorator-macro.
;; However, we pass our kind of ASTs, not theirs.

(defrecord Environment [global nonlocal local yield])
;; level: 0 for global, incremented when you descend into scope
;; local: map of names to getters
;; outer: the next outer environment, or nil if already global
;; global: the global environment
;; yield?: has yield appeared at this scope level? if yes, we'll have to transform the def.

(def null-env (->Environment #{} #{} #{} false))

(defn A
  ([x] (A x null-env))
  ([x E] (NIY)))

(comment

     (match [x]
       [([h & _] :seq)]
    (<- (if (symbol? h)
          (if-let [[getter found?] (symbol-getter h E)]
            [getter E]
            (let [newE (create-binding h E)]
              [(first (symbol-getter h newE)) newE])))
        (if (#{:identity :Expression :Interactive} h) (C x E))
        (if-let [v ({:Module 'do :progn 'do} h)] (C* v (rest x) E))
        (if (#{:and :or
               :add :sub :mul :div :floordiv :mod :lshift :rshift
               :and_ :or_ :xor :pow
               :not :pos :neg :invert
               :lt :gt :eq :ge :le :ne :in :is :not-in :is_not ;; magic
               :assert :pass} h) (C* (runtime-symbol h) (rest x) E))
        ($syntax-error))
    [x :guard literal?] [x E] ;; :integer :float :string :bytes
    [:True] [$True E]
    [:False] [$False E]
    [:None] [$None E]
    [:zero-uple] [$empty-tuple E]
    [:empty-list] [$empty-list E]
    [:empty-dict] [$empty-dict E]
    :else ($syntax-error))

  :dict
  :return :star :identity
  :comp-for :comp-if :del :except :for :list :global :nonlocal
  :raise :select :set :slice :tuple :while
  :break :continue
  :import
  :def
  :decorator
  :class
  :call
  :subscript
  :imaginary
  :if
  =
  :assign-expr
  :augassign-expr ;; :iadd :isub :imul :imul :ifloordiv :imod :iand :ior :ixor :irshift :ilshift :ipow :imatmul
  :comparison
  :comprehension
  :from
  :try
  :with
  :yield-from :yield)
