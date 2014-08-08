(ns skylark.clojurifier
  (:use [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.runtime]))

;; We map Python list to Clojure vector for fast-ish append,
;; Python tuple to Clojure list,
;; Python dict to Clojure map, Python set to Clojure set.

;; Just like macropy, we have three kinds of macros:
;; expr-macro, block-macro and decorator-macro.
;; However, we pass our kind of ASTs, not theirs.

;; TODO: transform every branch point into binding-passing style?

(declare C create-binding)

(defrecord Environment [level local outer global])
;; level: 0 for global, incremented when you descend into scope
;; local: map of names to getters, or nil if level is 0
;; outer: the next outer non-global environment, or nil if only global is next
;; global: the global environment
;; yield?: has yield appeared at this scope level? if yes, we'll have to transform the def.


(def null-env (->Environment {} {} {} {}))

(defn symbol-getter [class name] (NFN))


(declare C Cassign)

(defn C* [head xs E] (thread-args head C xs E))

(defn C
  ([x] (C x null-env))
  ([x E]
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
       :else ($syntax-error))))

(comment
  "
def foo(a):
  x=1
  if a: x=2
  return x
"
  (defn foo [a] (let [x 1] (-> ($if a [2] [x]) (fn [[x]] x))))

  "
def foo(a):
  x=1;
  if a:
    return x
  elif bar(a):
    y = 3
  else:
    y = 4
    z = 5
  if y == 4:
    return z
"
  (defn foo [a]
    (let [x 1]
      ((fn [k] ($cond a x
                      ($call bar a) (k 3 nil false)
                      :else (k 4 5 true)))
       ;; no need to pass a witness for y: it is always bound in all branches that reach k
       ;; on the other hand, our analysis doesn't prove that z is unbound, so we check
       (fn [y z z?] ((fn [k] ($cond (= y 4) ($check-bound z? z) ;
                                    :else (k)))
                (fn [] (return $None))))))))
;; Further passes may show that some functions are used only once, and thus may be inlined,

