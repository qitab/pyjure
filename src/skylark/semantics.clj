(ns skylark.semantics
  (:require [clojure.core :as c])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [skylark.parsing :as parsing])
  (:require [skylark.parser :as p])
  (:require [skylark.lexer :as l])
  (:require [skylark.sexpify :as s])
  (:use [clojure.core.match :only [match]])
  (:use [skylark.utilities])
  (:use [skylark.runtime]))

;; TODO: rename away this file after it's been determined, since this is just one pass,
;; will all the passes contributing to the semantics.
;; Name the pass after something meaningful, if possible.

(defn symbol-getter [class name] (NFN))

;;; We map Python list to Clojure vector for fast-ish append,
;;; Python tuple to Clojure list,
;;; Python dict to Clojure map, Python set to Clojure set.

;;; Just like macropy, we have three kinds of macros:
;;; expr-macro, block-macro and decorator-macro.
;;; However, we pass our kind of ASTs, not theirs.

(defn expr-macro [name]
  ;; (NIY)
  nil)

(defn block-macro [name]
  ;; (NIY)
  nil)

(defn decorator-macro [name]
  ;; (NIY)
  nil)


;;; Decorators

(defn decorate [definition decorator]
  ;;; implement decorators...
  (let [[name args] (if (symbol? decorator) [decorator nil] decorator)
        macro (decorator-macro name)]
    (if macro
      (macro args definition)
      (let [[_ defname] definition]
        `(do ~definition
             (set! ~defname (~decorator ~defname)))))))

(defmacro decorated [decorators definition]
  (reduce decorate definition decorators))

(comment
   (letfn [(w ([x] (w x info)) ([x i] (with-meta x {:source-info i})))
           (X* [s] (when s (map X s)))
           (Xvec [s] (vec (X* s)))
           (Xvec* [s] (map Xvec s))
           (def-arg [[[name type] default]]
             (when-not (nil? name)
               (list 'argument (X name) (X type) (X default))))
           (def-xarg [[name type]]
             (when-not (nil? name)
               (list 'argument (X name) (X type))))
           (def-args [[positional-args rest-arg more-args kw-arg]]
             [(vec (map def-arg positional-args))
              (def-xarg rest-arg)
              (vec (map def-arg more-args))
              (def-xarg kw-arg)])
           (Xarglist [[args rarg margs kargs]]
             [(Xvec args) (X rarg) (Xvec margs) (X kargs)])]))

(defn literal? [x]
  (or (integer? x) (float? x) (string? x) (byte-array? x)))

(declare C create-binding)

(defn C* [head xs E]
  (let [[o E] (reduce (fn [[os E] x] (let [[on En] (C x E)] [(conj os on) En])) [(list head) E] xs)]
       [(reverse o) E]))

(defn $syntax-error []
  (throw (Throwable. "Syntax Error")))

(defn runtime-symbol [x]
  (symbol 'skylark.runtime (str \$ x)))

(defn C [x E]
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
    :else ($syntax-error)))

(comment
  :comp-for :comp-if :del :except :for :list :global :nonlocal
  :raise :select :set :slice :tuple :while
  :dict
  :return :star :identity
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

(defn Cp* [x] (C (s/Xp x) initial-environment))
(defn Cp [x] (first (Cp* x)))

