(ns skylark.semantics
  (:require [clojure.core :as c])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [skylark.parsing :as parsing])
  (:require [skylark.parser :as p])
  (:require [skylark.lexer :as l])
  (:require [skylark.sexpify :as s])
  (:require [skylark.runtime :as r])
  (:use [clojure.core.match :only [match]]))


;;; We map Python list to Clojure vector for fast-ish append,
;;; Python tuple to Clojure list,
;;; Python dict to Clojure map, Python set to Clojure set.

(defn NIY [& args] (throw (Throwable. "Not Implemented Yet")))

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
  (or (integer? x) (float? x) (string? x) (bytes? x)))

(defn C [x E]
  (match [x]
    [x :guard literal?] [x E] ;; :integer :float :string :bytes
    [:True] [true E]
    [:False] [false E]
    [:None] [nil E]
    [:zero-uple] [() E]
    [:empty-list] [[] E]
    [:empty-dict] [{} E]
    [x :guard symbol?]
    (let [[getter :as found?] (E x)]
      (if found? [getter E]
          ;; Not found? Introduce a local binding...
          (NIY)))
    [((:or :identity :Expression :Interactive) x)] (C x E)
    [((:or :Module :progn) & xs)]
    (let [[o E] (reduce (fn [[os E] x] (let [[on En] (C x E)] [(conj os on) En])) ['(do) E] xs)]
      [(reverse o) E])
    [((:or ':and ':or :as op) & xs)]
    (NIY)
    _ (throw (Throwable. "Invalid form"))))

(comment
       (:lt :gt :eq :ge :le :ne :in :is :not-in :is_not) tag
       (:and_ :assert :comp-for :comp-if :del :except :for :list :global :nonlocal
              :or_ :pow :progn :raise :select :set :slice :tuple :while :xor)
       (w (cons tag (X* x)))
       (:dict) (w (cons tag (Xvec* x)))
       (:return :not :pos :neg :invert :star :identity) (w (list tag (X x)))
       (:break :continue :pass) (w (list tag))
       (:import) (cons :import (map (fn [[names name]] [(X* names) (X name)]) x))
       :def
       (let [[name args return-type body decorators] x]
         (w (list :def (X name) (def-args args) (X return-type) (X body) (X* decorators))))
       :decorator
       (let [[name args] x]
         (w (list :decorator (X* name) (when args (Xarglist args)))))
       :class
       (let [[name superclasses body decorators] x]
         (w (list :class (X name) (X* superclasses) (X body) (X* decorators))))
       :call
       (let [[fun args] x]
         (w (list :call (X fun) (Xarglist args))))
       :subscript
       (let [[arg indices] x]
         (w (list :subscript (X arg) (X* indices))))
       :imaginary
       (w (list tag (X (conj x info))))
       :if
       (let [[clauses else] x]
         (w (list :if (Xvec* clauses) (X else))))
       :assign
       (let [[a b] x]
         (w (list '= (X a) (X b))))
       :assign-expr
       (let [v (Xvec x)]
         (w (list 'assign (subvec v 0 (dec (count v))) (last v))))
       :augassign-expr
       ;; :iadd :isub :imul :imul :ifloordiv :imod :iand :ior :ixor :irshift :ilshift :ipow :imatmul
       (let [[a op b] x] (w (list (first op) (X a) (X b))))
       :comparison
       (let [[a ops] x] (w (list* :comparison (X a) (map Xvec ops))))
       (:arith-expr :shift-expr :term)
       ;; :add :sub :mul :div :floordiv :mod :lshift :rshift
       (let [[[_ _ info :as a0] ops] x]
         (first (reduce (fn [[a info] [op b]]
                          (let [i (parsing/merge-info info (b 2))]
                            [(with-meta (list (first op) a (X b)) {:source-info i}) i]))
                        [(X a0) info] ops)))
       :comprehension
       (let [[kind a for] x] (w (list :comprehension kind (X a) (X for))))
       :from
       (let [[[dots names] imports] x]
         (list :from [dots (X* names)]
               (if (= (first imports) :mul) :all
           (Xvec* imports))))
       :try
       (let [[body [excepts else finally]] x]
         (w (list :try (X body) (Xvec* excepts) (X else) (X finally))))
       :with
       (let [[items body] x]
         (w (list :with (vec (map (fn [[x y]] [(X x) (X y)]) items)) (X body))))
       :yield
       (w (if (= (first x) :subiterator)
            (list :yield-from (X (second x)))
            (list :yield (X x)))))

(defn Cp* [x] (C (s/Xp x) initial-environment))
(defn Cp [x] (first (Cp* x)))


