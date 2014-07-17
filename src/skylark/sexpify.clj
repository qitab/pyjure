(ns skylark.sexpify
  (:use [clojure.algo.monads])
  (:use [clojure.core.match :only [match]])
  (:require [skylark.parser :as p])
  (:require [skylark.lexer :as l])
  (:require [skylark.semantics :as s])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn X [form]
  (match form
   nil nil
   [tag x info]
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
           (deco [decorators]
             (map (fn [[name args i]]
                    (w (list :decorator name (X* args)) i)) decorators))
           ]
     (case tag
       (:integer :float :string :bytes) x
       (:id) (w (symbol x))
       (:True :False :None :ellipsis
        :zero-uple :empty-list :empty-dict
        :lt :gt :eq :ge :le :ne :in :is :not-in :is_not) tag
       (:Expression :Interactive) (X x)
       (:Module :and :and_ :assert :comp-for :comp-if :del :except :for :if :list :global :non-local
                :or :or_ :pow :progn :raise :select :set :slice :tuple :while :xor)
       (w (cons tag (X* x)))
       (:dict) (w (cons tag (Xvec* x)))
       (:return :not :pos :neg :invert :star) (w (list tag (X x)))
       (:break :continue :pass) (w (list tag))
       (:import) (cons :import x)
       :def
       (let [[name args return-type body decorators] x]
         (w (list :def (X name) (def-args args) (X return-type) (X body) (deco decorators))))
       :class
       (let [[name superclasses body decorators] x]
         (w (list :class (X name) (X* superclasses) (X body) (deco decorators))))
       :call
       (let [[fun [args rarg margs kargs]] x]
         (w (list :call (X fun) (Xvec args) (X rarg) (Xvec margs) (X kargs))))
       :subscript
       (let [[arg indices] x]
         (w (list :subscript (X arg) (X* indices))))
       :imaginary
       (w (list tag (X (conj x info))))
       :assign
       (let [[a b] x]
         (w (list '= (X a) (X b))))
       :assign-expr
       (let [v (Xvec x)]
         (w (list 'assign (subvec v 0 (dec (count v))) (last v))))
       :augassign-expr
       (let [[a op b] x] (w (op a b)))
       :comparison
       (let [[a ops] x] (w (list* :comparison (X a) (map Xvec ops))))
       (:arith-expr :shift-expr :term)
       (let [[[_ _ info :as a0] ops] x]
         (first (reduce (fn [[a info] [op b]]
                          (let [i (p/merge-info info (b 2))]
                            [(with-meta (list (first op) a (X b)) {:source-info i}) i]))
                        [(X a0) info] ops)))
       :comprehension
       (let [[kind a for] x] (w (list :comprehension kind (X a) (X for))))
       :from
       (let [[[dots name] imports] x]
         (list :from [dots (X name)]
               (if (= imports :mul) :all
                   (Xvec* imports))))
       :try
       (let [[body [excepts else finally]] x]
         (w (list :try (X body) (Xvec* excepts) (X else) (X finally))))
       :with
       (let [[items body] x]
         (w (list :with (vec (map (fn [[x y]] [(X x) (X y)]) items)) body)))
       :yield
       (w (if (= (first x) :subiterator)
            (list :yield-from (X (second x)))
            (list :yield (X x))))))))

(defn p [s] (p/python-parser s))
(defn Xp [s] (X (p/python-parser s)))

(comment
  (set! *file* nil)
  (def x0 (p/python-parser "def foo(x): return x*6\nprint(foo(7))\n"))
  (X x0)
)
