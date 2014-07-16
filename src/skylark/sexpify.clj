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
   (letfn [(w [x] (with-meta x {:source-info info}))
           (def-arg [[[name type] default]]
             (list 'argument (X name) (X type) (X default)))
           (def-xarg [[name type]]
             (list 'argument (X name) (X type)))
           (def-args [[positional-args rest-arg more-args kw-arg]]
             [(map def-arg positional-args)
              (def-xarg rest-arg)
              (map def-arg more-args)
              (def-xarg kw-arg)])]
     (case tag
       :id (w (symbol x))
       :integer x
       Module (w (cons 'Module (map X x)))
       def (let [[name args return-type body] x]
             (w (list* 'def (X name) (def-args args) (X return-type) body)))
       call (let [[fun [args rarg margs kargs]] x]
              (list 'call (X fun) (map X args) (X rarg) (map X margs) (X kargs)))
       assign (let [[a b] x] (list '= (X a) (X b)))
       (w (list tag (X x)))))))

(defn p [s] (p/python-parser s))
(defn Xp [s] (X (p/python-parser s)))

(comment
  (set! *file* nil)
  (def x0 (p/python-parser "def foo(x): return x*6\nprint(foo(7))\n"))
  (X x0)
)
