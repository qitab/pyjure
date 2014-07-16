(ns skylark.sexpify
  (:use [clojure.algo.monads])
  (:use [clojure.core.match :only [match]])
  (:require [skylark.parser :as p])
  (:require [skylark.lexer :as l])
  (:require [skylark.semantics :as s])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

(defn X [form]
  (let [[tag x info] form]
    (letfn [[w [x] (with-meta x {:source-info info})]]
      (case tag
        :id (w (symbol x))
        :integer x
        'Module (w (cons 'Module (map X x)))
        (w (list tag (X x)))))))

(defn Xp [s] (X (p/python-parser s)))

(comment
  (set! *file* nil)
  (def x0 (p/python-parser "def foo(x): return x*6\nprint(foo(7))\n"))
  (X x0)
)
