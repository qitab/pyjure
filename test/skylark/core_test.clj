(ns skylark.core-test
  (:require [me.raynes.conch :only [programs with-programs let-programs] :as sh])
  (:use [clojure.tools.trace]
        [clojure.tools.nrepl]
        [clojure.repl]
        [clojure.test]
        [skylark.core]))

(def foo (clojure.java.io/resource "skylark/foo.py"))
(def python-test (clojure.java.io/resource "skylark/python_test.py"))
