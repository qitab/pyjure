(ns pyjure.core-test
  (:require [me.raynes.conch :only [programs with-programs let-programs] :as sh])
  (:use [clojure.tools.trace]
        [clojure.tools.nrepl]
        [clojure.repl]
        [clojure.test]
        [pyjure.core]))

(def foo (clojure.java.io/resource "pyjure/foo.py"))
(def python-test (clojure.java.io/resource "pyjure/python_test.py"))
