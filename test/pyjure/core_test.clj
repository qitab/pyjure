(ns pyjure.core-test
  (:require [me.raynes.conch :only [programs with-programs let-programs] :as sh])
  (:use [pyjure.debug]
        [pyjure.passes]
        [pyjure.core]))

(def foo (clojure.java.io/resource "pyjure/foo.py"))
(def python-test (clojure.java.io/resource "pyjure/python_test.py"))
