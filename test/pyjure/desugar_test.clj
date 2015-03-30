(ns pyjure.desugar-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [pyjure.core-test]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.desugar])
  (:require [pyjure.core :as py]))

(defn test-desugar [input] (tryf #(py/desugar input)))

(deftest desugar-test
  ;; (testing "desugar smoketest")
  (testing "Every python desugaring rule"
    (match [(tryf #(py/desugar python-test))] [[':module & _]]
           (is (= 1 1)))))
