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
  (testing "desugar smoketest"
    (= (py/desugar "a ; b+1 ; c")
       [:module [:suite [:id "a"] [:builtin :add [:id "b"] [:constant [:integer 1N]]] [:id "c"]]])
    (= (py/desugar "f(a, b)")
       [:module [:call [:id "f"] [[[:id "a"] [:id "b"]] nil [] nil]]]))
  (comment ;; Reenable when we're done...
  (testing "Every python desugaring rule"
    (match [(tryf #(py/desugar python-test))] [[':module & _]]
           (is (= 1 1))))))
