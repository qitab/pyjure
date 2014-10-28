(ns skylark.desugar-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [skylark.core-test]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.desugar])
  (:require [skylark.core :as sky]))

(defn test-desugar [input] (tryf #(sky/desugar input)))

(deftest desugar-test
  ;; (testing "desugar smoketest")
  (testing "Every python desugaring rule"
    (match [(tryf #(sky/desugar python-test))] [[':module & _]]
           (is (= 1 1)))))
