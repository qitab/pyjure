(ns skylark.desugar-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.desugar])
  (:require [skylark.core :as sky]))


(def all-python (-> "skylark/python_test.py" clojure.java.io/resource))
(defn d [file] (-> file slurp sky/desugar))
(defn foo [] (-> "skylark/foo.py" clojure.java.io/resource d))
(defn test-desugar [input] (tryf #(sky/desugar input)))

(deftest desugar-test
  ;; (testing "desugar smoketest")
  (testing "Every python desugaring rule"
    (is (= (first (d all-python)) :suite))))

