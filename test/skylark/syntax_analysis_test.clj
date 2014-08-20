(ns skylark.syntax-analysis-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [skylark.core-test]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.syntax-analysis])
  (:require [skylark.core :as sky]))

(deftest syntax-analysis-test
  ;; (testing "desugar smoketest")
  (testing "Every python desugaring rule"
    (let [x (sky/desugar python-test)
          y (analyze-syntax x)]
      (is (= x y)))))
