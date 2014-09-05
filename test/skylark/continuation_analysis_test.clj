(ns skylark.continuation-analysis-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [skylark.core-test]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.continuation-analysis])
  (:require [skylark.core :as sky]))

(deftest clarification-test
  (testing "Every python function scope clarification"
    (let [x (sky/desugar python-test)
          [y E] (clarify x)]
      (is (= x y)))))
