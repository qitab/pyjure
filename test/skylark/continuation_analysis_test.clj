(ns skylark.continuation-analysis-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [skylark.debug :exclude [analyze-continuations]]
        [skylark.core-test]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.continuation-analysis])
  (:require [skylark.core :as sky]))

(deftest continuation-analysis-test
  (comment
  (testing "continuation-analysis only touches meta-data"
    (let [x (sky/cleanup python-test)
          [y E] (analyze-continuations x)]
      (is (= x y))))))
