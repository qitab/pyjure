(ns pyjure.continuation-analysis-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [pyjure.debug :exclude [analyze-continuations]]
        [pyjure.core-test]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.continuation-analysis])
  (:require [pyjure.core :as sky]))

(deftest continuation-analysis-test
  (comment
  (testing "continuation-analysis only touches meta-data"
    (let [x (sky/cleanup python-test)
          [y E] (analyze-continuations x)]
      (is (= x y))))))
