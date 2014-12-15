(ns pyjure.clarification-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [pyjure.core-test]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.clarification])
  (:require [pyjure.core :as sky]))

(deftest clarification-test
  (testing "Every python function scope clarification"
    (let [x (sky/desugar python-test)
          y (clarify x)]
      (is (= x y)))))
