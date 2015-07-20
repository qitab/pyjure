(ns pyjure.clarification-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [pyjure.core-test]
        [pyjure.debug]
        [pyjure.passes]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.clarification]))

(deftest clarification-test
  (testing "Every python function scope clarification"
    (let [x (desugar python-test)
          y (clarify- x)]
      (is (= x y)))))
