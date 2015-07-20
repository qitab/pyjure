(ns pyjure.continuation-analysis-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [pyjure.debug]
        [pyjure.core-test]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.continuation-analysis]))

(deftest continuation-analysis-test
  (comment
  (testing "continuation-analysis only touches meta-data"
    (let [x (cleanup python-test)
          [y E] (analyze-continuations x)]
      (is (= x y))))))
