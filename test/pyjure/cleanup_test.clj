(ns pyjure.cleanup-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [pyjure.core-test]
        [pyjure.debug]
        [pyjure.passes]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.cleanup]))

(deftest cleanup-test
  (testing "Some cleanups"
    (is (= (cleanup "") [:module nil]))))
