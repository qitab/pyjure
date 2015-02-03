(ns pyjure.cleanup-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [pyjure.core-test]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.cleanup])
  (:require [pyjure.core :as sky]))


(deftest cleanup-test
  (testing "Some cleanups"
    (is (= (sky/cleanup "") [:module [:suite]]))))
