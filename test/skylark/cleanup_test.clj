(ns skylark.cleanup-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [skylark.core-test]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.cleanup])
  (:require [skylark.core :as sky]))


(deftest cleanup-test
  (testing "Some cleanups"
    (is (= (first (cleanup "")) [:module]))))
