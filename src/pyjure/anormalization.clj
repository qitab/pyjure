(ns pyjure.anormalization
  (:use [clojure.core.match :only [match]]
        [pyjure.debug :exclude [anormalization]]
        [pyjure.utilities]))

(defn &A [x]
  (fn [] x))
(defn anormalize [x]
  ((&A x) []))
