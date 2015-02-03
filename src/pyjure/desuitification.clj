(ns pyjure.desuitification
  (:use [clojure.core.match :only [match]]
        [pyjure.debug :exclude [desuitification]]
        [pyjure.utilities]))

;; This pass takes every expression of form [:operator [:suite ...]]
;; and transforms them into a [:suite ... [operator ...]]
;; Can be merged with A-normalization?

(defn desuitify [x]
  ;;(first ((&D x) ())))
  x)
