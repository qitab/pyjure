(ns pyjure.eval
  (:use [clojure.core.match :only [match]]
        [pyjure.debug :exclude [evaluate]]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.runtime]))

(defn evaluate [x]
  (binding [*ns* (find-ns (or (:namespace (meta x)) 'pyjure.user))]
    (eval x)))
