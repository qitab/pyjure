(ns skylark.eval
  (:use [clojure.core.match :only [match]]
        [skylark.debug :exclude [evaluate]]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.runtime]))

(defn evaluate [x]
  (binding [*ns* (find-ns (or (:namespace (meta x)) 'skylark.user))]
    (eval x)))
