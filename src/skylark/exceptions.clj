(ns skylark.exceptions
  (:use [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.runtime]
        [skylark.exceptions]))

(defn no-$-symbol [s]
  (let [s (str s)]
    (assert (= (first s) \$))

(defmacro define-exception [exname bases args fmt]
  (let [pyname (subs (name exname) 1)]
  `(do
     ;; TODO:
     ;; * pick a keyword
     ;; * pick a symbol
     ;; * defn the symbol as $error'ing with the keyword as tag
     ;; * register the keyword to the py mop, with a __dict__ that integrates it in the pyjure object system
     ~(comment `(defpyclass ~name ~supers
                  (defpyfield format ~fmt)
                  (defpyinit ~name ~args)))
     (def-py-type ~exname ~(or bases $Exception))
     (def (
