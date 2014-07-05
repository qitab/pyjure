(ns skylark.semantics
  (refer-clojure
   ;; These are special forms: def throw & if
   :exclude [;; Python keywords
             and as assert break class continue
             def del elif else except exec
             finally for from global if import in is
             lambda not or pass print raise return try
             while with yield])
  (require [clojure.core :as c]))

(defn NIY [& args] (throw (Throwable. "Not Implemented Yet")))

(defn decorator-macro [name]
  ;; (NIY)
  nil)

(defn decorate [definition decorator]
  ;;; implement decorators...
  (let [[name args] (if (symbol? decorator) [decorator nil] decorator)
        macro (decorator-macro name)]
    (if macro
      (macro args definition)
      (let [[_ defname] definition]
        `(do ~definition
             (set! ~defname (~decorator ~defname)))))))

(defmacro decorated [decorators definition]
  (reduce decorate definition decorators))

