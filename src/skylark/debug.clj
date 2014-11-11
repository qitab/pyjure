(ns skylark.debug
  (:use [clojure.pprint :only [cl-format]]
        [skylark.utilities])
  (:require [clojure.repl]
            [clojure.tools.trace]))

;; debugging utilities while developing skylark

(reexport clojure.repl apropos pst)
(reexport-macro clojure.repl doc)
(reexport-macro clojure.tools.trace trace untrace)

(reexport-deferred skylark.core
  skylark to-reader position-stream lex parse desugar clarify cleanup analyze-continuations clojurify)

(defn tracing [name f]
  (fn [& args]
    (cl-format true "~s <= ~{~s~^ ~}~%" name args)
    (let [result (apply f args)]
      (cl-format true "~s => ~s~%" name result)
      result)))
