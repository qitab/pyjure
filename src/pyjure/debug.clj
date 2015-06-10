(ns pyjure.debug
  (:use [clojure.pprint :only [cl-format]]
        [pyjure.utilities])
  (:require [clojure.repl]
            [clojure.tools.trace]))

;; debugging utilities while developing pyjure

(reexport clojure.repl apropos pst)
(reexport-macro clojure.repl doc)
(reexport-macro clojure.tools.trace trace untrace trace-ns untrace-ns)

(reexport-deferred pyjure.core
  pyjure to-reader position-stream lex parse desugar clarify cleanup analyze-continuations clojurify)

(defn tracing [name f]
  (fn [& args]
    (cl-format true "~s <= ~{~s~^ ~}~%" name args)
    (let [result (apply f args)]
      (cl-format true "~s => ~s~%" name result)
      result)))
