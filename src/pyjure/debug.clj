;; Debugging tools used while developing pyjure
(ns pyjure.debug
  (:use
   [clojure.pprint :only [cl-format]])
  (:require
   [clojure.repl]
   [clojure.tools.trace]))

;;; Reexporting things from another namespace
(defmacro reexport [ns prefix suffix & xs]
  `(do ~@(map #(do `(def ~(symbol (str prefix % suffix)) ~(symbol (str ns) (str %)))) xs)))
(defmacro reexport-macro [ns prefix suffix & xs]
  `(do ~@(map #(do `(defmacro ~(symbol (str prefix % suffix)) [& a#] `(~'~(symbol (str ns) (str %)) ~@a#))) xs)))
(defmacro reexport-deferred [ns prefix suffix & xs]
  `(do ~@(map #(do `(defn ~(symbol (str prefix % suffix)) [& a#] (apply (find-var (symbol ~(str ns) ~(str %))) a#))) xs)))
(defmacro reexport-macro-deferred [ns prefix suffix & xs]
  `(do ~@(map #(do `(defmacro ~(symbol (str prefix % suffix)) [& a#] `(~(symbol ~(str ns) ~(str %)) ~@a#))) xs)))

;;; Reexport the usual helpers
(reexport clojure.repl "%" "" apropos pst)
(reexport-macro clojure.repl "%" "" doc)
(reexport-macro clojure.tools.trace "%" "" trace untrace trace-ns untrace-ns)

;;; More tracing
(defn tracing [name f]
  (fn [& args]
    (cl-format true "~s <= ~{~s~^ ~}~%" name args)
    (let [result (apply f args)]
      (cl-format true "~s => ~s~%" name result)
      result)))

;;; Non-intrusive print-debugging
(defmacro DBG [tag & exprs]
    "debug macro for print-debugging:
tag is typically a constant string or keyword to identify who is printing,
but can be an arbitrary expression returning a tag to be princ'ed first;
if the tag evaluates to falsy (false or nil), nothing is printed.
exprs are expressions, which when the TAG was truthy are evaluated in order,
with their source code then their return values being printed each time.
The last expresion is *always* evaluated and its multiple values are returned,
but its source and return values are only printed if the tag was truthy;
previous expressions are not evaluated at all if TAG was falsy.
The macro expansion has relatively low overhead in space or time."
  (let [last-expr (last exprs)
        other-exprs (butlast exprs)
        thunk (gensym "thunk_")]
    `(let [tag# ~tag]
       (letfn ~(if exprs `[(~'thunk [] ~last-expr)] [])
         (if tag#
             (DBG-helper tag#
                         [~@(map #(do `['~% (fn [] ~%)]) other-exprs)]
                         ~(when exprs `['~last-expr ~'thunk]))
             ~(if exprs `(~'thunk) nil))))))

(defn DBG-helper [tag xts last-xt]
  ;; Helper for the above debugging macro
  (letfn [(foo [[expression thunk]]
            (print "  ") (pr expression) (print " => ")
            (let [val (thunk)]
              (prn val)
              val))]
    (println tag)
    (doseq [xt xts] (foo xt))
    (when last-xt (foo last-xt))))


(defmacro &DBG [tag f & x]
  `(let [x# ~(vec x)
         tag# ~tag]
     (fn [E#]
       (print tag#) (print " => ") (prn x#)
       (let [[v# E#] ((apply ~f x#) E#)]
         (print tag#) (print " <= ") (prn v#)
         [v# E#]))))
