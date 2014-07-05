(ns skylark.parser
  (:require [skylark.lexer :as lex])
  (:use [skylark.lexer :refer [&return &bind &do]])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [clojure.algo.monads]))

;; https://docs.python.org/2/reference/grammar.html

;; This parser is written in monadic style.
;; type State = InputStream
;; monad PythonParser α = State → α×State
;;
;; The State is what input remains unconsumed from the lexer output,
;; a sequence of tokens of the form [type data info],
;; where info is of the form [file [start-line start-column] [end-line end-column]]
;;
;; Monadic parser entities have the & prefix.

(def fail-msg "python parser failure")
(defn fail ;; for richer throws, use slingshot ?
  ([] (fail {}))
  ([details] (throw (clojure.lang.ExceptionInfo. fail-msg details))))

(defn &error
  ([]
     (&error {}))
  ([details]
     (fn [in]
       (let [[_ _ info]]
         (fail (conj details [:info info]))))))
(def &fail (&error))
(defn try-parse [f σ]
  (try (f σ)
       (catch clojure.lang.ExceptionInfo x
         (when-not (= (.getMessage x) fail-msg) (throw x)))))
(defn &or* [ls]
  (fn [σ]
    (loop [l ls]
      (if (empty? l) ((&error) σ)
          (or (try-parse (first l) σ)
              (recur (rest l)))))))
(defn &or [& ls] (&or* ls))
(defn &not [l] ;; lookahead that l does not appear here
  (fn [σ] (if (try-parse l σ) (fail) [nil σ])))

(defmonad parser-m
  [ m-result &return
    m-bind &bind
    m-zero (&error)
    m-plus &or ])

(defmacro &let [& r] `(domonad parser-m ~@r))

(defn &token [[tok & rest]]
  [tok rest])
(defn &token-type-if [pred]
  (fn [[[type data info :as tok] & rest]] (if (= type t) [tok rest] (fail))))
(defn &token-type= [t] (&token-type-if #(= % t)))
(defn &token-type-if-not [pred] (&token-type-if #(not (pred %))))

(defn &optional [m]
  (&or m &nil))

(defn &fold [m f a]
  (&or (&bind m #(&fold m f (f a %))) (&return a)))

(defn &conj [m a]
  (&fold m conj a))

(defn &repeat [m]
  (&fold m (constantly nil) nil))

(def delimiters
  '("@" "," ":" "." "`" "=" ";"
    "+=" "-=" "*=" "/=" "//" "%="
    "&=" "|=" "^=" ">>=" "<<=" "**="))

(def operators
  '("+" "-" "*" "**" "/" "//" "%"
    "<<" ">>" "&" "|" "^" "~"
    "<=" ">=" "==" "!=" "<>" "<" ">"))

(def delimiters-and-operators
  ;; Order matters: prefixes must come afterwards, or we must use a better decision algorithm.
  (sort-by count > (concat delimiters operators)))

(def &delimiter-or-operator
  (&let [s (&or* (map #(&string= %) delimiters-and-operators))]
    [(keyword s) nil]))

(def paren-closer {\( \) \[ \] \{ \}})

(def &python
  &fail)

(defn python-parser [input]
  (first (&python (lex/python-lexer input))))

(comment
  (defn tryf [fun] (try (fun) (catch clojure.lang.ExceptionInfo x (.data x))))

  (defn test-parse [input] (tryf #(python-parser input)))

  (defn test& [l input] (tryf #(l (lex/python-lexer input))))

  (test-lex "
def hello (world, *more)
  print()
  \"a b\" + 'c d' + \\
  foo['abcd',
bar, \"1 2 3\", 0, 1, [ 2, 03, 0b101, 0x7, 0o13, 0O15, 0X11 ],
12.345e+67, 1., 1.0, 10e-1, .1e1, .01e+2, 1+.5j, -1,
     # comment
  baz]
def quux ()
  {ur\"x\": \"a\"}")
);comment
