(ns skylark.parser
  (:require [skylark.lexer :as lex])
  (:use [skylark.lexer :refer [&return &bind &do]])
  (:require [skylark.semantics :as s])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [clojure.algo.monads]))

;; Input grammar: https://docs.python.org/2/reference/grammar.html
;; Output AST: https://docs.python.org/2/library/ast.html

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
(defn fail
  ;; For more efficient failing, we ought to use a monad that knows about continuations
  ([] (fail {}))
  ([details] (throw (clojure.lang.ExceptionInfo. fail-msg details))))

(defn &error
  ([]
     (&error {}))
  ([details]
     (fn [in]
       (let [[[_ _ info]] in]
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

(def &nil (&return nil))

(defmonad parser-m
  [ m-result &return
    m-bind &bind
    m-zero (&error)
    m-plus &or ])

(defmacro &let [& r] `(domonad parser-m ~@r))

(defn &fold [m f a]
  (&or (&bind m #(&fold m f (f a %))) (&return a)))

(defn &list [m]
  (&let [r (&fold m conj ())] (reverse r)))

(defn &optional [m]
  (&or m &nil))

(defn &repeat [m]
  (&fold m (constantly nil) nil))

(defn &token [[tok & rest]]
  [tok rest])
(defn &type-if [pred]
  (fn [[[type data info :as tok] & rest]] (if (pred type) [tok rest] (fail))))
(defn &type= [t] (&type-if #(= % t)))
(defn &type-if-not [pred] (&type-if #(not (pred %))))

(def delimiters
  '("@" "," ":" "." "`" "=" ";"
    "+=" "-=" "*=" "/=" "//" "%="
    "&=" "|=" "^=" ">>=" "<<=" "**="))

(def operators
  '("+" "-" "*" "**" "/" "//" "%"
    "<<" ">>" "&" "|" "^" "~"
    "<=" ">=" "==" "!=" "<>" "<" ">"))

(def &NIY &fail)
(defn &paren [opener m closer]
  (&let [_ (&type= opener) x m _ (&type= closer)] x))

(defn &newline
  ([] (&newline nil))
  ([value] (&do (&type= :newline) (&return value))))

(def &dotted-name
  &NIY)
(def &arglist
  &NIY)
(def &class-definition
  &NIY)
(def &function-definition
  &NIY)
(def &simple-statement
  &NIY)
(def &compound-statement
  &NIY)

(def &decorator
  (&let [_ (&type= 'at)
         name &dotted-name
         args (&optional (&paren \( &arglist \)))
         _ (&newline)]
    (if (nil? args)
      name
      (cons name args))))

(def &decorators
  (&list &decorator))

(defn &decorated [m]
  (&let [d &decorators
         x m]
    (if d `(s/decorated ~d ~x) x)))

(def &definition (&decorated (&or &class-definition &function-definition)))

;; Start symbols for the grammar:
;;   &single-input is a single interactive statement;
;;   &file-input is a module or sequence of commands read from an input file;
;;   &eval-input is the input for the eval() and input() functions.

(def &single-input
  (&or (&newline) &simple-statement (&bind &compound-statement &newline)))

(def &file-input
  (&or (&newline) &simple-statement (&bind &compound-statement &newline)))

(def &eval-input
  (&or (&newline) &simple-statement (&bind &compound-statement &newline)))

(defn python-parser [input]
  (first (&file-input (lex/python-lexer input))))

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
