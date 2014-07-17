(ns skylark.parser-test
  (:use [clojure.tools.trace])
  (:use [clojure.tools.nrepl])
  (:use [clojure.repl])
  (:use [clojure.test])
  (:use [clojure.algo.monads])
  (:use [clojure.core.match :only [match]])
  (:use [skylark.lexer :refer [&return &nil &bind &let &do &do1]])
  (:use [skylark.parser])
  (:require [skylark.semantics :as s])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))


(defn tryf [fun] (try (fun) (catch clojure.lang.ExceptionInfo x (.data x))))

(defn test-parse [input] (tryf #(python-parser input)))

(defn test& [l input] (tryf #(l (parser-input input))))

(deftest parser-test
  (set! *file* nil)
  (testing "parser smoketest"
    (is (= (test-parse "
def hello (world, *more):
  print()
  \"a b\" + 'c d' + \\
  foo['abcd',
bar, \"1 2 3\", 0, 1, [ 2, 3, 0b101, 0x7, 0o13, 0O15, 0X11, ],
12.345e+67, 1., 1.0, 10e-1, .1e1, .01e+2, 1+.5j, -1, 1e0
     # comment
  ,baz]
def quux ():
  {u\"x\": \"a\"}")
           '[:Module ([:def [[:id "hello" [nil [2 4] [2 9]]] [([[[:id "world" [nil [2 11] [2 16]]] nil] nil]) [[:id "more" [nil [2 19] [2 23]]] nil] nil nil] nil [:progn ([:call [[:id "print" [nil [3 2] [3 7]]] [nil nil nil nil]] [nil [3 2] [3 9]]] [:arith-expr [[:string "a b" [nil [4 2] [4 7]]] ([[:add nil [nil [4 8] [4 9]]] [:string "c d" [nil [4 10] [4 15]]]] [[:add nil [nil [4 16] [4 17]]] [:subscript [[:id "foo" [nil [5 2] [5 5]]] ([:string "abcd" [nil [5 6] [5 12]]] [:id "bar" [nil [6 0] [6 3]]] [:string "1 2 3" [nil [6 5] [6 12]]] [:integer 0 [nil [6 14] [6 15]]] [:integer 1 [nil [6 17] [6 18]]] [:list ([:integer 2 [nil [6 22] [6 23]]] [:integer 3 [nil [6 25] [6 26]]] [:integer 5 [nil [6 28] [6 33]]] [:integer 7 [nil [6 35] [6 38]]] [:integer 11 [nil [6 40] [6 44]]] [:integer 13 [nil [6 46] [6 50]]] [:integer 17 [nil [6 52] [6 56]]]) [nil [6 21] [6 57]]] [:float 1.2345E68 [nil [7 0] [7 10]]] [:float 1.0 [nil [7 12] [7 14]]] [:float 1.0 [nil [7 16] [7 19]]] [:float 1.0 [nil [7 21] [7 26]]] [:float 1.0 [nil [7 28] [7 32]]] [:float 1.0 [nil [7 34] [7 40]]] [:arith-expr [[:integer 1 [nil [7 42] [7 43]]] ([[:add nil [nil [7 43] [7 44]]] [:imaginary [:float 0.5] [nil [7 44] [7 47]]]])] [nil [7 41] [7 47]]] [:neg [:integer 1 [nil [7 50] [7 51]]] [nil [7 49] [7 51]]] [:float 1.0 [nil [7 53] [7 56]]] [:id "baz" [nil [9 3] [9 6]]])] [nil [5 2] [9 7]]]])] [nil [4 0] [9 7]]]) [nil [3 2] [10 0]]] ()] [nil [0 0] [10 0]]] [:def [[:id "quux" [nil [10 4] [10 8]]] [nil nil nil nil] nil [:progn ([:dict ([[:string "x" [nil [11 3] [11 7]]] [:string "a" [nil [11 9] [11 12]]]]) [nil [11 3] [11 12]]]) [nil [11 2] [11 12]]] ()] [nil [10 0] [11 12]]]) [nil [0 0] [11 12]]]))))
