(ns skylark.parser-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.parser])
  (:require [skylark.core :as sky]
            [clojure.string :as str]))

(defn foo []
  (->> "skylark/foo.py" clojure.java.io/resource slurp sky/parse))

(defn test-parse [input] (tryf #(sky/parse input)))

(defn test& [l input] (tryf #(l (mkParserState (sky/lex input)))))

(deftest parser-test
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
           '[:Module ([:def [[:id "hello" [nil [2 4] [2 8]]] [([[[:id "world" [nil [2 11] [2 15]]] nil] nil]) [[:id "more" [nil [2 19] [2 22]]] nil] nil nil] nil [:progn ([:call [[:id "print" [nil [3 2] [3 6]]] [nil nil nil nil]] [nil [3 2] [3 8]]] [:arith-expr [[:string "a b" [nil [4 2] [4 6]]] ([[:add nil [nil [4 8] [4 8]]] [:string "c d" [nil [4 10] [4 14]]]] [[:add nil [nil [4 16] [4 16]]] [:subscript [[:id "foo" [nil [5 2] [5 4]]] ([:string "abcd" [nil [5 6] [5 11]]] [:id "bar" [nil [6 0] [6 2]]] [:string "1 2 3" [nil [6 5] [6 11]]] [:integer 0N [nil [6 14] [6 14]]] [:integer 1N [nil [6 17] [6 17]]] [:list ([:integer 2N [nil [6 22] [6 22]]] [:integer 3N [nil [6 25] [6 25]]] [:integer 5N [nil [6 28] [6 32]]] [:integer 7N [nil [6 35] [6 37]]] [:integer 11N [nil [6 40] [6 43]]] [:integer 13N [nil [6 46] [6 49]]] [:integer 17N [nil [6 52] [6 55]]]) [nil [6 20] [6 56]]] [:float 1.2345E68 [nil [7 0] [7 9]]] [:float 1.0 [nil [7 12] [7 13]]] [:float 1.0 [nil [7 16] [7 18]]] [:float 1.0 [nil [7 21] [7 25]]] [:float 1.0 [nil [7 28] [7 31]]] [:float 1.0 [nil [7 34] [7 39]]] [:arith-expr [[:integer 1N [nil [7 42] [7 42]]] ([[:add nil [nil [7 43] [7 43]]] [:imaginary [:float 0.5] [nil [7 44] [7 46]]]])] [nil [7 40] [7 46]]] [:neg [:integer 1N [nil [7 50] [7 50]]] [nil [7 49] [7 50]]] [:float 1.0 [nil [7 53] [7 55]]] [:id "baz" [nil [9 3] [9 5]]])] [nil [5 2] [9 6]]]])] [nil [3 9] [9 6]]]) [nil [3 2] [9 7]]] ()] [nil [0 0] [10 0]]] [:def [[:id "quux" [nil [10 4] [10 7]]] [nil nil nil nil] nil [:progn ([:dict ([[:string "x" [nil [11 3] [11 6]]] [:string "a" [nil [11 9] [11 11]]]]) [nil [11 2] [11 11]]]) [nil [11 2] [11 12]]] ()] [nil [10 0] [11 12]]]) [nil [0 0] [11 12]]]))))
