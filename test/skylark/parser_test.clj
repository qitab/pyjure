(ns skylark.parser-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.parser])
  (:require [skylark.core :as sky]
            [clojure.string :as str]))

(def all-python (-> "skylark/python_test.py" clojure.java.io/resource))
(defn p [file] (-> file slurp sky/parse))
(defn foo [] (-> "skylark/foo.py" clojure.java.io/resource p))
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
           '[:module [:def [:id "hello"] [[[:argument [:id "world"] nil nil]] [:id "more"] [] nil] nil [:suite [:call [:id "print"] [[] nil [] nil]] [:binop :add [:binop :add [:string "a b"] [:string "c d"]] [:subscript [:id "foo"] [:tuple [:string "abcd"] [:id "bar"] [:string "1 2 3"] [:integer 0N] [:integer 1N] [:list [:integer 2N] [:integer 3N] [:integer 5N] [:integer 7N] [:integer 11N] [:integer 13N] [:integer 17N]] [:float 1.2345E68] [:float 1.0] [:float 1.0] [:float 1.0] [:float 1.0] [:float 1.0] [:binop :add [:integer 1N] [:imaginary [:float 0.5]]] [:unaryop :neg [:integer 1N]] [:float 1.0] [:id "baz"]]]]] []] [:def [:id "quux"] [[] nil [] nil] nil [:suite [:dict [:tuple [:string "x"] [:string "a"]]]] []]])))
  (testing "Every python lexing and parsing rule"
    (is (= (first (p all-python)) :module))))

