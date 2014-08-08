(ns skylark.sexpifier-test
  (:use [skylark.parsing :only [merge-info]]
        [clojure.core.match :only [match]]
        [clojure.test]
        [skylark.sexpifier])
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [skylark.core :as sky]))

;; check out the resources/ dir and clojure.java.io/resource
(def all-python (-> "skylark/python_test.py" clojure.java.io/resource))

(defn s [file]
  (-> file slurp sky/sexpify))

(defn foo []
  (-> "skylark/foo.py" clojure.java.io/resource s))

(deftest sexpify-test
  (testing "parser smoketest"
    (is (= (sky/sexpify "
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
           '(:module (:def hello [[(:argument world nil nil)] (:argument more nil) [] nil] nil (:progn (:call print [[] nil [] nil]) (:add (:add "a b" "c d") (:subscript foo ("abcd" bar "1 2 3" 0N 1N (:list 2N 3N 5N 7N 11N 13N 17N) 1.2345E68 1.0 1.0 1.0 1.0 1.0 (:add 1N (:imaginary 0.5)) (:neg 1N) 1.0 baz)))) ()) (:def quux [[] nil [] nil] nil (:progn (:dict ["x" "a"])) ())))))
  (testing "Every python lexing and parsing rule"
    (is (= (first (s all-python)) :module))))
