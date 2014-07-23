(ns skylark.sexpify-test
  (:use [clojure.tools.trace])
  (:use [clojure.tools.nrepl])
  (:use [clojure.repl])
  (:use [clojure.test])
  (:use [clojure.algo.monads])
  (:use [clojure.core.match :only [match]])
  (:use [skylark.sexpify])
  (:require [skylark.semantics :as s])
  (:require [skylark.lexer :as l])
  (:require [skylark.parser :as p])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set]))

;; check out the resources/ dir and clojure.java.io/resource
(def all-python (-> "skylark/python_test.py" clojure.java.io/resource))

(defn s [file]
  (binding [*file* file]
    (->> file slurp Xp)))

(defn foo []
  (binding [*file* nil]
    (->> "skylark/foo.py" clojure.java.io/resource slurp Xp)))

(deftest sexpify-test
  (set! *file* nil)
  (testing "parser smoketest"
    (is (= (Xp "
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
           '(:Module (:def hello [[(argument world nil nil)] (argument more nil) [] nil] nil (:progn (:call print [] nil [] nil) (:add (:add "a b" "c d") (:subscript foo ("abcd" bar "1 2 3" 0 1 (:list 2 3 5 7 11 13 17) 1.2345E68 1.0 1.0 1.0 1.0 1.0 (:add 1 (:imaginary 0.5)) (:neg 1) 1.0 baz)))) ()) (:def quux [[] nil [] nil] nil (:progn (:dict ["x" "a"])) ())))))
  (testing "Every python lexing and parsing rule"
    (is (= (s all-python)
           '()))))
