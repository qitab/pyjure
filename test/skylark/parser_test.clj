(ns skylark.parser-test
  (:use [skylark.parser])
  (:use [clojure.algo.monads])
  (:use [clojure.tools.trace])
  (:use [clojure.tools.nrepl])
  (:use [clojure.repl])
  (:use [clojure.test]))

(defn tryf [fun] (try (fun) (catch clojure.lang.ExceptionInfo x (.data x))))

(defn test-parse [input] (tryf #(python-parser input)))

(defn test& [l input] (tryf #(l (parser-input input))))


(deftest parser-test
  (set! *file* nil)
  (testing "parser smoketest"
    (is (= (test-parse "
def hello (world, *more)
  print()
  \"a b\" + 'c d' + \\
  foo['abcd',
bar, \"1 2 3\", 0, 1, [ 2, 03, 0b101, 0x7, 0o13, 0O15, 0X11 ],
12.345e+67, 1., 1.0, 10e-1, .1e1, .01e+2, 1+.5j, -1, 1e0
     # comment
  baz]
def quux ()
  {ur\"x\": \"a\"}")
           []))))
