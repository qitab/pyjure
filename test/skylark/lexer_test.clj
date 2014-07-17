(ns skylark.lexer-test
  (:use [skylark.lexer])
  (:use [clojure.algo.monads])
  (:use [clojure.test]))

(defn tryf [fun] (try (fun) (catch clojure.lang.ExceptionInfo x (.data x))))

(defn test-lex* [input] (tryf #(python-lexer input)))

(defn simplify [[a b pos]]
  (let [[_ [x y] [z t]] pos]
    (assert (reduce #(and % %2) (map integer? [x y z t])))
    (assert (or (< x z) (and (= x z) (<= y t)))))
  (if (nil? b) a [a b]))

(defn test-lex [input] (map simplify (test-lex* input)))

(defn test& [l input] (tryf #(l (mkΣ input))))

(deftest lexer-test
  (set! *file* nil)
  (testing "lexer positions"
    (is (= (test-lex* "hello world\n  foo bar\n")
           '([:id "hello" [nil [1 0] [1 5]]] [:id "world" [nil [1 6] [1 11]]] [:newline nil [nil [2 0] [2 0]]] [:indent nil [nil [2 2] [2 2]]] [:id "foo" [nil [2 2] [2 5]]] [:id "bar" [nil [2 6] [2 9]]] [:newline nil [nil [2 9] [2 9]]] [:dedent nil [nil [2 9] [2 9]]] [:endmarker nil [nil [2 9] [2 9]]]))))
  (testing "lexer smoketest"
    (is (= (test-lex "
def hello (world, *more)
  print()
  \"a b\" + 'c d' + \\
  foo['abcd',
bar, \"1 2 3\", 0, 1, [ 2, 03, 0b101, 0x7, 0o13, 0O15, 0X11 ],
12.345e+67, 1., 1.0, 10e-1, .1e1, .01e+2, 1+.5j, -1, 1e0,
     # comment
  baz]
def quux ()
  {ur\"x\": \"a\"}")
           '(:def [:id "hello"] \( [:id "world"] :comma :mul [:id "more"] \) :newline :indent [:id "print"] \( \) :newline [:string "a b"] :add [:string "c d"] :add [:id "foo"] \[ [:string "abcd"] :comma [:id "bar"] :comma [:string "1 2 3"] :comma [:integer 0] :comma [:integer 1] :comma \[ [:integer 2] :comma [:integer 0] [:integer 3] :comma [:integer 5] :comma [:integer 7] :comma [:integer 11] :comma [:integer 13] :comma [:integer 17] \] :comma [:float 1.2345E68] :comma [:float 1.0] :comma [:float 1.0] :comma [:float 1.0] :comma [:float 1.0] :comma [:float 1.0] :comma [:integer 1] :add [:imaginary [:float 0.5]] :comma :sub [:integer 1] :comma [:float 1.0] :comma [:id "baz"] \] :newline :dedent :def [:id "quux"] \( \) :newline :indent \{ [:id "ur"] [:string "x"] :colon [:string "a"] \} :newline :dedent :endmarker)))))

