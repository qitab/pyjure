(ns pyjure.desugar-test
  (:use [clojure.test]
        [clojure.core.match :only [match]]
        [pyjure.core-test]
        [pyjure.debug]
        [pyjure.passes]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.desugar]))

(defn test-desugar [input] (tryf #(desugar input)))

(deftest desugar-test
  (testing "desugar smoketest"
    (is (= (desugar "a ; b+1 ; c")
           [:module [:suite [:id "a"] [:builtin :add [:id "b"] [:constant [:integer 1N]]] [:id "c"]]]))
    (is (= (desugar "f(a, b)")
           [:module [:call [:id "f"] [[[:id "a"] [:id "b"]] nil [] nil]]]))
    (is (= (desugar "@foo\n@bar.baz(1)\ndef hello(): pass")
           [:module [:suite [:bind [:id "hello"] [:function [[] nil [] nil] nil
                                                  [:suite [:constant [:None]] [:constant [:None]]]]]
                     [:bind [:id "hello"]
                      [:call [:call [:builtin :attribute [:id "bar"] [:constant [:string "baz"]]]
                              [[[:constant [:integer 1N]]] nil [] nil]]
                       [[[:id "hello"]] nil [] nil]]]
                     [:bind [:id "hello"] [:call [:id "foo"] [[[:id "hello"]] nil [] nil]]]]])))
  (testing "Every python desugaring rule"
    (match [(test-desugar python-test)] [[':module & _]]
           (is (= 1 1)))))
