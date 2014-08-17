(ns skylark.core
  (:use [clojure.tools.trace]
        ;;[clojure.tools.nrepl]
        [clojure.repl]
        [clojure.test])
  ;;; Our passes, in order:
  (:require [skylark.utilities :as utilities]
            [leijure.delta-position :as position]
            [skylark.lexer :as lexer]
            [skylark.parser :as parser]
            [skylark.desugar :as desugar]
            [skylark.syntax-analysis :as syntax-analysis]
            [skylark.cleanup :as cleanup]
            [skylark.clojurifier :as clojurifier]))

;; TODO: this file should just combine together all the passes.
;; TODO: interleave evaluation and macro-expansion?

(def passes
  [;; * → [reader:java.io.Reader filename:String]
   :to-reader utilities/ensure-reader-and-filename

   ;; → [([char:Character & Position]...) filename:String]
   ;; where Position = [line:Integer column:Integer]
   :position-stream lexer/position-stream ; NB: trivial wrapper around leijure.delta-position

   ;; → ([type:keyword data:* info:Info]...)
   ;; where Info = [filename:String start:Position end:Position]
   :lex lexer/lex

   ;; → AST: nested ^{:source-info Info} [type:keyword & data:*]
   :parse parser/parse

   ;; → AST1: smaller, desugared language, with macros expanded.
   :desugar desugar/desugar

   ;; → AST1bis: annotating :function entries with scoping and effect information
   :analyze-syntax syntax-analysis/analyze-syntax

   ;; → AST2: cleanup, with bindings resolved, suites merged, generator functions distinguished,
   ;; some invalid forms filtered, etc. Insert vars for later type analysis.
   :cleanup cleanup/cleanup

   ;; → AST3: type analysis via 0CFA (?)

   ;; → converting to clojure code, executable with skylark.runtime
   :clojurify clojurifier/C
  ])

(defn skylark
  ([input] (skylark nil input))
  ([last-pass input] (skylark nil last-pass input))
  ([first-pass last-pass input]
     (loop [x input
            p (let [i (.indexOf passes first-pass)]
                (if (= i -1) passes (subvec passes (+ i 2))))]
       (if (empty? p) x
           (let [[pass fun & more] p]
             (recur (fun x) (if (= pass last-pass) () more)))))))

(defmacro def-funs []
  `(do ~@(map (fn [[name _]]
                `(defn ~(symbol (.getName name)) [~'x] (skylark ~name ~'x)))
              (partition 2 passes))))

(def-funs)
