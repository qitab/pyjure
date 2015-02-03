;; pyjure.core: this file just combines together all the compilation passes.
(ns pyjure.core
  (:use [clojure.tools.trace]
        ;;[clojure.tools.nrepl]
        [clojure.repl]
        [clojure.test])
  ;;; Our passes, in order:
  (:require [pyjure.utilities :as utilities]
            [leijure.delta-position :as position]
            [pyjure.lexer :as lexer]
            [pyjure.parser :as parser]
            [pyjure.desugar :as desugar]
            [pyjure.clarification :as clarification]
            [pyjure.cleanup :as cleanup]
            [pyjure.anormalization :as anormalization]
            [pyjure.continuation-analysis :as continuation-analysis]
            [pyjure.effect-analysis :as effect-analysis]
            [pyjure.clojurifier :as clojurifier]
            [pyjure.eval :as eval]
            [pyjure.user :as user]))

(def passes
  [;; * → [reader:java.io.Reader filename:String]
   :to-reader #'utilities/ensure-reader-and-filename

   ;; → [([char:Character & Position]...) filename:String]
   ;; where Position = [line:Integer column:Integer]
   :position-stream #'lexer/position-stream ; NB: trivial wrapper around leijure.delta-position

   ;; → ^{:source-info Info} ([type:keyword data:*]...)
   ;; where Info = [filename:String start:Position end:Position]
   :lex #'lexer/lex

   ;; → AST: nested ^{:source-info Info} [type:keyword & data:*]
   :parse #'parser/parse

   ;; → AST1: smaller, desugared language, with macros expanded.
   :desugar #'desugar/desugar

   ;; → [AST1bis effect]: clarifying: process global and nonlocal declarations detect presence of yield,
   ;; and annotate :function and :class entries with scoping and effect information.
   :clarify #'clarification/clarify

   ;; → AST2: cleanup, with bindings resolved, generator functions distinguished,
   ;; some invalid forms filtered, etc. Insert vars for later type analysis.
   :cleanup #'cleanup/cleanup

   ;; → AST3: transform into A-normal form, where all function call arguments are trivial,
   ;; i.e. constant, function or variable, and suites (once moved out of funcall) are merged.
   :anormalize #'anormalization/anormalize

   ;; → AST3.1: analyze liveness of variables and effects captured in each statement's continuation.
   :analyze-continuations #'continuation-analysis/analyze-continuations

   ;; → AST3.2: analyze variables initialized and other effects in each statement's past
   :analyze-effects #'effect-analysis/analyze-effects

   ;; control flow via 0CFA (?)

   ;; → converting to clojure code, executable with pyjure.runtime
   :clojurify #'clojurifier/clojurify

   ;; → evaluate
   :evaluate #'eval/evaluate
  ])

;; Which vars should we bind from this list?
;; (->> (all-ns) (mapcat ns-publics) (map second) (filter (comp :dynamic meta)))
(defn pyjure
  ([input] (pyjure nil input))
  ([last-pass input] (pyjure nil last-pass input))
  ([first-pass last-pass input]
     (loop [x input
            p (let [i (.indexOf passes first-pass)]
                (if (= i -1) passes (subvec passes (+ i 2))))]
       (if (empty? p) x
           (let [[pass fun & more] p]
             (recur (fun x) (if (= pass last-pass) () more)))))))

(defmacro def-funs []
  `(do ~@(map (fn [[name _]]
                `(defn ~(symbol (.getName name)) [~'x] (pyjure ~name ~'x)))
              (partition 2 passes))))

(def-funs)
