;; skylark.core: this file just combines together all the compilation passes.
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
            [skylark.clarification :as clarification]
            [skylark.cleanup :as cleanup]
            [skylark.continuation-analysis :as continuation-analysis]
            [skylark.effect-analysis :as effect-analysis]
            [skylark.clojurifier :as clojurifier]
            [skylark.eval :as eval]
            [skylark.user :as user]))

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

   ;; → AST2: cleanup, with bindings resolved, suites merged, generator functions distinguished,
   ;; some invalid forms filtered, etc. Insert vars for later type analysis.
   :cleanup #'cleanup/cleanup

   ;; TODO:
   ;; → AST3: transform into A-normal form, where all function call arguments are trivial,
   ;; i.e. constant, function or variable.
   ;; (maybe do effect analysis first or intermingled, to ensure all variables are bound
   ;; or always add a binding verification, that will be checked later?)

   ;; → AST2.1: analyze liveness of variables and effects captured in each statement's continuation.
   :analyze-continuations #'continuation-analysis/analyze-continuations

   ;; → AST2.2: analyze variables initialized and other effects in each statement's past
   :analyze-effects #'effect-analysis/analyze-effects

   ;; control flow via 0CFA (?)

   ;; → converting to clojure code, executable with skylark.runtime
   :clojurify #'clojurifier/clojurify

   ;; → evaluate
   :evaluate #'eval/evaluate
  ])

;; Which vars should we bind from this list?
;; (->> (all-ns) (mapcat ns-publics) (map second) (filter (comp :dynamic meta)))
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
