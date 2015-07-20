;; pyjure.passes: this file just combines together all the compilation passes.
(ns pyjure.passes
  (:use
   [pyjure.debug]))

;; Note that this file does *not* depend on the passes, but relies on them to be present at runtime.
;; This way, the passes can all depend on this file and have its functions present while debugging.
;; When the code is stable, this can be changed; or caching can be added.

(def passes
  '[;; * → [reader:java.io.Reader filename:String]
    :to-reader utilities

    ;; → [([char:Character & Position]...) filename:String]
    ;; where Position = [line:Integer column:Integer]
    :position-stream lexer ; NB: trivial wrapper around leijure.delta-position

    ;; → ^{:source-info Info} ([type:keyword data:*]...)
    ;; where Info = [filename:String start:Position end:Position]
    :lex lexer

    ;; → AST: nested ^{:source-info Info} [type:keyword & data:*]
    :parse parser

    ;; TODO: a pass to process imports, so dependencies can be statically traversed, early, before any evaluation.
    ;; Or should that be interspersed with desugaring, so that macros may expand into imports,
    ;; at the risk of macros having "interesting" side-effects interfering with dependencies?

    ;; → AST1: smaller, desugared language, with macros expanded.
    :desugar desugar

    ;; → [AST1bis effect]: clarifying: process global and nonlocal declarations, detect presence of yield,
    ;; and annotate :function and :class entries with scoping and effect information.
    :clarify clarification

    ;; → AST2: cleanup, with bindings resolved, generator functions distinguished,
    ;; some invalid forms filtered, etc. Insert vars for later type analysis.
    :cleanup cleanup

    ;; → AST3: transform into A-normal form, where all function call arguments are trivial,
    ;; i.e. constant, function or variable, and suites (once moved out of funcall) are merged.
    :anormalize anormalization

    ;; → AST3.1: analyze liveness of variables and effects captured in each statement's continuation.
    :analyze-continuations continuation-analysis

    ;; → AST3.2: analyze variables initialized and other effects in each statement's past
    :analyze-effects effect-analysis

    ;; control flow via 0CFA (?)

    ;; → converting to clojure code, executable with pyjure.runtime
    ;; TODO: in this pass, or a previous one,
    ;; re-inline use-once definitions that have no out-of-order effect,
    ;; transform :bind (setq) back into let and letfn where possible,
    ;; regrouping together mutually recursive functions by pushing
    :clojurify clojurifier

    ;; → evaluate
    :evaluate eval
  ])

(defn -pass-fun [pass-name pass-package]
  (find-var (symbol (str "pyjure." (name pass-package)) (str (name pass-name) "-"))))

;; Which vars should we bind from this list?
;; (->> (all-ns) (mapcat ns-publics) (map second) (filter (comp :dynamic meta)))
(defn pyjure
  ([input] (pyjure nil nil input))
  ([last-pass input] (pyjure nil last-pass input))
  ([first-pass last-pass input]
   (let [[names funs] ;; TODO: cache these vectors?
         (let [passes2 (map (fn [[name package]] [name (-pass-fun name package)])
                            (partition 2 passes))]
           (map #(vec (map % passes2)) [first second]))
         start (.indexOf names first-pass)
         end (.indexOf names last-pass)
         funs2 (subvec funs (inc start) (if (= end -1) (count funs) (inc end)))]
     (reduce #(%2 %) input funs2))))

(defmacro -def-funs []
  `(do ~@(map (fn [[pass _]]
                `(defn ~(symbol (name pass)) [~'x] (pyjure ~pass ~'x)))
              (partition 2 passes))))
(-def-funs)
