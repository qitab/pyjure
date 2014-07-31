(ns skylark.core
  ;;; Our passes, in order:
  (:require [skylark.utilities :as utilities]
            [leijure.delta-position :as position]
            [skylark.lexer :as lexer]
            [skylark.parser :as parser]
            [skylark.sexpifier :as sexpifier]
            [skylark.scope-analysis :as scope-analysis]
            [skylark.clojurifier :as clojurifier]))

;; TODO: this file should just combine together all the passes.

(def passes
  [;; * → [reader:java.io.Reader filename:String]
   :to-reader utilities/ensure-reader-and-filename

   ;; → [([char:Character & Position]...) filename:String]
   ;; where Position = [line:Integer column:Integer]
   :position-stream lexer/position-stream ; NB: trivial wrapper around leijure.delta-position

   ;; → ([type:keyword data:* info:Info]...)
   ;; where Info = [filename:String start:Position end:Position]
   :lex lexer/lex

   ;; → nested [type:keyword data:* info:Info]
   :parse parser/parse

   ;; → Sexp: ^{:source-info Info} (type:keyword & arguments:*)
   :sexpify sexpifier/X

   ;; → same, but :def and :class entries annotated with sets of bindings
   :analyze-scope scope-analysis/A

   ;; → converting to clojure code, executable with skylark.runtime
   :clojurify clojurifier/C
  ])

(defn skylark
  ([input] (skylark nil input))
  ([last-pass input]
     (loop [x input p passes]
       (if (empty? p) x
           (let [[pass fun & more] p]
             (recur (fun x) (if (= pass last-pass) () more)))))))

(defmacro def-funs []
  `(do ~@(map (fn [[name _]]
                `(defn ~(symbol (.getName name)) [~'x] (skylark ~name ~'x)))
              (partition 2 passes))))

(def-funs)
