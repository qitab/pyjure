(ns skylark.desugar
  (:use [skylark.parsing :only [merge-info]]
        [clojure.core.match :only [match]]))

;; macroexpand python syntax into a somewhat simpler language:
;; Only:
;;     lambda-calculus and variable bindings
;;           :function :call :id :assign :nonlocal :global :import :from :keyarg
;;           :builtin (all calls to core operators and constants)
;;           :del (variable name only; others are through :builtin call)
;;     control flow: :suite :if (two branches only) :raise :try
;;                   :while :continue :break :with :yield :yield-from
;; We don't yet simplify away the control flow, because it requires syntactic analysis
;; of what is local, nonlocal, global, what is yielding, etc., which is done later.
;; Or can we simplify it already to e.g. a single clojure-style loop statement?
;;
;; * decorators are expanded away
;; * function definitions transformed in x = function()... ; return None
;; * lambdas args: value become function (args): return value
;; * comprehensions are transformed into functions that yield
;; * augassign are transformed into assignments
;; * destructuring-binds are transformed into individual bindings
;; * comparisons are expanded into individual comparisons
;; * multibranch if is replaced by two-branch ifs.
;; * boolean operations are expanded into simple ifs
;; * nested suites are merged; pass is eliminated
;; Not yet(?):
;; ? with: is also expressed using higher-order functions? ==> not yet, due to assignments?
;; ? binary and unary operations are expanded into calls to primitive functions.


;; A macro-environment maps symbols to macros
(def null-macro-environment {})


;; The results of parsing is of the form [head args info]
;; We massage it into (with-meta (cons massaged-head massaged-args) {:source-info info})
;;
;; Differences from the python AST https://docs.python.org/3.5/library/ast.html
;; 0- we use lists headed by lower-case keywords, they use CamelCase node classes,
;;    and the names don't exactly match, e.g. :if instead of IfExp.
;; 1- we put the source-info in node meta-data, not as additional "attributes".
;; 2- we use lists for sexps, vectors for internal data sequences.


;; TODO: monadic treatment of environment?
(defn desugar
  ([form] (desugar form null-macro-environment))
  ([x E]
   (letfn [(X [a] (desugar a E))
           (w ([a] (w a (source-info x)))
              ([a i] (with-source-info a i)))
           (X* [s] (when s (map X s)))
           (Xvec [s] (vec (X* s)))
           (Xvec* [s] (vec (map Xvec s)))
           (def-arg [[[name type] default]]
             (when-not (nil? name)
               (list :argument (X name) (X type) (X default))))
           (def-xarg [[name type]]
             (when-not (nil? name)
               (list :argument (X name) (X type))))
           (def-args [[positional-args rest-arg more-args kw-arg]]
             [(vec (map def-arg positional-args))
              (def-xarg rest-arg)
              (vec (map def-arg more-args))
              (def-xarg kw-arg)])
           (Xarglist [l]
             (if-let [[args rarg margs kargs] l]
               [(Xvec args) (X rarg) (Xvec margs) (X kargs)]))]
     (cond
      (or (nil? x) (keyword? x)) x
      (vector? x)
        (let [[tag & as] x]
          (case tag
            (:from) (NIY) ;; TODO: handle import of macros!
            (:integer :float :string :bytes :imaginary
             :id
             :import
             :True :False :None :ellipsis
             :zero-uple :empty-list :empty-dict
             :lt :gt :eq :ge :le :ne :in :is :not-in :is_not) x
            (:expression :interactive :boolop :binop :unaryop :return :starred
             :module :assert :del :except :for :list :global :nonlocal :keyarg
             :raise :attribute :set :slice :tuple :while :suite
             :break :continue :pass) (w (cons tag (X* as)))
            (:dict) (w (cons tag (Xvec* x)))
            :def
            (let [[name args return-type body decorators] x]
              (w (list :def (X name) (def-args args) (X return-type) (X body) (Xvec decorators))))
            :decorator
            (let [[name args] as]
              (w (list :decorator (Xvec name) (Xarglist args))))
            :class ;; TODO: arbitrary args to class ?
            (let [[name superclasses body decorators] as]
              (w (list :class (X name) (Xvec superclasses) (X body) (Xvec decorators))))
            :call ;; TODO: call macros?
            (let [[fun args] x]
              (w (list :call (X fun) (Xarglist args))))
       :subscript
       (let [[arg indices] x]
         (w (list :subscript (X arg) (Xvec indices))))
       (w (list tag (X (conj x info))))
       :if
       (let [[clauses else] x]
         (w (list :if (Xvec* clauses) (X else))))
       :assign
       (let [v (Xvec x)]
         (w (list :assign (subvec v 0 (dec (count v))) (last v))))
       :augassign
       ;; :iadd :isub :imul :idiv :ifloordiv :imod :iand :ior :ixor :irshift :ilshift :ipow :imatmul
       (let [[a op b] x] (w (list :augassign (first op) (X a) (X b))))
       :compare
       (let [[a opargs] x
             opargs (map X* opargs)]
         (w (list tag (X a) (vec (map first opargs)) (vec (map second opargs)))))
       (:list-comp :set-comp :generator-exp)
       (let [[a for] x] (w (list tag (X a) (X for))))
       (:dict-comp)
       (let [[[a b] for] x] (w (list tag [(X a) (X b)] (X for))))
       (:comp-for) (let [[var gen more] (X* x)] (cons (w (list tag var gen)) more))
       (:comp-if) (let [[test more] (X* x)] (cons (w (list tag test)) more))
       :from
       (let [[[dots names] imports] x]
         (list :from [dots (X* names)]
               (if (= (first imports) :mul) :all
           (Xvec* imports))))
       :try
       (let [[body [excepts else finally]] x]
         (w (list :try (X body) (Xvec* excepts) (X else) (X finally))))
       :with
       (let [[items body] x]
         (w (list :with (vec (map (fn [[x y]] [(X x) (X y)]) items)) (X body))))
       :yield
       (w (if (= (first x) :subiterator)
            (list :yield-from (X (second x)))
            (list :yield (X x))))))))


(defn $generator [expr comps]
  [:function [[] nil [] nil] nil
   (reduce (fn [statement comp]
             (case (first comp)
               (:comp-for) (let [[_ target gen] comp]
                             (copy-source-info [:for target gen statement] comp))
               (:comp-if) (let [[_ test] comp]
                            (copy-source-info [:if [[target statement]] nil]) comp)))
           [:yield expr])))

(defn $generator-seq [expr gen]
  (match [gen]
    [nil] `(list ~expr)
    [[([:comp-for target gen] :seq) & more] `($comp-for gen (fn 
    [[([:comp-if test] :seq) & more]
    [([:comp-for target gen]


(defn desugar [program] ((X program) null-macro-environment))
