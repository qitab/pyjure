(ns skylark.desugar
  (:use [skylark.parsing :only [merge-info]]
        [clojure.core.match :only [match]]))

;; macroexpand python syntax into a somewhat simpler language:
;; Only:
;;     lambda-calculus and variable bindings
;;           :function :call :id :assign :nonlocal :global :import :from :keyarg
;;           :builtin (all calls to core operators) :constant
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

(def ^{:dynamic true} *gensym-counter* 0)
(defn $gensym [] (str "g-" *gensym-counter*))

;; TODO: monadic treatment of environment?

(comment
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
              [(Xvec args) (X rarg) (Xvec margs) (X kargs)])))

(declare &desugar)

(defn &desugar* [xs] (&map &desugar xs))
(defn &desugar** [xs] (&map &desugar* xs))
(defn &desugar/ [h xs] (&let [s (&desugar* s)] (into h s)))
(defn &desugar// [h xs] (&let [s (&desugar** s)] (into h (map vec s))))

(defn &desugar-target [x right kind i]
  (letfn [(c [x] (copy-source-info x i))
          (v [& args] (c (vec args)))
          (flatten* [x acc]
            (cond (vector? x) (conj acc x)
                  (seq? x) (reduce #(flatten* %2 %) acc x)
                  :else ($error "oops in flatten")))
          (flatten [x] (reverse (flatten* x ())))
          (D [x right]
            (fn [s]
              (if-let [[h & as] (and (vector? x) x)]
                (case h
                  (:id) (v :assign x right)
                  (:attribute) ($error "Attribute as a target is impure")
                  (:subscript) ($error "Subscript as a target is impure")
                  (:tuple :list)
                  (if (not (= kind :assign)) ($error "list or tuple not allowed as target in augassign")
                      (let [r ($gensym)
                            si (map first (filter #(= (first (second %)) :starred) (map-indexed as)))
                            [head starred tail] (if si

                        (list (v :assign r right)
                              (loop [index 0N as as stmts []]
                                (match [as]
                                  [([] :seq)]
                                  (v :builtin :check-length-eq r
                                     (v :constant (v :integer index)))
                                  [([[:starred t] & tail] :seq)]
                                  (let [n (count tail)
                                        ng ($gensym)]
                                    (list
                                     (v :builtin :check-length-ge right
                                        (v :constant (v :integer (+ index n))))
                                     (v :assign ng (v :builtin :length right))
                                     stmts
                              (map-indexed #(D

      ($error "Syntax Error: invalid target"))))

(defn expand-compare [[left ops args] x]
  (if-let [[op & moreops] ops]
    (let [[arg & moreargs] args
          g (when moreops ($gensym))
          [right init] (if g [g [(w :assign g arg)]] [arg nil])]
      `[:suite ~@init ~(copy-source-info
                        [:if [[(with-source-info [:builtin op left right]
                                 (merge-info left right))
                               (expand-compare [right moreops moreargs] x)]]
                              (copy-source-info [:constant :False] x)])])
    (copy-source-info [:constant :True] x)])

(defn expand-cond [[clauses else] x]
  (if-let [[[test iftrue] & moreclauses] clauses]
    (copy-source-info [:if test iftrue (expand-cond [moreclauses else] x)] x)
    (or else (copy-source-info [:constant :None] x))))

(defn &desugar [x]
  (cond
   (or (nil? x) (keyword? x)) (&return x)
   (vector? x)
   (let [[tag & as] x]
     (letfn [(i [f] (copy-source-info f x))
             (v [& s] (i (apply vec* s)))
             (w [& s] (i (vec s)))]
     (case tag
       (:from)
       (let [[[dots names] imports] x]
         (NIY {:r "&desugar from"}) ;; TODO: handle import of macros!

         (list :from [dots (X* names)]
               (if (= (first imports) :mul) :all
           (Xvec* imports))))

       (:import :id :builtin :return :constant) (&return x) ;; :constant is for recursive desugar
       (:integer :float :string :bytes :imaginary
        :True :False :None :Ellipsis
        :zero-uple :empty-list :empty-dict) (&return (w :constant x))
       (:expression :interactive) (&desugar (first as))
       (:module :pass) (&desugar (v :suite as))
       ;; :builtin is for recursively desugared code.
       ;; :lt :gt :eq :ge :le :ne :in :is :not-in :is_not are transformed into builtin's as well.
       (:builtin :binop :unaryop) (&let [s (&desugar* (rest as))] (v :builtin (first as) s))
       (:del) (if (rest as) (&desugar (v :suite (map #(w del %) as)))
                  (let [[h & t :as a] as]
                    (if (= h :id) (&return x) ;; del identifier remains as primitive
                        (NIY {:r "Can only del names" :a a}))))
       (:assert :list :set :tuple :slice :subscript :yield :yield-from)
       (&let [s (&desugar* as)] (v :builtin tag s))
       (:for) (let [[target generator body] as
                    g ($gensym)]
                ($desugar
                 (v :suite
                    (w :assign g generator)
                    (w :while (w :builtin :gen-next? g)
                       (v :suite
                          (w :assign (w :tuple g target) (w :builtin :gen-next g))
                          body)))))
       (:dict) (&let [s (&desugar** x)] (w (vec* tag (map vec s))))
       (:except :keyarg) ($error "Not at toplevel: %s" [:x] {:x x})
       (:global :nonlocal) (if (rest as) (&desugar (v :suite (map #(w tag %) as)))
                               (&return x))
       (:attribute) (let [[x [_ s :as n]] as]
                      (&let [x (&desugar x)]
                            (w :builtin :attribute x (copy-source-info [:string s] n))))
       (:raise :while :break :continue :if) (&let [s (&desugar* as)] (v tag s)) ;; if is for recursing
       (:compare) (&desugar (expand-compare as x))
       (:cond) (&desugar (expand-cond as))

       (:suite) (NIY) ;; TODO: unwrap singletons, flatten nested suites

       (:augassign)
       (let [[target iop arg] as
             op ({:iadd :add :isub :sub
                  :imul :mul :idiv :div :ifloordiv :floordiv :imod :mod
                  :iand :and :ior :or :ixor :xor :irshift :rshift :ilshift :lshift
                  :ipow :pow :imatmul :matmul} iop)]
         (

       (:def)
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
       :assign
       (let [v (Xvec x)]
         (w (list :assign (subvec v 0 (dec (count v))) (last v))))
       (let [[a op b] x] (w (list :augassign (first op) (X a) (X b))))
       (:list-comp :set-comp :generator)
       (let [[expr gens]
       (&desugar
       (w :builtin
       (let [[a for] x] (w (list tag (X a) (X for))))
       (:dict-comp)
       (let [[[a b] for] x] (w (list tag [(X a) (X b)] (X for))))
       (:comp-for) (let [[var gen more] (X* x)] (cons (w (list tag var gen)) more))
       (:comp-if) (let [[test more] (X* x)] (cons (w (list tag test)) more))
       :try
       (let [[body [excepts else finally]] x]
         (w (list :try (X body) (Xvec* excepts) (X else) (X finally))))
       :with
       (let [[items body] x]
         (w (list :with (vec (map (fn [[x y]] [(X x) (X y)]) items)) (X body))))


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


(defn desugar [program] ((&desugar program) null-macro-environment))
