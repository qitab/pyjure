(ns skylark.desugar
  (:use [skylark.parsing :only [merge-info]]
        [clojure.core.match :only [match]]))

;; macroexpand python syntax into a simpler language:



;; The results of parsing is of the form [head args info]
;; We massage it into (with-meta (cons massaged-head massaged-args) {:source-info info})
;;
;; Differences from the python AST https://docs.python.org/3.5/library/ast.html
;; 0- we use lists headed by lower-case keywords, they use CamelCase node classes,
;;    and the names don't exactly match, e.g. :if instead of IfExp.
;; 1- we put the source-info in node meta-data, not as additional "attributes".
;; 2- we use lists for sexps, vectors for internal data sequences.

(defn X [form]
  (match form
   nil nil
   [tag x info]
   (letfn [(w ([x] (w x info))
              ([x i] (with-meta x {:source-info i})))
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
           (Xarglist [[args rarg margs kargs]]
             [(Xvec args) (X rarg) (Xvec margs) (X kargs)])]
     (case tag
       (:integer :float :string :bytes) x
       (:id) (w (symbol x))
       (:True :False :None :ellipsis
        :zero-uple :empty-list :empty-dict
        :lt :gt :eq :ge :le :ne :in :is :not-in :is_not) tag
       (:expression :interactive) (X x)
       (:module :assert :del :except :for :list :global :nonlocal :keyarg
                :progn :raise :attribute :set :slice :tuple :while) (w (cons tag (X* x)))
       (:and :or) (w (list* :boolop tag (X* x)))
       (:and_ :or_ :xor :pow) (w (list* :binop tag (X* x)))
       (:dict) (w (cons tag (Xvec* x)))
       (:return :star :identity) (w (list tag (X x)))
       (:not :pos :neg :invert) (w (list :UnaryOp tag (X x)))
       (:break :continue :pass) (w (list tag))
       (:import) (cons :import (map (fn [[names name]] [(Xvec names) (X name)]) x))
       :def
       (let [[name args return-type body decorators] x]
         (w (list :def (X name) (def-args args) (X return-type) (X body) (Xvec decorators))))
       :decorator
       (let [[name args] x]
         (w (list :decorator (Xvec name) (when args (Xarglist args)))))
       :class
       (let [[name superclasses body decorators] x]
         (w (list :class (X name) (Xvec superclasses) (X body) (Xvec decorators))))
       :call
       (let [[fun args] x]
         (w (list :call (X fun) (Xarglist args))))
       :subscript
       (let [[arg indices] x]
         (w (list :subscript (X arg) (Xvec indices))))
       :imaginary
       (w (list tag (X (conj x info))))
       :if
       (let [[clauses else] x]
         (w (list :if (Xvec* clauses) (X else))))
       :assign
       (let [v (Xvec x)]
         (w (list :assign (subvec v 0 (dec (count v))) (last v))))
       :augassign-expr
       ;; :iadd :isub :imul :idiv :ifloordiv :imod :iand :ior :ixor :irshift :ilshift :ipow :imatmul
       (let [[a op b] x] (w (list :augassign (first op) (X a) (X b))))
       :compare
       (let [[a opargs] x
             opargs (map X* opargs)]
         (w (list tag (X a) (vec (map first opargs)) (vec (map second opargs)))))
       (:arith-expr :shift-expr :term)
       ;; :add :sub :mul :div :floordiv :mod :lshift :rshift
       (let [[[_ _ info :as a0] ops] x]
         (first (reduce (fn [[a info] [op b]]
                          (let [i (merge-info info (b 2))]
                            [(with-meta (list :binop (first op) a (X b)) {:source-info i}) i]))
                        [(X a0) info] ops)))
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


;; Problem: 
(defn $generator-exp [expr gen] `(make-generator ($generator-seq expr gen)))

(defn $generator-seq [expr gen]
  (match [gen]
    [nil] `(list ~expr)
    [[([:comp-for target gen] :seq) & more] `($comp-for gen (fn  
    [[([:comp-if test] :seq) & more]
     
    [([:comp-for target gen]
