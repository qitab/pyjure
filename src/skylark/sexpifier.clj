(ns skylark.sexpifier
  (:use [skylark.parsing :only [merge-info]]
        [clojure.core.match :only [match]]))

;; The results of parsing is of the form [head args info]
;; We massage it into (with-meta (cons massaged-head massaged-args) {:source-info info})

(defn X [form]
  (match form
   nil nil
   [tag x info]
   (letfn [(w ([x] (w x info))
              ([x i] (with-meta x {:source-info i})))
           (X* [s] (when s (map X s)))
           (Xvec [s] (vec (X* s)))
           (Xvec* [s] (map Xvec s))
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
       (:Expression :Interactive) (X x)
       (:Module :and :and_ :assert :comp-for :comp-if :del :except :for :list :global :nonlocal
                :or :or_ :pow :progn :raise :select :set :slice :tuple :while :xor)
       (w (cons tag (X* x)))
       (:dict) (w (cons tag (Xvec* x)))
       (:return :not :pos :neg :invert :star :identity) (w (list tag (X x)))
       (:break :continue :pass) (w (list tag))
       (:import) (cons :import (map (fn [[names name]] [(X* names) (X name)]) x))
       :def
       (let [[name args return-type body decorators] x]
         (w (list :def (X name) (def-args args) (X return-type) (X body) (X* decorators))))
       :decorator
       (let [[name args] x]
         (w (list :decorator (X* name) (when args (Xarglist args)))))
       :class
       (let [[name superclasses body decorators] x]
         (w (list :class (X name) (X* superclasses) (X body) (X* decorators))))
       :call
       (let [[fun args] x]
         (w (list :call (X fun) (Xarglist args))))
       :subscript
       (let [[arg indices] x]
         (w (list :subscript (X arg) (X* indices))))
       :imaginary
       (w (list tag (X (conj x info))))
       :if
       (let [[clauses else] x]
         (w (list :if (Xvec* clauses) (X else))))
       :assign
       (let [[a b] x]
         (w (list :keyarg (X a) (X b))))
       :assign-expr
       (let [v (Xvec x)]
         (w (list :assign-expr (subvec v 0 (dec (count v))) (last v))))
       :augassign-expr
       ;; :iadd :isub :imul :idiv :ifloordiv :imod :iand :ior :ixor :irshift :ilshift :ipow :imatmul
       (let [[a op b] x] (w (list (first op) (X a) (X b))))
       :comparison
       (let [[a ops] x] (w (list* :comparison (X a) (map X* ops))))
       (:arith-expr :shift-expr :term)
       ;; :add :sub :mul :div :floordiv :mod :lshift :rshift
       (let [[[_ _ info :as a0] ops] x]
         (first (reduce (fn [[a info] [op b]]
                          (let [i (merge-info info (b 2))]
                            [(with-meta (list (first op) a (X b)) {:source-info i}) i]))
                        [(X a0) info] ops)))
       :comprehension
       (let [[kind a for] x] (w (list :comprehension kind (X a) (X for))))
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

