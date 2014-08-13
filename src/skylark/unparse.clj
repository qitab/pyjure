(ns skylark.unparser
  (:use [skylark.utilities]
        [skylark.parsing]))

(defn unparse
  ([x] (unparse x 0))
  ([x i] ; expression, indentation
     (match [x]
       [[(:or :pass :break :continue) :as tag]] (&outln (:name tag)



       (:from)
       (let [[[dots names] imports] as]
         (NIY {:r "&desugar from"})) ;; TODO: handle import of macros!
       (:import :id :return :constant) (&return x) ;; :constant is for recursive desugar
       (:integer :float :string :bytes :imaginary
        :True :False :None :Ellipsis
        :zero-uple :empty-list :empty-dict) (&return (w :constant x))
       (:expression :interactive) (&desugar (first as))
       (:module :pass) (&desugar (v :suite as))
       ;; :builtin is for recursively desugared code.
       ;; :lt :gt :eq :ge :le :ne :in :is :not-in :is_not are transformed into builtin's as well.
       (:builtin :binop :unaryop) (&let [s (&desugar* (rest as))] (v :builtin (first as) s))
       (:del) (if (rest as) (&desugar (v :suite (map #(w :del %) as)))
                  (let [[h & t :as a] as]
                    (if (= h :id) (&return x) ;; del identifier remains as primitive
                        (NIY {:r "Can only del names" :a a}))))
       (:assert :list :dict :set :tuple :slice :subscript :yield :yield-from)
       (&let [s (&desugar* as)] (v :builtin tag s))
       (:for) (let [[target generator body] as
                    g ($gensym)]
                (&desugar
                 (v :suite
                    (w :assign g generator)
                    (w :while (w :builtin :gen-next? g)
                       (v :suite
                          (w :assign (w :tuple g target) (w :builtin :gen-next g))
                          body)))))
       (:except :keyarg) ($error "Not at toplevel: %s" [:x] {:x x})
       (:global :nonlocal) (if (rest as) (&desugar (v :suite (map #(w tag %) as)))
                               (&return x))
       (:attribute) (let [[x [_ s :as n]] as]
                      (&let [x (&desugar x)]
                            (w :builtin :attribute x (copy-source-info [:string s] n))))
       (:raise :while :break :continue :if) (&let [s (&desugar* as)] (v tag s))
       (:compare) (&desugar (expand-compare as x))
       (:cond) (&desugar (expand-cond as))
       (:suite) (NIY) ;; TODO: unwrap singletons, flatten nested suites
       (:augassign)
       (let [[target iop arg] as
             op ({:iadd :add :isub :sub
                  :imul :mul :idiv :div :ifloordiv :floordiv :imod :mod
                  :iand :and :ior :or :ixor :xor :irshift :rshift :ilshift :lshift
                  :ipow :pow :imatmul :matmul} iop)]
         (&desugar (expand-target target (w :builtin op target arg) :augassign x)))
       (:assign)
       (let [[targets expr] as]
         (&let [val (&desugar expr)
                z (match [targets]
                    [[[:id _]]] (&return (w :assign (first targets) val))
                      [[_]] (&return (expand-target (first targets) val))
                      :else (let [g ($gensym)]
                              (&let [y (&desugar
                                        (v :suite
                                           (map #(expand-target % g :assign x) (reverse targets))))]
                                    (w :suite (w :assign g val) y))))]))
       (:list-comp :dict-comp :set-comp :generator)
       (let [[expr gen] as]
         (&let [g (&desugar (expand-generator expr gen))]
               (w :builtin tag g)))
       :try
       (let [[body [excepts else finally]] x]
         (&let [body (&desugar body)
                excepts (vec (&map (fn [[[_ extype exvar] suite]]
                                     (&desugar-except extype exvar suite))
                                   excepts))
                else (&desugar else)
                finally (&desugar finally)]
               (w :try body excepts else finally)))
       (:call) ;; TODO: call macros?
       (let [[fun args] as]
         (if-let [f (&call-macro fun)]
           (&bind (f x) &desugar)
           (&let [fun (&desugar fun)
                  args (&desugar-args args)]
                 (w :call fun args))))
       (:def)
       (let [[name args return-type body decorators] x]
         (w (list :def (X name) (def-args args) (X return-type) (X body) (Xvec decorators))))
       (comment
       :decorator
       (let [[name args] as]
         (w (list :decorator (Xvec name) (Xarglist args))))
       :class ;; TODO: arbitrary args to class ?
       (let [[name superclasses body decorators] as]
         (w (list :class (X name) (Xvec superclasses) (X body) (Xvec decorators))))
       :with
       (let [[items body] x]
         (w (list :with (vec (map (fn [[x y]] [(X x) (X y)]) items)) (X body))))))

