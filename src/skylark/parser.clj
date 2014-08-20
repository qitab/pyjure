(ns skylark.parser
  (:require [clojure.string :as str])
  (:use [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.parsing]))

;; This is largely based on the Python 3.5 grammar
;; Python 3 grammar: https://docs.python.org/3.5/reference/grammar.html
;;
;; Our output is very similar to the Python AST: https://docs.python.org/3.5/library/ast.html
;; except that tuples are headed by keywords rather than new record types.
;; other differences: Class

;; This parser is written using the parsing monad from parsing.clj, with
;; type State = InputStream×LastTokenInfo
;;
;; The State is what input remains unconsumed from the lexer output,
;; a sequence of tokens of the form [type data info],
;; where info is of the form [file [start-line start-column] [end-line end-column]]

;; The results of parsing is vectors headed by a keyword with source info in meta-data:
;; (with-meta [:head args...] {:source-info info})
;;
;; Differences from the python AST https://docs.python.org/3.5/library/ast.html
;; 0- we use lists headed by lower-case keywords, they use CamelCase node classes,
;;    and the names don't exactly match, e.g. :cond instead of IfExp.
;; 1- we put the source-info in node meta-data, not as additional "attributes".
;; 2- we use lists for sexps, vectors for internal data sequences.



;;; Basic monad constructors
(defrecord ParserState [in prev-info] ; our State
  SourceInfoStream
  (prev-info [σ] (:prev-info σ))
  (next-info [σ]
    (match (:in σ)
      [[x]] (source-info x)
      _ (if-let [[file _ end] (:prev-info σ)] [file end end]))))


;;; Parsing tokens and location information

(defn &token [{[tok & rest] :in}] [tok (->ParserState rest (source-info tok))])
(defn &type-if [pred]
  (fn [{[[type _ info :as tok] & rest] :in :as σ}]
    (if (pred type) [tok (->ParserState rest info)] (&fail σ))))
(defn &type [t] (&type-if #(= % t)))


;;; Parsing utilities

(def &comma (&type :comma))
(def &optional-comma (&optional &comma))
(def &dot (&type :dot))
(def &dots0 (&fold (&type-if #{:dot :Ellipsis}) #(+ % ({:dot 1 :Ellipsis 3} (first %2))) 0))
(def &dots (&let [x &dots0 _ (if (= x 0) &fail &nil)] x))
(def &colon (&type :colon))
(def &name (&type :id))

(defn &prefixed* [prefix & ms] (&info (&do (&type prefix) (apply &tag* prefix ms))))
(defn &prefixed [prefix & ms] (&info (&do (&type prefix) (apply &tag prefix ms))))

(defn &non-empty-separated-list
  ([m] (&non-empty-separated-list m &comma))
  ([m separator] (&bind m (fn [a] (&list (&do separator m) (list a))))))

(defn &non-empty-maybe-terminated-list
  ([m] (&non-empty-maybe-terminated-list m &comma))
  ([m separator]
     (&do1 (&non-empty-separated-list m separator) (&optional separator))))

(defn &maybe-terminated-list
  ([m] (&maybe-terminated-list m &comma))
  ([m separator]
     (&or (&non-empty-maybe-terminated-list m separator)
          (&return ()))))

(defn &tuple-or-singleton [m] ;; have expr-context :load :store :del :aug-load :aug-store :param ???
  (&leti [l (&non-empty-separated-list m)
          s &optional-comma]
         (if (and (empty? (rest l)) (nil? s)) (first l) (vec* :tuple l))))

(defn &paren
  ([m] (&paren \( \) m))
  ([opener closer m] (&let [_ (&type opener) * m _ (&type closer)])))

(def &newline (&type :newline))

(defn &mod-expr [m modifier f]
  (&leti [x m mod (&optional modifier)] (if mod (f x mod) x)))

(defn &op-expr [op m f]
  (&mod-expr m (&non-empty-list (&do (&type op) m)) #(f (cons % %2))))

(defn &multi-op-expr [op m f]
  (&mod-expr m (&non-empty-list (&vector op m)) f))

(defn &unary-op-expr [opmap m]
  (&leti [l (&list (&type-if opmap)) x m]
         (reduce (fn [x [op :as o]]
                   (with-source-info [:unaryop (opmap op) x]
                     (merge-info (source-info o) (source-info x))))
                 x (reverse l))))

(defn merge-strings [ss]
  (assert (seq ss))
  (let [[[b s1 :as fs] & rs] ss
        i1 (source-info fs)]
    (loop [sl (list s1) i2 i1 r rs]
      (if (empty? r)
        (with-source-info [b ((if (= b :bytes) join-bytes str/join) (reverse sl))] (merge-info i1 i2))
        (let [[[b2 s2 i2] r2] r]
          (assert (= b b2))
          (recur (cons s2 sl) i2 r2))))))

(defn binop [left more]
  (reduce (fn [a [op b]]
            (with-source-info [:binop (first op) a b]
              (merge-info (source-info a) (source-info b))))
          left more))

(defn binop* [op]
  #(reduce (fn [a b] (with-source-info [:binop op a b]
                       (merge-info (source-info a) (source-info b)))) %))



;;; Our Python grammar

;; We need to declare forward references to some combinators to break mutual-recursion cycles
(def-forward
  &test &test-nocond &test-star-expr &exprlist &factor
  &comp-iter &comp-for &simple-statement &statement)

(def &testlist (&tuple-or-singleton &test))

;; The reason that keywords are test nodes instead of NAME is that using NAME
;; results in an ambiguity. ast.c makes sure it's a NAME.
(def &argument (&mod-expr &test (&or &comp-for (&prefixed :assign &test))
                          #(if (= (first %2) :assign)
                             [:keyarg % (second %2)]
                             [:generator % %2])))

(defn &argslist [defining typed]
  ;; NB: Like Python 3, unlike Python 2, we don't allow destructuring of arguments
  (let [&args (&non-empty-separated-list
               (if defining
                 (&info (&tag :argument &name
                              (if typed (&optional (&do &colon &test)) &nil)
                              (&optional (&do (&type :assign) &test))))
                 &argument))
        &rarg (if defining &name &test)]
    (&let [[positional-args more]
           (&or (&vector &args &optional-comma)
                (&return [nil true]))
           [rest-arg yetmore]
           (if more (&or (&vector (&do (&type :mul) &rarg) &optional-comma)
                         (&return [nil more]))
               (&return [nil nil]))
           [more-args stillmore]
           (if (and rest-arg yetmore)
             (&or (&vector &args &optional-comma)
                  (&return [nil yetmore]))
             (&return [nil nil]))
           keyword-arg
           (if stillmore (&optional (&do (&type :pow) &rarg)) &nil)
           _ (if (and (nil? keyword-arg) (vector? stillmore)) &fail &nil)]
          [(vec positional-args) rest-arg (vec more-args) keyword-arg])))

;; Note: these correspond to _optional_ [*argslist] in the python grammar.
(def &varargslist (&argslist true false))
(def &typed-args-list (&argslist true true))
(def &arglist (&argslist false false))

(def &slice-op (&do &colon (&optional &test)))
(def &subscript (&let [x (&optional &test)
                       * (let [&s (&into [:slice x] &slice-op (&optional &slice-op))]
                           (if x (&or &s (&return x)) &s))]))
(def &subscriptlist (&tuple-or-singleton &subscript))
(defn &comprehension [kind m]
  (&leti [x m
          f (&optional &comp-for)
          [l c] (if f &nil (&vector (&list (&do &comma m)) &optional-comma))]
         (cond f [({:tuple :generator, :list :list-comp, :dict :dict-comp, :set :set-comp} kind)
                  x (vec f)]
               (and (empty? l) (nil? c) (= kind :tuple)) x
               :else (vec* kind x l))))

(def &yield-expr (&do (&type :yield)
                      (&or (&do (&type :from) (&tag :yield-from &test))
                           (&tag :yield (&optional &testlist)))))

(def &dict-pair (&info (&tag :tuple &test (&do &colon &test)))) ;; :dict-pair would be redundant

(def &atom (&or (&paren (&or &yield-expr (&comprehension :tuple &test-star-expr) (&return :zero-uple)))
                (&paren \[ \] (&or (&comprehension :list &test-star-expr) (&return :empty-list)))
                (&paren \{ \} (&or (&comprehension :dict &dict-pair)
                                   (&comprehension :set &test)
                                   (&return :empty-dict)))
                &name (&type-if #{:integer :float :imaginary :ellipsis :None :True :False})
                (&let [x (&non-empty-list (&type-if #{:string :bytes}))]
                      (merge-strings x))))
(def &trailer (&info (&or (&tag :call (&paren &arglist))
                          (&tag :subscript (&paren \[ \] &subscriptlist))
                          (&tag :attribute (&do &dot &name)))))
(def &atom-trailer
  (&let [a &atom t (&list &trailer)]
        (reduce (fn [x [tag & args :as o]]
                  (with-source-info (vec* tag x args)
                    (merge-info (source-info x) (source-info o))))
                a t)))
(def &power (&mod-expr &atom-trailer (&do (&type :pow) &factor) #(vector :binop :pow % %2)))
(def &factor (&unary-op-expr {:add :pos, :sub :neg, :invert :invert} &power))
(def &term (&multi-op-expr (&type-if #{:mul :matmul :div :mod :floordiv}) &factor binop))
(def &arith-expr (&multi-op-expr (&type-if #{:add :sub}) &term binop))
(def &shift-expr (&multi-op-expr (&type-if #{:lshift :rshift}) &arith-expr binop))
(def &and-expr (&op-expr :and_ &shift-expr (binop* :and_)))
(def &xor-expr (&op-expr :xor &and-expr (binop* :xor)))
(def &expr (&op-expr :or_ &xor-expr (binop* :or_)))
(def &star-expr (&info (&do (&type :mul) (&tag :starred &expr))))
(def &comp-op (&or (&info (&do (&type :not) (&type :in) (&tag :not-in)))
                   (&info (&do (&type :is) (&type :not) (&tag :is_not)))
                   (&type-if #{:lt :gt :eq :ge :le :ne :in :is})))
(def &comparison (&multi-op-expr &comp-op &expr
                                 #(do [:compare % (vec (map first %2)) (vec (map second %2))])))
(def &not-test (&unary-op-expr #{:not} &comparison))
(def &and-test (&op-expr :and &not-test #(into [:boolop :and] %)))
(def &or-test (&op-expr :or &and-test #(into [:boolop :or] %)))
(defn &lambdef0 [m] (&prefixed :lambda &varargslist (&do &colon m)))
(def &lambdef (&lambdef0 &test))
(def &lambdef-nocond (&lambdef0 &test-nocond))
(def &test-nocond (&or &or-test &lambdef-nocond))
(def &test (&or (&mod-expr &or-test (&vector (&do (&type :if) &or-test) (&do (&type :else) &test))
                           (fn [t [x f]] [:if-expr x t f]))
                &lambdef))

(def &exprlist (&tuple-or-singleton (&or &expr &star-expr)))
(def &comp-for (&call cons (&info (&do (&type :for)
                                       (&tag :comp-for &exprlist (&do (&type :in) &or-test))))
                      (&optional &comp-iter)))
(def &comp-if (&call cons (&info (&do (&type :if) (&tag :comp-if &test-nocond)))
                     (&optional &comp-iter)))
(def &comp-iter (&or &comp-for &comp-if))

(def &suite
  (&or &simple-statement
       (&let [_ (&do &newline (&type :indent))
              l (&non-empty-list &statement)
              _ (&type :dedent)]
             (vec* :suite l))))
(def &colon-suite (&do &colon &suite))
(def &else (&optional (&do (&type :else) &colon-suite)))

(def &function-definition
  (&prefixed
   :def &name
   (&paren &typed-args-list)
   (&optional (&do (&type :rarrow) &test)) ;; PEP 3107 type annotations: return-type
   &colon-suite))
(def &class-definition
  (&prefixed :class &name (&optional (&paren &arglist)) &colon-suite))

(def &augassign
  (&type-if #{:iadd :isub :imul :ifloordiv :imod :iand :ior :ixor :irshift :ilshift :ipow}))

(def &dotted-name (&info (&tag* :dotted-name (&non-empty-separated-list &name &dot))))
(def &import-as-name (&vector &name (&optional (&do (&type :as) &name))))
(def &dotted-as-name (&vector &dotted-name (&optional (&do (&type :as) &name))))
(def &import-as-names (&non-empty-maybe-terminated-list &import-as-name))
(def &dotted-as-names (&non-empty-separated-list &dotted-as-name))

(def &test-star-expr (&or &test &star-expr))
(def &testlist-star-expr (&tuple-or-singleton &test-star-expr))

(def &expr-statement
  (&leti [x &testlist-star-expr
          l (&or (&vector &augassign (&or &yield-expr &testlist))
                 (&non-empty-list (&do (&type :assign) (&or &yield-expr &testlist-star-expr)))
                 &nil)]
         (cond (nil? l) x
               (vector? l) [:augassign x (first l) (second l)]
               :else (let [v (vec (cons x l))] [:assign (pop v) (last v)]))))

;; The python grammar specifies exprlist, but we interpret it here as a &n-e-m-t-list,
;; whereas we interpret &exprlist as a &tuple-or-singleton
(def &del-statement (&prefixed* :del (&non-empty-maybe-terminated-list (&or &expr &star-expr))))

(def &global-statement (&prefixed* :global (&non-empty-separated-list &name)))
(def &nonlocal-statement (&prefixed* :nonlocal (&non-empty-separated-list &name)))
(def &assert-statement (&prefixed :assert &test (&optional (&do &comma &test))))
(def &pass-statement (&type :pass))
(def &break-statement (&type :break))
(def &continue-statement (&type :continue))
(def &return-statement (&prefixed :return (&optional &testlist)))
(def &yield-statement &yield-expr)
(def &raise-statement
  (&prefixed* :raise (&optional (&vector &test (&optional (&do (&type :from) &test))))))
(def &flow-statement
  (&or &break-statement &continue-statement &return-statement &raise-statement &yield-statement))

(def &import-name (&prefixed* :import &dotted-as-names))
(def &import-from
  (&prefixed
   :from
   (&or (&vector &dots0 &dotted-name) (&vector &dots &nil))
   (&do (&type :import)
        (&or (&leti [_ (&type :mul)] [:all])
             (&paren &import-as-names)
             &import-as-names))))
(def &import-statement (&or &import-name &import-from))

(def &small-statement
  (&or &expr-statement &del-statement &pass-statement &flow-statement
       &import-statement &global-statement &nonlocal-statement &assert-statement))

(def &simple-statement
  (&leti [l (&non-empty-maybe-terminated-list &small-statement (&type :semicolon)) _ &newline]
         (if (empty? (rest l)) (first l) (into [:suite] l))))

(def &if-statement
  (&do (&type :if)
       (&tag :cond (&vec (&non-empty-separated-list (&vector &test &colon-suite) (&type :elif)))
             &else)))

(def &while-statement (&prefixed :while &test &colon-suite &else))

(def &for-statement
  (&prefixed :for &exprlist (&do (&type :in) &testlist) &colon-suite &else))

;; NB compile.c makes sure that the default except clause is last
(def &except-clause ;; Differs from Python 2
  (&leti [_ (&type :except)
          [ex-type target] (&optional (&vector &test (&optional (&do (&type :as) &name))))
          body &colon-suite]
         [:except ex-type target body]))

(def &try-statement
  (&let [_ (&type :try)
         body &colon-suite
         [excepts else] (&optional (&vector (&non-empty-list &except-clause) &else))
         finally (let [m (&do (&type :finally) &colon-suite)] (if excepts (&optional m) m))]
        [:try body (vec excepts) else finally]))

(def &with-item (&vector &test (&optional (&do (&type :as) &expr))))

(def &with-statement
  (&prefixed :with (&vec (&non-empty-separated-list &with-item)) &colon-suite))

(def &decorator
  (&do (&type :matmul) (&do1 (&tag :decorator &dotted-name (&optional (&paren &arglist))) &newline)))

(def &definition
  (&let [decorators (&list &decorator)
         definition (&info (&or &class-definition &function-definition))]
        (copy-source-info (conj definition (vec decorators)) definition)))

(def &compound-statement
  (&or &if-statement &while-statement &for-statement &try-statement &with-statement &definition))

(def &statement
  (&or &compound-statement &simple-statement))


;; Start symbols for the grammar:
;;   &single-input is a single interactive statement;
;;   &file-input is a module or sequence of commands read from an input file;
;;   &eval-input is the input for the eval() and input() functions.

(def &single-input
  (&info (&tag :interactive
               (&or (&do &newline (&tag :pass)) &simple-statement (&do1 &compound-statement &newline)))))

(def &file-input
  (&info (&do (&repeat &newline)
              (&do1 (&tag* :module (&list (&do1 &statement (&repeat &newline))))
                    (&type :endmarker)))))

(def &eval-input
  (&info (&do1 (&tag :expression &testlist)
               (&repeat &newline) (&type :endmarker))))

(defn mkParserState [lexed]
  (let [[[_ _ [file _ _]]] lexed]
    (->ParserState lexed [file [0 0] [0 0]])))

(defn parse
  ([lexed] (parse &file-input lexed))
  ([parser lexed] (-> lexed mkParserState parser first)))
