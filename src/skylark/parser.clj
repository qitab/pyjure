(ns skylark.parser
  (:require [clojure.string :as str])
  (:use [clojure.core.match :only [match]]
        [skylark.utilities]
        [skylark.parsing]))

;; This is largely based on the Python 3 grammar
;; Python 3 grammar: https://docs.python.org/3.5/reference/grammar.html
;;
;; For reference, see also the
;; Python 2 grammar: https://docs.python.org/2/reference/grammar.html
;; Our output is NOT based on the Python AST, but informed by it
;; Output AST: https://docs.python.org/2/library/ast.html

;; This parser is written using the parsing monad from parsing.clj, with
;; type State = InputStream×LastTokenInfo
;;
;; The State is what input remains unconsumed from the lexer output,
;; a sequence of tokens of the form [type data info],
;; where info is of the form [file [start-line start-column] [end-line end-column]]


;;; Basic monad constructors
(defrecord ParserState [in prev-info]) ; our State

(def fail-msg "python parser failure")

(defmethod fail-message skylark.parser.ParserState [σ] fail-msg)
(defmethod prev-info skylark.parser.ParserState [σ] (:prev-info σ))
(defmethod next-info skylark.parser.ParserState [σ]
  (match (:in σ)
         [[_ _ info]] info
         _ (let [[file _ end] (:prev-info σ)] [file end end])))


;;; Parsing tokens and location information

(defn &token [{[[_ _ info :as tok] & rest] :in}] [tok (->ParserState rest info)])
(defn &type-if [pred]
  (fn [{[[type _ info :as tok] & rest] :in :as σ}]
    (if (pred type) [tok (->ParserState rest info)] (&fail σ))))
(defn &type [t] (&type-if #(= % t)))


;;; Parsing utilities

(def &comma (&type :comma))
(def &optional-comma (&optional &comma))
(def &dot (&type :dot))
(def &dots0 (&fold (&type-if #{:dot :ellipsis}) #(+ % ({:dot 1 :ellipsis 3} (first %2))) 0))
(def &dots (&let [x &dots0 _ (if (= x 0) &fail &nil)] x))
(def &colon (&type :colon))
(def &name (&type :id))

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
  (&leti [[[type data _ :as x] :as l] (&non-empty-separated-list m)
          s &optional-comma]
         (if (and (empty? (rest l)) (nil? s)) x [:tuple l info&])))

(defn &paren
  ([m] (&paren \( \) m))
  ([opener closer m] (&let [_ (&type opener) x m _ (&type closer)] x)))

(def &newline (&type :newline))

(defn &prefixed [prefix m]
  (&letx [_ (&type prefix) x m] [prefix x]))

(defn &prefixed-vector [prefix & ms]
  (&prefixed prefix (apply &vector-f ms)))

(defn &tag [tag m] (&letx [x m] [tag x]))

(defn &mod-expr [m modifier f]
  (&leti [x m mod (&optional modifier)] (if mod (conj (f x mod) info&) x)))

(defn &op-expr [op m]
  (&mod-expr m (&non-empty-list (&do (&type op) m)) #(do [op (cons % %2)])))

(defn &multi-op-expr [tag op m]
  (&mod-expr m (&non-empty-list (&vector op m)) #(do [tag [% %2]])))

(defn &unary-op-expr [opmap m]
  (&leti [l (&list (&type-if opmap)) x m]
         (reduce (fn [[_ _ i2 :as x] [op _ i1]] [(opmap op) x (merge-info i1 i2)]) x l)))

(defn merge-strings [ss]
  (assert (not (empty? ss)))
  (let [[[b s1 i1 :as fs] & rs] ss]
    (loop [sl (list s1) i2 i1 r rs]
      (if (empty? r)
        [b ((if (= b :bytes) join-bytes str/join) (reverse sl)) (merge-info i1 i2)]
        (let [[[b2 s2 i2] r2] r]
          (assert (= b b2))
          (recur (cons s2 sl) i2 r2))))))


;;; Our Python grammar

;; We need to declare forward references to some combinators to break mutual-recursion cycles
(def-forward
  &test &test-nocond &test-star-expr &exprlist &factor
  &comp-iter &comp-for &simple-statement &statement)

(def &testlist (&tuple-or-singleton &test))

(defn &argslist [arg rarg defaults]
  ;; NB: Like Python 3, unlike Python 2, we don't allow destructuring of arguments
  (let [&args (if defaults
                (&non-empty-separated-list (&vector arg (&optional (&do (&type :assign) &test))))
                (&non-empty-separated-list arg))]
    (&let [[positional-args more]
           (&or (&vector &args &optional-comma)
                (&return [nil true]))
           [rest-arg yetmore]
           (if more (&or (&vector (&do (&type :mul) rarg) &optional-comma)
                         (&return [nil more]))
               (&return [nil nil]))
           [more-args stillmore]
           (if (and rest-arg yetmore)
             (&or (&vector &args &optional-comma)
                  (&return [nil yetmore]))
             (&return [nil nil]))
           keyword-arg
           (if stillmore (&optional (&do (&type :pow) rarg)) &nil)
           _ (if (and (nil? keyword-arg) (vector? stillmore)) &fail &nil)]
          [positional-args rest-arg more-args keyword-arg])))

;; Note: these correspond to _optional_ [*argslist] in the python grammar.
(def &varargslist (&argslist &name &name true))
(def &typed-arg (&vector &name (&optional (&do &colon &test))))
(def &typed-args-list (&argslist &typed-arg &typed-arg true))

;; The reason that keywords are test nodes instead of NAME is that using NAME
;; results in an ambiguity. ast.c makes sure it's a NAME.
(def &argument (&mod-expr &test (&or &comp-for (&prefixed :assign &test))
                          #(if (= (first %2) :assign)
                             [:assign [% (second %2)]]
                             [:comprehension [:argument % %2]])))
(def &arglist (&argslist &argument &test false))

(def &slice-op (&do &colon (&optional &test)))
(def &subscript (&or &test
                     (&letx [x (&optional &test)
                             y (&do &colon (&optional &test))
                             z (&optional &slice-op)]
                            [:slice [x y z]])))
(def &subscriptlist (&non-empty-maybe-terminated-list &subscript))
(defn &comprehension [kind m]
  (&letx [x m
          f (&optional &comp-for)
          [l c] (if f &nil (&vector (&list (&do &comma m)) &optional-comma))]
         (cond f [:comprehension [kind x f]]
               (and (empty? l) (nil? c) (= kind :tuple)) [:identity x]
               :else [kind (cons x l)])))

(def &yield-expr (&prefixed :yield
                            (&optional (&or (&do (&type :from) (&tag :subiterator &test))
                                            &testlist))))
(def &atom (&or (&paren (&or &yield-expr (&comprehension :tuple &test-star-expr) (&return :zero-uple)))
                (&paren \[ \] (&or (&comprehension :list &test-star-expr) (&return :empty-list)))
                (&paren \{ \} (&or (&comprehension :dict (&vector &test (&do &colon &test)))
                                   (&comprehension :set &test)
                                   (&return :empty-dict)))
                &name (&type-if #{:integer :float :imaginary :ellipsis :None :True :False})
                (&let [x (&non-empty-list (&type-if #{:string :bytes}))]
                      (merge-strings x))))
(def &trailer (&or (&letx [x (&paren &arglist)] [:call x])
                   (&letx [x (&paren \[ \] &subscriptlist)] [:subscript x])
                   (&letx [x (&do &dot &name)] [:select x])))
(def &atom-trailer
  (&let [a &atom t (&list &trailer)]
        (reduce (fn [[_ _ ix :as x] [tag y iy]] [tag [x y] (merge-info ix iy)]) a t)))
(def &power (&mod-expr &atom-trailer (&do (&type :pow) &factor) #(do [:pow [% %2]])))
(def &factor (&unary-op-expr {:add :pos, :sub :neg, :invert :invert} &power))
(def &term (&multi-op-expr :term (&type-if #{:mul :matmul :div :mod :floordiv}) &factor))
(def &arith-expr (&multi-op-expr :arith-expr (&type-if #{:add :sub}) &term))
(def &shift-expr (&multi-op-expr :shift-expr (&type-if #{:lshift :rshift}) &arith-expr))
(def &and-expr (&op-expr :and_ &shift-expr))
(def &xor-expr (&op-expr :xor &and-expr))
(def &expr (&op-expr :or_ &xor-expr))
(def &star-expr (&letx [_ (&type :mul) x &expr] [:star x]))
(def &comp-op (&or (&letx [_ (&type :not) _ (&type :in)] [:not-in nil])
                   (&letx [_ (&type :is) _ (&type :not)] [:is_not nil])
                   (&type-if #{:lt :gt :eq :ge :le :ne :in :is})))
(def &comparison (&multi-op-expr :comparison &comp-op &expr))
(def &not-test (&unary-op-expr #{:not} &comparison))
(def &and-test (&op-expr :and &not-test))
(def &or-test (&op-expr :or &and-test))
(defn &lambdef0 [m]
  (&prefixed-vector 'lambda &varargslist (&do &colon m)))
(def &lambdef (&lambdef0 &test))
(def &lambdef-nocond (&lambdef0 &test-nocond))
(def &test-nocond (&or &or-test &lambdef-nocond))
(def &test
  (&or (&mod-expr &or-test (&vector (&do (&type :if) &or-test) (&do (&type :else) &test))
                  #(do [:if [(first %2) % (second %2)]]))
       &lambdef))

(def &exprlist (&tuple-or-singleton (&or &expr &star-expr)))
(def &comp-for (&letx [_ (&type :for) x &exprlist _ (&type :in) y &or-test z (&optional &comp-iter)]
                      [:comp-for [x y z]]))
(def &comp-if (&letx [_ (&type :if) x &test-nocond y (&optional &comp-iter)] [:comp-if [x y]]))
(def &comp-iter (&or &comp-for &comp-if))

(def &suite
  (&or &simple-statement
       (&do &newline (&type :indent) (&do1 (&tag :progn (&non-empty-list &statement)) (&type :dedent)))))
(def &colon-suite (&do &colon &suite))

(def &function-definition
  (&prefixed-vector
   :def &name
   (&paren &typed-args-list)
   (&optional (&do (&type :rarrow) &test)) ;; PEP 3107 type annotations: return-type
   &colon-suite))
(def &class-definition ;; Python grammar says testlist, but this is not a tuple-or-singleton
  (&prefixed-vector :class &name (&optional (&paren (&maybe-terminated-list &test))) &colon-suite))

(def &augassign
  (&type-if #{:iadd :isub :imul :ifloordiv :imod :iand :ior :ixor :irshift :ilshift :ipow}))

(def &dotted-name (&non-empty-separated-list &name &dot))
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
               (vector? l) [:augassign-expr [x (first l) (second l)] info&]
               :else [:assign-expr (cons x l) info&])))

;; The python grammar specifies exprlist, but we interpret it here as a &n-e-m-t-list,
;; whereas we interpret &exprlist as a &tuple-or-singleton
(def &del-statement (&prefixed :del (&non-empty-maybe-terminated-list (&or &expr &star-expr))))

(def &global-statement (&prefixed :global (&non-empty-separated-list &name)))
(def &nonlocal-statement (&prefixed :nonlocal (&non-empty-separated-list &name)))
(def &assert-statement (&prefixed-vector :assert &test (&optional (&do &comma &test))))
(def &pass-statement (&type :pass))
(def &break-statement (&type :break))
(def &continue-statement (&type :continue))
(def &return-statement (&prefixed :return (&optional &testlist)))
(def &yield-statement &yield-expr)
(def &raise-statement
  (&prefixed :raise (&optional (&vector &test (&optional (&do (&type :from) &test))))))
(def &flow-statement
  (&or &break-statement &continue-statement &return-statement &raise-statement &yield-statement))

(def &import-name (&prefixed :import &dotted-as-names))
(def &import-from
  (&prefixed-vector
   :from
   (&or (&vector &dots0 &dotted-name) (&vector &dots &nil))
   (&do (&type :import)
        (&or (&type :mul) (&paren &import-as-names) &import-as-names))))
(def &import-statement (&or &import-name &import-from))

(def &small-statement
  (&or &expr-statement &del-statement &pass-statement &flow-statement
       &import-statement &global-statement &nonlocal-statement &assert-statement))

(def &simple-statement
  (&leti [l (&non-empty-maybe-terminated-list &small-statement (&type :semicolon)) _ &newline]
         (if (empty? (rest l)) (first l) [:progn l info&])))

(def &if-statement
  (&prefixed-vector :if
                    (&non-empty-separated-list (&vector &test &colon-suite) (&type :elif))
                    (&optional (&do (&type :else) &colon-suite))))

(def &while-statement
  (&prefixed-vector :while &test &colon-suite (&optional (&do (&type :else) &colon-suite))))

(def &for-statement
  (&prefixed-vector :for &exprlist (&do (&type :in) &testlist) &colon-suite
                    (&optional (&do (&type :else) &colon-suite))))

;; NB compile.c makes sure that the default except clause is last
(def &except-clause
  ;; Differs from Python 2
  (&prefixed :except (&optional (&vector &test (&optional (&do (&type :as) &name))))))

(def &try-statement
  (&prefixed-vector :try &colon-suite
                    (&or (&vector
                          (&non-empty-list (&vector &except-clause &colon-suite))
                          (&optional (&do (&type :else) &colon-suite))
                          (&optional (&do (&type :finally) &colon-suite)))
                         (&vector &nil &nil (&do (&type :finally) &colon-suite)))))

(def &with-item (&vector &test (&optional (&do (&type :as) &expr))))

(def &with-statement
  (&prefixed-vector :with (&non-empty-separated-list &with-item) &colon-suite))

(def &decorator
  (&letx [_ (&type :matmul)
          name &dotted-name
          args (&optional (&paren &arglist))
          _ &newline]
         [:decorator [name args]]))

(def &definition
  (&leti [decorators (&list &decorator)
          [defkind data info] (&or &class-definition &function-definition)]
         [defkind (conj data decorators) info&]))

(def &compound-statement
  (&or &if-statement &while-statement &for-statement &try-statement &with-statement &definition))

(def &statement
  (&or &compound-statement &simple-statement))


;; Start symbols for the grammar:
;;   &single-input is a single interactive statement;
;;   &file-input is a module or sequence of commands read from an input file;
;;   &eval-input is the input for the eval() and input() functions.

(def &single-input
  (&letx [x (&or &newline &simple-statement (&do1 &compound-statement &newline))]
         [:Interactive x]))

(def &file-input
  (&letx [_ (&repeat &newline)
          x (&list (&do1 &statement (&repeat &newline)))
          _ (&type :endmarker)]
         [:Module x]))

(def &eval-input
  (&letx [x &testlist
          _ (&repeat &newline)
          _ (&type :endmarker)]
        [:Expression x]))

(defn mkParserState [lexed]
  (let [[[_ _ [file _ _]]] lexed]
    (->ParserState lexed [file [0 0] [0 0]])))

(defn parse
  ([lexed] (parse &file-input lexed))
  ([parser lexed] (-> lexed mkParserState parser first)))

