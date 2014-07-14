(ns skylark.parser
  (:require [skylark.lexer :as lex])
  (:use [skylark.lexer :refer [&return &bind &do &do1]])
  (:require [skylark.semantics :as s])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [clojure.algo.monads])
  (:use [clojure.core.match :only [match]]))

;; This is largely based on the Python 3 grammar
;; Python 3 grammar: https://docs.python.org/3.5/reference/grammar.html
;;
;; For reference, see also the
;; Python 2 grammar: https://docs.python.org/2/reference/grammar.html
;; Our output is NOT based on the Python AST, but informed by it
;; Output AST: https://docs.python.org/2/library/ast.html

;; This parser is written in monadic style.
;; type State = InputStream×LastTokenInfo
;; monad PythonParser α = State → α×State
;;
;; The State is what input remains unconsumed from the lexer output,
;; a sequence of tokens of the form [type data info],
;; where info is of the form [file [start-line start-column] [end-line end-column]]
;;
;; Monadic parser entities have the & prefix.

;; TODO: modify the monad so it records the many choices at any given point,
;; and the choices at the furthest point reach, so that
;; so that (1) fail doesn't use exceptions, and (2) the final error is at the furthest point.


;;; Basic monad constructors
(defrecord Σ [in prev-info]) ; our State

(def fail-msg "python parser failure")
(defn fail
  ([] (fail {}))
  ([details] (throw (clojure.lang.ExceptionInfo. fail-msg details))))
(defn &error
  ([] (&error {}))
  ([details] (fn [σ] (fail (conj details [:info (:prev-info σ)])))))
(def &fail (&error))

(defn &or* [ls]
  (fn [σ]
    (loop [l ls]
      (if (empty? l) ((&error) σ)
          (or (try-parse (first l) σ)
              (recur (rest l)))))))
(defn &or [& ls] (&or* ls))

(defmonad parser-m
  [ m-result &return
    m-bind &bind
    m-zero (&error)
    m-plus &or ])
(defmacro &let
  ([bindings]
     `(&let ~bindings ;; return the last result that is not _ or a keyword
            ~(lex/find-if #(not (or (= % '_) (keyword? %))) (reverse (map first (partition 2 bindings))))))
  ([bindings result] `(domonad parser-m ~bindings ~result)))

;;; Monadic combinators

(defn try-parse [f σ]
  (try (f σ)
       (catch clojure.lang.ExceptionInfo x
         (when-not (= (.getMessage x) fail-msg) (throw x)))))
(defn &not [l] ;; lookahead that l does not appear here
  (fn [σ] (if (try-parse l σ) (fail) [nil σ])))
(def &nil (&return nil))
(defn &lift-f
  ([fun] (&return (fun)))
  ([fun m] (&bind m (fn [x] (&return (fun x)))))
  ([fun m & ms] (apply &lift-f (fn [x] (partial fun x)) ms)))
(defn &vector-f [& ms] (apply &lift-f ms))
(defmacro &lift [fun & ms]
  (let [vars (map #(do % (gensym)) ms)
        bindings (into [] (mapcat list vars ms))]
  `(&let ~bindings (~fun ~@vars))))
(defmacro &vector [& ms] `(&lift vector ~@ms))
(defn &optional [m] (&or m &nil))
(defn &fold [m f a]
  (&or (&bind m #(&fold m f (f a %))) (&return a)))
(defn &list
  ([m] (&list m ()))
  ([m a] (&let [r (&fold m conj a)] (reverse r))))
(defn &non-empty-list [m] (&bind m (fn [a] (&list m (list a)))))
(defn &repeat [m] (&fold m (constantly nil) nil))


;;; Parsing tokens and location information

(defn &token [{[[_ _ info :as tok] & rest] :in}] [tok (->Σ rest info)])
(defn &type-if [pred]
  (fn [{[[type _ info :as tok] & rest] :in}] (if (pred type) [tok (->Σ rest info)] (fail))))
(defn &type [t] (&type-if #(= % t)))
(defn &type-if-not [pred] (&type-if #(not (pred %))))

(defn &prev-info [σ] [(:prev-info σ) σ])
(defn &next-info [σ]
  [(match (:in σ)
          [[_ _ info]] info
          _ (let [[file _ end] (:prev-info σ)] [file end end]))
   σ])
(defn merge-info [[file start-pos _] [filetoo _ end-pos]]
  {:pre [(= file filetoo)]}
  [file start-pos end-pos])

(defmacro &leti [bindings value]
  ;; not hygienic: exposes bindings to start& end& info&
  `(&let ~(into `[~'start& &next-info] (into bindings `[~'end& &prev-info]))
         (let [~'info& (merge-info ~'start& ~'end&)] ~value)))
(defmacro &letx [bindings value]
  `(&leti ~bindings (let [~'v& ~value] (and ~'v& (conj ~'v& ~'info&)))))

(defn &info [m] (&letx [x m] x))


;;; Parsing utilities

(def &comma (&type 'comma))
(def &optional-comma (&optional (&type 'comma)))
(def &dot (&type 'dot))
(def &dots0 (&fold (&type-if '#{dot ellipsis}) #(+ % ('{dot 1 ellipsis 3} %2)) 0))
(def &dots (&let [x &dots _ (if (= x 0) &fail &nil)] x))
(def &colon (&type 'colon))
(def &name (&type :id))

(defn &non-empty-separated-list
  ([m] &non-empty-separated-list &comma)
  ([m separator] (&bind m (fn [a] (&list (&do separator m) (list a))))))

(defn &separated-list [m separator]
  (&or (&non-empty-separated-list m separator)
       (&return ())))

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
  (&letx [t (&or (&let [[type data _ :as x] (&non-empty-separated-list m)
                        s &optional-comma]
                       (if (and (not (empty? x)) (empty? (rest x)))
                         [:singleton x] ;; strip info, will be added by caller
                         [:tuple x]))
                 &nil)]
         t))

(def &NIY &fail)
(defn &paren
  ([m] (&paren \( \) m))
  ([opener closer m] (&let [_ (&type opener) x m _ (&type closer)] x)))

(def &newline (&type :newline))

(defn &prefixed [prefix m]
  (&letx [_ (&type prefix) x m] [prefix x]))

(defn &prefixed-vector [prefix & ms]
  (&prefixed prefix (apply &vector-f ms)))

(defn &mod-expr [m modifier f]
  (&leti [x m mod (&optional modifier)] (if mod (conj (f x mod) info&) x)))

(defn &op-expr [op m]
  (&mod-expr m (&non-empty-list (&do (&type op) m)) #(do [op (cons % %2)])))

(defn &multi-op-expr [tag op m]
  (&mod-expr m (&non-empty-list (&vector op m)) #(do [tag [% %2]])))

(defn &unary-op-expr [opmap m]
  (&leti [l (&list (&type-if opmap)) x m]
         (reduce (fn [[_ _ i2 :as x] [op _ i1]] [(opmap op) x (merge-info i1 i2)]) x l)))

;;; Our Pythonic grammar

;; We need to declare forward references.
(defmacro def-forward [& names]
  `(do ~@(map #(do `(defn ~% [~'σ] ((deref (ref '~%)) ~'σ))) names)))
(def-forward
  &test &test-nocond &test-star-expr &exprlist &factor
  &comp-iter &comp-for &simple-statement &statement)

(def &testlist (&non-empty-maybe-terminated-list &test))

(defn &argslist [arg defaults]
  ;; NB: Like Python 3, unlike Python 2, we don't allow destructuring of arguments
  (let [&args (if defaults
                (&non-empty-separated-list (&vector arg (&optional (&do (&type 'assign) &test))))
                (&non-empty-separated-list arg))]
    (&let [[positional-args more]
           (&or (&optional (&vector &args &optional-comma))
                (&return [nil true]))
           [rest-arg yetmore]
           (if more (&or (&optional (&vector (&do (&type 'mul) &name) &optional-comma))
                       (&return [nil more]))
               (&return [nil nil]))
           [more-args stillmore]
           (if (and rest-arg yetmore)
             (&or (&optional (&vector &args &optional-comma))
                  (&return [nil yetmore]))
             (&return [nil nil]))
           keyword-arg
           (if stillmore (&optional (&do (&type 'pow) &name)) &nil)
           _ (if (and (nil? keyword-arg) (= stillmore 'comma)) &fail &nil)]
          [positional-args rest-arg more-args keyword-arg])))

;; Note: these correspond to _optional_ [*argslist] in the python grammar.
(def &varargslist (&argslist &name true))
(def &typed-args-list (&argslist (&vector &name (&optional (&do &colon &test))) true))

(defn join-bytes [arrays]
  (let [sizes (map count arrays)
        sizes_r (vec (reductions + sizes))
        offsets (cons 0 (drop-last sizes_r))
        total (last sizes_r)
        out (byte-array total)]
    (dorun (map #(System/arraycopy %2 0 out %1 %3) offsets arrays sizes))
    out))

(defn merge-strings [ss]
  (assert (not (empty? ss)))
  (let [[[b s1 i1 :as fs] rs] ss]
    (loop [sl (list s1) i2 i1 r rs]
      (if (empty? r)
        [b ((if (= b :bytes) join-bytes str/join) (reverse sl)) (merge-info i1 i2)]
        (let [[[b2 s2 i2] r2] r]
          (assert (= b b2))
          (recur (cons s2 sl) i2 r2))))))

;; The reason that keywords are test nodes instead of NAME is that using NAME
;; results in an ambiguity. ast.c makes sure it's a NAME.
(def &argument (&or (&mod-expr &test &comp-for #(do ['comprehension ['argument % %2]]))
                    (&vector &test (&do (&type 'assign) &test)))) ;; Really [NAME '='] test
(def &arglist (&argslist &argument false))

(def &slice-op (&do &colon (&optional &test)))
(def &subscript (&or &test
                     (&letx [x (&optional &test)
                             y (&do &colon (&optional &test))
                             z (&optional &slice-op)]
                            ['slice [x y z]])))
(def &subscriptlist (&non-empty-maybe-terminated-list &subscript))
(defn &comprehension [kind m]
  (&letx [x m
          f (&optional &comp-for)
          [l c] (if f &nil (&vector (&list (&do &comma m)) &optional-comma))]
         (cond f ['comprehension [kind x f]]
               (or l c (not (= kind 'tuple))) [kind (cons x l)]
               :else ['identity x])))
(def &yield-expr (&prefixed 'yield (&optional (&or (&prefixed 'from &test) &testlist))))
(def &atom (&or (&paren (&or &yield-expr (&comprehension 'tuple &test-star-expr) (&return 'zero-uple)))
                (&paren \[ \] (&or (&comprehension 'list &test-star-expr) (&return 'empty-list)))
                (&paren \{ \} (&or (&comprehension 'dict (&vector &test (&do &colon &test)))
                                   (&comprehension 'set &test)
                                   (&return 'empty-dict)))
                &name (&type-if '#{:integer :float :imaginary ellipsis None True False})
                (&let [x (&non-empty-list (&type-if #{:string :bytes}))]
                      (merge-strings x))))
(def &trailer (&or (&letx [x (&paren &arglist)] ['call x])
                   (&letx [x (&paren \[ \] &subscriptlist)] ['subscript x])
                   (&letx [x (&do &dot &name)] ['select x])))
(def &atom-trailer
  (&let [a &atom t (&list &trailer)]
        (reduce (fn [[_ _ ix :as x] [tag y iy]] [tag [x y] (merge-info ix iy)]) a t)))
(def &power (&mod-expr &atom-trailer (&do (&type 'pow) &factor) #(do ['pow [% %2]])))
(def &factor (&unary-op-expr '{add pos, sub neg, invert invert} &power))
(def &term (&multi-op-expr 'term (&type-if '#{mul matmul div mod floordiv}) &factor))
(def &arith-expr (&multi-op-expr 'arith-expr (&type-if '#{add sub}) &term))
(def &shift-expr (&multi-op-expr 'shift-expr (&type-if '#{lshift rshift}) &arith-expr))
(def &and-expr (&op-expr 'and_ &shift-expr))
(def &xor-expr (&op-expr 'xor &and-expr))
(def &expr (&op-expr 'or_ &xor-expr))
(def &star-expr (&letx [_ (&type 'mul) x &expr] ['star x]))
(def &comp-op (&or (&letx [_ (&type 'not) _ (&type 'in)] ['not-in nil])
                   (&letx [_ (&type 'is) _ (&type 'not)] ['is_not nil])
                   (&type-if '#{lt gt eq ge le ne in is})))
(def &comparison (&multi-op-expr 'comparison &comp-op &expr))
(def &not-test (&unary-op-expr #{not} &comparison))
(def &and-test (&op-expr 'and &not-test))
(def &or-test (&op-expr 'or &and-test))
(defn &lambdef0 [m]
  (&prefixed-vector 'lambda &varargslist (&do &colon m)))
(def &lambdef (&lambdef0 &test))
(def &lambdef-nocond (&lambdef0 &test-nocond))
(def &test-nocond (&or &or-test &lambdef-nocond))
(def &test
  (&or (&mod-expr &or-test (&vector (&do (&type 'if) &or-test) (&do (&type 'else) &test))
                  #(do ['if [(first %2) % (second %2)]]))
       &lambdef))

(def &exprlist (&non-empty-maybe-terminated-list (&or &expr &star-expr)))
(def &comp-for (&letx [_ (&type 'for) x &exprlist _ (&type 'in) y &or-test z (&optional &comp-iter)]
                      ['comp-for [x y z]]))
(def &comp-if (&letx [_ (&type 'if) x &test-nocond y (&optional &comp-iter)] ['comp-if [x y]]))
(def &comp-iter (&or &comp-for &comp-if))

(def &suite
  (&or &simple-statement
       (&do &newline (&type :indent) (&do1 (&non-empty-list &statement) (&type :dedent)))))

(def &colon-suite (&do &colon &suite))

(def &function-definition
  (&prefixed-vector
   'def &name
   (&paren &typed-args-list)
   (&optional (&do (&type 'rarrow) &test)) ;; PEP 3107 type annotations: return-type
   &colon-suite))

(def &class-definition
  (&prefixed-vector 'class &name (&optional (&paren &arglist)) &colon-suite))

(def &augassign
  (&type-if '#{iadd isub imul ifloordiv imod iand ior ixor ixrshift ilshift ipow}))

(def &dotted-name (&non-empty-separated-list &name &dot))
(def &import-as-name (&vector &name (&optional (&do (&type 'as) &name))))
(def &dotted-as-name (&vector &dotted-name (&optional (&do (&type 'as) &name))))
(def &import-as-names (&non-empty-maybe-terminated-list &import-as-name))
(def &dotted-as-names (&non-empty-separated-list &dotted-as-name))

(def &global-name (&prefixed 'global (&non-empty-separated-list &name)))
(def &nonlocal-statement (&prefixed 'nonlocal (&non-empty-separated-list &name)))
(def &assert-statement (&prefixed-vector 'assert &test (&optional &test)))

(def &test-star-expr (&or &test &star-expr))
(def &testlist-star-expr (&non-empty-maybe-terminated-list &test-star-expr))

(def &expr-statement
  (&leti [x &testlist-star-expr
          l (&or (&vector &augassign (&or &yield-expr &testlist))
                 (&non-empty-list (&do (&type 'assign) (&or &yield-expr &testlist-star-expr)))
                 &nil)]
         (cond (nil? l) x
               (vector? l) ['augassign-expr [x (first l) (second l)] info&]
               :else ['assign-expr (cons x l) info&])))

(def &del-statement (&prefixed 'del &exprlist))

(def &pass-statement (&type 'pass))

(def &break-statement (&type 'break))

(def &continue-statement (&type 'continue))

(def &return-statement (&prefixed 'return (&optional &testlist)))

(def &yield-statement &yield-expr)

(def &raise-statement
  (&prefixed 'raise
             (&optional (&vector &test (&optional (&do (&type 'from) &test))))))

(def &flow-statement
  (&or &break-statement &continue-statement &return-statement &raise-statement &yield-statement))

(def &import-name (&prefixed 'import &dotted-as-names))

(def &import-from
  (&prefixed-vector
   'from
   (&or (&vector &dots0 &dotted-name) &dots)
   (&do (&type 'import)
        (&or (&type 'mul) (&paren &import-as-names) &import-as-names))))

(def &import-statement (&or &import-name &import-from))

(def &global-statement (&prefixed 'global (&non-empty-separated-list &name)))

(def &small-statement
  (&or &expr-statement &del-statement &pass-statement &flow-statement
       &import-statement &global-statement &nonlocal-statement &assert-statement))

(def &simple-statement
  (&do1 (&non-empty-maybe-terminated-list &small-statement (&type 'semicolon)) &newline))

(def &if-statement
  (&prefixed-vector 'if
                    (&non-empty-separated-list (&vector &test &colon-suite) (&type 'elif))
                    (&optional (&do (&type 'else) &colon-suite))))

(def &while-statement
  (&prefixed-vector 'while &test &colon-suite (&optional (&do (&type 'else) &colon-suite))))

(def &for-statement
  (&prefixed-vector 'for &exprlist (&do 'in &testlist) &colon-suite
                    (&optional (&do (&type 'else) &colon-suite))))

;; NB compile.c makes sure that the default except clause is last
(def &except-clause
  ;; Differs from Python 2
  (&prefixed 'except (&optional (&vector &test (&optional (&type 'as) &name)))))

(def &try-statement
  (&prefixed-vector 'try &colon-suite
                    (&or (&vector
                          (&non-empty-list (&vector &except-clause &colon-suite))
                          (&optional (&do (&type 'else) &colon-suite))
                          (&optional (&do (&type 'finally) &colon-suite)))
                         (&vector &nil &nil (&do (&type 'finally) &colon-suite)))))

(def &with-item (&vector &test (&optional (&do (&type 'as) &expr))))

(def &with-statement
  (&prefixed-vector 'with (&non-empty-separated-list &with-item) &colon-suite))

(def &decorator
  (&leti [_ (&type 'matmul)
          name &dotted-name
          args (&optional (&paren &arglist))
          _ &newline]
         [name args info&]))

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
  (&let [x (&or &newline &simple-statement (&do1 &compound-statement &newline))]
        ['Interactive x]))

(def &file-input
  (&letx [_ (&repeat &newline)
          x (&list (&do1 &statement (&repeat &newline)))
          _ (&type :endmarker)]
         ['Module x]))

(def &eval-input
  (&letx [x &testlist
          _ (&repeat &newline)
          _ (&type :endmarker)]
        ['Expression x]))

(defn parser-input [input]
  [(lex/python-lexer input) [*file* [0 0] [0 0]]])

(defn python-parser [input]
  (first (&file-input (parser-input input))))

