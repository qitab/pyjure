(ns skylark.desugar
  (:use [skylark.utilities]
        [skylark.parsing]
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


;; A macro-environment maps lists of symbols (as in dotted names) to macros
(def null-macro-environment {})

(defn namify [x]
  (match [x]
    [[:id n]] [n]
    [[:dotted-name & ns]] (map second ns)
    [[:attribute y [:id n]]] (if-let [yn (namify y)] (conj yn n))))

(defn unnamify [n x]
  {:pre ((vector? n) (pos? (count n)))}
  (letfn [(v [& args] (copy-source-info (vec args) x))]
    (if (<= 2 (count n))
      (v :attribute (unnamify (pop n) x) (v :id (last n)))
      (v :id (first n)))))

(defn &macro [kind n]
  (fn [E] (if-let [m (E (namify n))] (m kind))))
(defn &call-macro [n E]
  (&let [m (&macro :call n)] (when m (fn [x] (fn [E] (m x E))))))
(defn &decorator-macro [n E]
  (&let [m (&macro :decorator n)] (when m (fn [args x] (fn [E] (m args x E))))))
(defn &with-macro [n E]
  (&let [m (&macro :with n)] (when m (fn [args x] (fn [E] (m args x E))))))

(def gensym-counter (atom -1))
(defn $gensym
  ([] ($gensym "g"))
  ([x] [:id (str x "-" (swap! gensym-counter inc))]))
(defmacro with-gensyms [gensyms & body]
  `(let ~(vec (mapcat list gensyms (map $gensym gensyms))) ~@body))

;; TODO: monadic treatment of environment?

(declare &desugar)

(defn &desugar* [xs] (&map &desugar xs))
(defn &desugar** [xs] (&map &desugar* xs))

(defn &desugar-args [[args sarg moreargs kwarg]]
  (&let [args (&desugar* args)
         sarg (&desugar sarg)
         moreargs (&desugar* moreargs)
         kwarg (&desugar kwarg)]
        [(vec args) sarg (vec moreargs) kwarg]))

(defn constint [n] [:constant [:integer (+ 0N n)]])

;; NB: DO THAT IN A FURTHER PASS, NOT THIS ONE
(defn &expand-suite [x]
  (letfn [(flatten [x acc]
            (if (and (vector? x) (= (first x) :suite))
              (reduce #(flatten %2 %) acc x)
              (conj acc x)))]
    (&let [s (&desugar* (rest x))]
          (if (and (seq s) (nil? (rest s))) (first s)
              (copy-source-info (vec* :suite s) x)))))


(defn expand-target [x right kind i]
  (letfn [(c [x] (copy-source-info x i))
          (v [& args] (c (vec args)))
          (D [x right]
            (if-let [[h & as] (and (vector? x) x)]
              (case h
                (:id) (v :assign x right)
                (:attribute) ($syntax-error x "Attribute as a target is impure")
                (:subscript) ($syntax-error x "Subscript as a target is impure")
                (:tuple :list)
                (if (not (= kind :assign)) ($error "list or tuple not allowed as target in augassign")
                    (with-gensyms [r nr]
                      (let [si (first (map first (filter #(= (first (second %)) :starred)
                                                         (map-indexed vector as))))
                            a (vec as)]
                        (v :suite
                           (v :assign r right)
                           (if si
                             (let [head (subvec a 0 si)
                                   nhead (count head)
                                   starred (second (nth a si))
                                   tail (subvec a (inc si))
                                   ntail (count tail)]
                               (v :suite
                                  (v :builtin :check-length-ge r
                                     (v :constant (v :integer (+ nhead ntail))))
                                  (v :assign nr (v :builtin :length r))
                                  (Dhead head r)
                                  (D starred (v :builtin :subscript r
                                                (v :builtin :slice
                                                   (constint nhead)
                                                   (v :builtin :sub nr (constint ntail)) (constint 1))))
                                  (Dtail tail r nr ntail)))
                             (v :suite
                                (v :builtin :check-length-eq r (constint (count as)))
                                (Dhead as r)))))))
                ($syntax-error x "Syntax Error: invalid target"))
              ($syntax-error "Syntax Error: invalid target")))
          (Dhead [head r]
            (c (vec* :suite
                     (map-indexed (fn [i x] (v :assign x (v :builtin :subscript r (constint i)))) head))))
          (Dtail [tail r nr ntail]
            (c (vec* :suite
                     (map-indexed (fn [i x] (v :assign x (v :builtin :subscript r
                                                            (v :builtin :sub nr (constint (- ntail i))))))
                                  tail))))]
    (D x right)))

(defn expand-compare [left ops args x]
  (letfn [(i [a] (copy-source-info a x))
          (v [& args] (i (vec args)))]
    (if-let [[op & moreops] ops]
      (let [[arg & moreargs] args
            [right init] (if moreops (with-gensyms [g] [g [(v :assign g arg)]]) [arg nil])]
        (i `(:suite ~@init ~(v :if (merge-source-info [:builtin op left right] left right)
                               (expand-compare right moreops moreargs x)
                               (v :constant :False)))))
      (v :constant :True))))

(defn expand-cond [clauses else x]
  (if-let [[[test iftrue] & moreclauses] clauses]
    (copy-source-info [:if test iftrue (expand-cond moreclauses else x)] x)
    (or else (copy-source-info [:constant :None] x))))

(defn &undecorate [x base]
  (let [decorators (last x)]
    (if (seq decorators)
      (let [[_ deco dargs :as d] (last decorators)
            simpler (copy-source-info (conj (pop x) (pop decorators)))]
        (letfn [(v [&args a] (copy-source-info (vec a) d))]
          (&let [f (&decorator-macro deco)
                 * (if f (&bind (f dargs simpler) &desugar)
                       (&desugar
                        (v :suite simpler
                           (v :assign name
                              (v :call (let [deco (unnamify deco)]
                                         (if dargs (v d :call deco dargs) deco))
                                 [[name] nil [] nil])))))])))
      (base))))

(defn &desugar [x]
  (letfn [(i [f] (copy-source-info f x))
          (v [& s] (i (vec s)))
          (w [& s] (i (apply vec* s)))]
    (match [x]
      [nil] &nil
      [[':from [dots dottedname] imports]] (NIY {:r "&desugar from"})
      [[':import & dotted-as-names]] (&return x) ;; TODO: process import bindings
      [[(:or ':id ;; TODO: handle lexical bindings in macro environment
             ':return ':constant) & _]] (&return x) ;; :constant is for recursive desugar
      [[(:or ':integer ':float ':string ':bytes ':imaginary
             ':True ':False ':None ':Ellipsis
             ':zero-uple ':empty-list ':empty-dict) & _]] (&return (v :constant x))
      [[(:or ':expression ':interactive) x]] (&desugar x)
      [[(:or ':module ':pass) & xs]] (&desugar (w :suite xs))
      ;; :builtin is for recursively desugared code.
      ;; :lt :gt :eq :ge :le :ne :in :is :not-in :is_not are transformed into builtin's as well.
      [[(:or ':builtin ':binop ':unaryop) op & args]]
      (&let [as (&desugar* args)] (w :builtin op as))
      [[':del _ _ & _]] (&desugar (w :suite (map #(v :del %) (rest x))))
      [[':del [:id n]]] (&return x) ;; del identifier remains as primitive
      [[':del _]] (NIY {:r "Can only del names" :x x})
      [[(:or ':raise ':while ':break ':continue ':if) & args]]
      (&let [s (&desugar* args)] (w (first x) s))
      [[(:or ':suite ':assert ':yield ':yield-from
              ':argument ':except ;; TODO: handle
              ':list ':dict ':set ':tuple ':slice ':subscript) & args]]
      (&let [s (&desugar* args)] (w :builtin (first x) s))
      [[':for target generator body]]
      (with-gensyms [gen]
        (&desugar
         (w :suite
            (v :assign gen generator)
            (v :while (v :builtin :gen-next? gen)
               (w :suite
                  (v :assign (v :tuple gen target) (v :builtin :gen-next gen))
                  body)))))
      [[(:or ':global ':nonlocal) & args]]
      (if (seq (rest args)) (&desugar (w :suite (map #(v (first x) %) args))) (&return x))
      [[':attribute x [:id s] :as n]]
      (&let [x (&desugar x)] (v :builtin :attribute x (copy-source-info [:string s] n)))
      [[':compare left ops args]] (&desugar (expand-compare left ops args x))
      [[':cond clauses else]] (&desugar (expand-cond clauses else x))
      [[':augassign target iop arg]]
      (&desugar (expand-target target (v :builtin iop target arg) :augassign x))
      [[':assign targets expr]]
      (&let [val (&desugar expr)
             z (match [targets]
                 [[[':id _]]] (&return (v :assign (first targets) val))
                 [[_]] (&return (expand-target (first targets) val))
                 :else (with-gensyms [g]
                         (&let [y (&desugar
                                   (w :suite
                                      (map #(expand-target % g :assign x) (reverse targets))))]
                               (v :suite (v :assign g val) y))))])
      [[(:or ':list-comp ':dict-comp ':set-comp ':generator) expr gen]]
      (&desugar
       (v :builtin (first x)
          (v :function [[] nil [] nil] nil
             (reduce (fn [statement comp]
                       (copy-source-info
                        (match [comp]
                               [[':comp-for target gen]] [:for target gen statement]
                               [[':comp-if test]] [:if test statement (v :None)]
                               :else ($syntax-error x)) comp))
                     (v :yield expr)))))
      [[':try body excepts else finally]]
      (&let [body (&desugar body)
             excepts (&desugar* excepts) ;;TODO: bindings
             else (&desugar else)
             finally (&desugar finally)]
            (v :try body excepts else finally))
      [[':call fun args]]
      (&let [f (&call-macro fun)
             z (if f (&bind (f x) &desugar)
                   (&let [fun (&desugar fun)
                          args (&desugar-args args)]
                         (v :call fun args)))])
      [[':def name args return-type body decorators]]
      (&undecorate x
       #(&let [args (&desugar-args args)
               return-type (&desugar return-type)
               body (&desugar body)] ;; TODO: desugar in lexical environment
              (v :assign name (v :function args return-type (v :suite body (v :None))))))
      [[':class name args body decorators]]
      (&undecorate x ;; TODO? recursively add a @method decorator to all function definitions.
       #(&let [args (&desugar-args args)
               body (&desugar body)]
              (v :class name args body)))
      [[':function args return-type body]]
      (&let [args (&desugar-args args)
             return-type (&desugar return-type)
             body (&desugar body)]
            (v :function args return-type body))
      [[':with items body]]
      (match items
        [[]] (&desugar body)
        [[[ctxmgr target] & more]]
        (let [simpler [v :with more body]]
          (letfn [(c [&args a] (copy-source-info (vec a) ctxmgr))]
            (&let [f (&with-macro ctxmgr)
                   * (if f (&bind (f simpler) &desugar)
                         (with-gensyms [mgr exception-type exception traceback]
                           (&desugar
                            (c :suite
                               (c :assign mgr ctxmgr)
                               (c :assign exception-type :None)
                               (c :assign exception :None)
                               (c :assign traceback :None)
                               (let [enter (c :call (c :attribute mgr (c :id "__enter__"))
                                              [[] nil [] nil])]
                                 (if target (c :assign target enter) enter))
                               (c :try simpler []
                                  (c :suite (c :assign (c :tuple exception-type exception traceback)
                                               (c :builtin :exc_info)))
                                  (c :call (c :attribute mgr (c :id "__exit__"))
                                     [[exception-type exception traceback] nil [] nil])
                                  (c :del exception-type exception traceback))))))]))))
      :else ($syntax-error x))))

(defn desugar [program] ((&desugar program) null-macro-environment))
