(ns skylark.desugar
  (:use [skylark.utilities]
        [skylark.parsing]
        [clojure.core.match :only [match]]))

;; macroexpand python syntax into a somewhat simpler language:
;; Only:
;;     lambda-calculus and variable bindings
;;           :function :call :id :bind :unbind :nonlocal :global :import :from :keyarg
;;           :builtin (all calls to core operators) :constant
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

(defn name? [n] (and (vector? n) (every? string? n) (pos? (count n))))

(defn namify [x]
  {:post ((or (name? %) (nil? %)))}
  (match [x]
    [[:id n]] [n]
    [[:dotted-name & ns]] (vec (map second ns))
    [[:attribute y [:id n]]] (if-let [yn (namify y)] (conj yn n))
    :else nil))

(defn unnamify [n x]
  {:pre (name? n)}
  (letfn [(v [& args] (copy-source-info (vec args) x))]
    (if (<= 2 (count n))
      (v :attribute (unnamify (pop n) x) (v :id (last n)))
      (v :id (first n)))))

(defn &macro [kind n]
  (fn [E] [(if-let [n (namify n)] (if-let [m (E n)] (m kind))) E]))
(defn &call-macro [n]
  (&let [m (&macro :call n)] (when m (fn [x] (fn [E] (m x E))))))
(defn &decorator-macro [n]
  (&let [m (&macro :decorator n)] (when m (fn [args x] (fn [E] (m args x E))))))
(defn &with-macro [x]
  (let [[n args] (match [x] [[':call x args]] [x args] :else [x nil])]
    (&let [m (&macro :with n)] (when m (fn [args x] (fn [E] (m args x E)))))))

(def gensym-counter (atom -1))
(defn $gensym
  ([] ($gensym "g"))
  ([x] [:id (str x "-" (swap! gensym-counter inc))]))
(defmacro with-gensyms [gensyms & body]
  `(let ~(vec (mapcat list gensyms (map $gensym gensyms))) ~@body))

;; TODO: monadic treatment of environment?

(declare &desugar &desugar* &desugar-args)

(defn constint [n] [:constant [:integer (+ 0N n)]])

(defn expand-target [x right kind i]
  (letfn [(c [x] (copy-source-info x i))
          (v [& args] (c (vec args)))
          (D [x right]
            (if-let [[h & as] (and (vector? x) x)]
              (case h
                (:id) (v :bind x right)
                (:attribute) ($syntax-error x "Attribute as a target is impure %s")
                (:subscript) ($syntax-error x "Subscript as a target is impure %s")
                (:tuple :list)
                (if (not (= kind :assign))
                  ($syntax-error x "list or tuple not allowed as target in augassign %s")
                  (with-gensyms [r nr]
                    (let [si (first (map first (filter #(= (first (second %)) :starred)
                                                       (map-indexed vector as))))
                          a (vec as)]
                      (v :suite
                         (v :bind r right)
                         (if si
                           (let [head (subvec a 0 si)
                                 nhead (count head)
                                 starred (second (nth a si))
                                 tail (subvec a (inc si))
                                 ntail (count tail)]
                             (v :suite
                                (v :builtin :check-length-ge r
                                   (v :constant (v :integer (+ nhead ntail))))
                                (v :bind nr (v :builtin :length r))
                                (Dhead head r)
                                (D starred (v :builtin :subscript r
                                              (v :builtin :slice
                                                 (constint nhead)
                                                 (v :builtin :sub nr (constint ntail)) (constint 1))))
                                (Dtail tail r nr ntail)))
                           (v :suite
                              (v :builtin :check-length-eq r (constint (count as)))
                              (Dhead as r)))))))
                ($syntax-error x "Syntax Error: invalid target %s"))
              ($syntax-error x "Syntax Error: invalid target %s")))
          (Dhead [head r]
            (c (vec* :suite
                     (map-indexed (fn [i x] (v :bind x (v :builtin :subscript r (constint i)))) head))))
          (Dtail [tail r nr ntail]
            (c (vec* :suite
                     (map-indexed (fn [i x] (v :bind x (v :builtin :subscript r
                                                            (v :builtin :sub nr (constint (- ntail i))))))
                                  tail))))]
    (D x right)))

(defn expand-compare [left ops args x]
  (letfn [(i [a] (copy-source-info a x))
          (v [& args] (i (vec args)))]
    (if-let [[[op] & moreops] ops]
      (let [[arg & moreargs] args
            [right init] (if moreops (with-gensyms [g] [g [(v :bind g arg)]]) [arg nil])]
        (i `[:suite ~@init ~(v :if (merge-source-info [:builtin op left right] left right)
                               (expand-compare right moreops moreargs x)
                               (v :constant :False))]))
      (v :constant :True))))

(defn expand-cond [clauses else x]
  (letfn [(v [& a] (copy-source-info (vec a) x))]
    (if-let [[[test iftrue] & moreclauses] clauses]
      (v :if (v :builtin :truthy test) iftrue (expand-cond moreclauses else x))
      (or else (v :constant :None)))))

(defn expand-decorator [x base]
  (let [decorators (last x)
        name (second x)]
    (if (seq decorators)
      (let [[_ deco dargs :as d] (last decorators)
            simpler (copy-source-info (conj (pop x) (pop decorators)) x)]
        (letfn [(v [& a] (copy-source-info (vec a) d))]
          (&let [f (&decorator-macro deco)
                 * (if f (&bind (f dargs simpler) &desugar)
                       (&desugar
                        (v :suite simpler
                           (v :bind name
                              (v :call (let [deco (unnamify (namify deco) deco)]
                                         (if dargs (v :call deco dargs) deco))
                                 [[name] nil [] nil])))))])))
      (base))))

(defn &desugar [x]
  (letfn [(i [f] (copy-source-info f x))
          (v [& s] (i (vec s)))
          (w [& s] (i (apply vec* s)))]
    (match [x]
      [nil] &nil
      [[':from [dots dottedname] imports]] (do (NFN) (&return x)) ;; (NIY {:r "&desugar from"})
      [[':import & dotted-as-names]] (&return x) ;; TODO: process import bindings
      [[(:or ':id ;; TODO: handle lexical bindings in macro environment
             ':unbind ':constant ;; these two are for recursive desugaring.
             ':return) & _]] (&return x)
      [[(:or ':integer ':float ':string ':bytes ':imaginary
             ':True ':False ':None ':Ellipsis
             ':zero-uple ':empty-list ':empty-dict) & _]] (&return (v :constant x))
      [[(:or ':expression ':interactive) x]] (&desugar x)
      [[(:or ':module ':pass) & xs]] (&desugar (w :suite xs))
      ;; :builtin is for recursively desugared code.
      ;; :lt :gt :eq :ge :le :ne :in :is :not-in :is_not are transformed into builtin's as well.
      [[(:or ':builtin ':binop ':unaryop ':handler-bind) op & args]] ;; handler-bind has a var name, not op
      (&let [as (&desugar* args)] (w :builtin op as))
      [[':del _ _ & _]] (&desugar (w :suite (map #(v :del %) (rest x))))
      [[':del [:id n]]] (&return (v :unbind n)) ;; del identifier remains as primitive
      [[':del [':subscript obj idx]]]
      (&return (v :builtin :delitem obj idx))
      [[':del _]] ($syntax-error x "Not a valid thing to del-ete %s")
      [[tag :guard #{:bind :argument :except :raise :unwind-protect
                     :suite :while :break :continue :if :yield :yield-from :all} & args]]
      (&let [s (&desugar* args)] (w tag s))
      [[tag :guard #{:assert :list :dict :set :tuple :slice :subscript} & args]]
      (&let [s (&desugar* args)] (w :builtin tag s))
      [[':if-expr test body else]]
      (&let [[test body else] (&desugar* [test body (or else (v :None))])]
            (v :if (v :builtin :truthy test) body else))
      [[':for target generator body else]]
      (with-gensyms [gen]
        (&desugar
         (v :suite
            (v :bind gen generator)
            (v :while (v :builtin :gen-next? gen)
               (v :suite
                  (v :assign [(v :tuple gen target)] (v :builtin :gen-next gen))
                  body))
            (or else (v :suite)))))
      [[tag :guard #{:global :nonlocal} & args]]
      (if (seq (rest args)) (&desugar (w :suite (map #(v tag %) args))) (&return x))
      [[':attribute expr [:id s] :as n]]
      (&let [x (&desugar expr)]
            (v :builtin :attribute x (copy-source-info [:string s] n)))
      [[':compare left ops args]] (&desugar (expand-compare left ops args x))
      [[':cond clauses else]] (&desugar (expand-cond clauses else x))
      [[':lambda args body]] (&desugar (v :function args nil body))
      [[':augassign target iop arg]]
      (&desugar (expand-target target (v :builtin iop target arg) :augassign x))
      [[':assign targets expr]]
      (&let [val (&desugar expr)
             z (match [targets]
                 [[[':id _] :as target]] (&return (v :bind target val))
                 [[target]] (&return (expand-target target val :assign x))
                 :else (with-gensyms [g]
                         (&let [y (&desugar
                                   (w :suite
                                      (map #(expand-target % g :assign x) (reverse targets))))]
                               (v :suite (v :bind g val) y))))])
      [[tag :guard #{:list-comp :dict-comp :set-comp :generator} expr gen]]
      (&desugar
       (v :builtin tag
          (v :function [[] nil [] nil] nil
             (reduce (fn [statement comp]
                       (copy-source-info
                        (match [comp]
                               [[':comp-for target gen]] [:for target gen statement]
                               [[':comp-if test]] [:if-expr test statement nil]
                               :else ($syntax-error x "Not a valid comprehension %s")) comp))
                     (v :yield expr)))))
      [[':try body excepts else finally]]
      (&desugar
       (let [handled
             (cond
              (empty? excepts) (do (assert (nil? else)) body)
              (not (every? first (pop excepts)))
              ($syntax-error x "expression-less except statement must be in last position")
              :else (with-gensyms [ex]
                      (let [[clauses else] (if (nil? (second (last excepts)))
                                             [(pop excepts) (nth (last excepts) 3)]
                                             [excepts (v :raise ex)])]
                        (v :handler-bind
                           ex
                           body
                           (v :cond
                              (vec (map (fn [[_ type target body :as xx]]
                                          (letfn [(vv [& a] (copy-source-info (vec a) xx))]
                                            [(vv :builtin :isinstance type ex)
                                             (vv :unwind-protect
                                                 (vv :suite (vv :bind target ex) body)
                                                 (vv :unbind ex))]))
                                        clauses))
                              else)))))]
         (if finally (v :unwind-protect handled finally) handled)))
      [[':call fun args]]
      (&let [f (&call-macro fun)
             z (if f (&bind (f x) &desugar)
                   (&let [fun (&desugar fun)
                          args (&desugar-args args)]
                         (v :call fun args)))])
      [[':def name args return-type body decorators]]
      (expand-decorator x
       #(&let [args (&desugar-args args)
               return-type (&desugar return-type)
               body (&desugar body)] ;; TODO: desugar in lexical environment
              (v :bind name (v :function args return-type (v :suite body (v :None))))))
      [[':class name args body decorators]]
      (expand-decorator x ;; TODO? recursively add a @method decorator to all function definitions?
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
        [] (&desugar body)
        [[ctxmgr target] & more]
        (let [simpler (v :with more body)]
          (letfn [(c [& a] (copy-source-info (vec a) ctxmgr))]
            (&let [f (&with-macro ctxmgr)
                   * (if f (&bind (f simpler) &desugar)
                         (with-gensyms [mgr exception-type exception traceback]
                           (&desugar
                            (c :suite
                               (c :bind mgr ctxmgr)
                               (c :bind exception-type (c :None))
                               (c :bind exception (c :None))
                               (c :bind traceback (c :None))
                               (let [enter (c :call (c :attribute mgr (c :id "__enter__"))
                                              [[] nil [] nil])]
                                 (if target (c :bind target enter) enter))
                               (c :try simpler
                                  [(c :except nil nil
                                      (c :assign [(c :tuple exception-type exception traceback)]
                                         (c :builtin :exc-info)))]
                                  nil
                                  (c :suite
                                     (c :call (c :attribute mgr (c :id "__exit__"))
                                        [[exception-type exception traceback] nil [] nil])
                                     (c :del exception-type exception traceback)))))))]))))
      :else ($syntax-error x "Unrecognized form %s"))))

(defn &desugar* [xs] (&map &desugar xs))
(def &desugar-args (&args &desugar))

(defn desugar [program] ((&desugar program) null-macro-environment))
