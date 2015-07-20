(ns pyjure.desugar
  (:use [pyjure.debug]
        [pyjure.passes]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.environment]
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
;; TODO: we might want to preserve cond instead of if, because we want to avoid chaining
;; of administrative reductions for chaining of slightly different monads when nesting cases of ifs.
;; Or is there a more general way of doing the reductions at compile-time?
;;
;; * decorators are expanded away
;; * contiguous function definitions "def f1(s1): b1\ndef f2(s2): b2..." transformed in
;;     defn [<<f1(s1) b1; return None>>, <<f2(s2) b2; return None>> ...]
;; * lambdas args: value become letfn [<<lambda (args): return value>>]
;; * lambda's also become a letfn with a name of $lambda.
;; * special form :dump for dumping the current environment, useful for toplevel and for objects.
;; * comprehensions are transformed into functions that yield
;; * augassign are transformed into assignments
;; * destructuring-binds are transformed into individual bindings
;; * comparisons are expanded into individual comparisons
;; * multibranch if is replaced by two-branch ifs.
;; * boolean operations are expanded into simple ifs
;; * nested suites are merged; pass is eliminated
;;
;; Being done:
;; * transform lambda in defn or a gensym then reference.
;;
;; Not yet(?):
;; ? mutual recursion within sub defs??? We want mutually recursive letfn, not let (fn ...) !!!
;; ? A module is not just a suite, after all: global bindings are happening!
;; ? maintain context attributes to functions into which decorators may store information,
;;   to e.g. enable yield continuations, rule definition side-effects, etc.
;; ? in a normal def context, group consecutive function definitions in a letfn;
;;   defer non-macro decorator expansion after that. In a module or class context,
;;   that's different. Ouch.
;; ? d[k] = v ===> d += {k:v}
;; ? del d[k] ===> d = $dissoc(d, k)
;; ? further pass: A-normal form: only use constants and variables as arguments, not more complex expressions
;; ? a module, if interactive, will print the result of each toplevel evaluation
;; ? handle meta-data and source-info more like Racket


;; A macro-environment maps lists of symbols (as in dotted names) to macros
;; :output-suite list of statements in the suite being created (when in desugar-suite mode).
;; :input-suite statements remaining in the current suite
;; :more-suites list of continuations of statement suites to desugar (?)
;; :bindings a map from strings to compile-time-bindings.
;; :parent a parent environment for lookups

(declare &desugar &desugar* &desugar-args)

(defrecord DesugarEnvironment [environment suites])

(defn initial-desugar-environment []
  (->DesugarEnvironment (initial-compile-time-environment) []))



(defn name? [n]
  "Is n a qualified pyjure name? i.e. a non-empty vector of strings"
  (and (vector? n) (every? string? n) (pos? (count n))))

(defn namify [x]
  {:post ((or (name? %) (nil? %)))}
  "return a qualified pyjure name given an identifier or dotted name
or identifier qualified by a series of attribute names"
  (match [x]
    [[:id n]] [n]
    [[:dotted-name & ns]] (vec (map second ns))
    [[:attribute y [:id n]]] (if-let [yn (namify y)] (conj yn n))
    :else nil))

(defn unnamify [n x]
  "given a qualified pyjure name n and a object x for its source-information,
return an expanded expression to compute said name"
  {:pre (name? n)}
  (letfn [(v [& args] (copy-source-info x (vec args)))]
    (if (<= 2 (count n))
      (v :attribute (unnamify (pop n) x) (v :id (last n)))
      (v :id (first n)))))

(defn &macro [kind n]
  (match [(namify n)]
    [([name & more] :seq)]
    (fn [E]
      (let [CTE (:environment E)
            [env _ binding] (compile-time-effect CTE name kind n)
            E1 (assoc-in E [:environment] env)]
        (loop [binding binding more more]
          (if (seq more)
            (if (and (ctb-flags? binding :constant) (instance? (:value binding) pyjure.environment.CompileTimeScope))
              (recur (:value binding) (rest more))
              [nil E1])
            (if (ctb-flags? binding :macro)
              [(:value binding) env]
              [nil E1])))))
    :else (&return nil)))

(defn &maybe-expand-macro [form getter else]
  (&bind getter (fn [mac] (if mac (&bind #(mac form %) &desugar) (else)))))


(def gensym-counter (atom -1))
(defn $gensym
  ([] ($gensym "g"))
  ([x] [:id (str x "-" (swap! gensym-counter inc))]))
(defmacro with-gensyms [gensyms & body]
  `(let ~(vec (mapcat list gensyms (map $gensym gensyms))) ~@body))

;; TODO: monadic treatment of environment?

(defn constint [n] [:constant [:integer (+ 0N n)]])

(defn expand-target [x right kind i]
  ;; TODO: handle += and such by being more like a client to CL's get-setf-expansion, etc.
  (letfn [(c [x] (copy-source-info i x))
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
  (letfn [(i [a] (copy-source-info x a))
          (v [& args] (i (vec args)))]
    (if-let [[[op] & moreops] ops]
      (let [[arg & moreargs] args
            [right init] (if moreops (with-gensyms [g] [g [(v :bind g arg)]]) [arg nil])]
        (i `[:suite ~@init ~(v :if (merge-source-info left right [:builtin op left right])
                               (expand-compare right moreops moreargs x)
                               (v :constant (v :False)))]))
      (v :constant (v :True)))))

(defn expand-cond [clauses else x]
  (letfn [(v [& a] (copy-source-info x (vec a)))]
    (if-let [[[test iftrue] & moreclauses] clauses]
      (v :if (v :builtin :truth test) iftrue (expand-cond moreclauses else x))
      (or else (v :constant (v :None))))))

(defn &expand-decorators [x base]
  (let [[kind decorators name & args] x]
    (if (seq decorators)
      (let [[[_ deco dargs :as d] & more] decorators]
        (&maybe-expand-macro
         x (&macro :decorator-referenced deco)
         #(letfn [(v [& a] (copy-source-info d (vec a)))]
            (&desugar
             (v :suite (copy-source-info x (assoc x 1 more))
                (v :bind name
                   (v :call (let [deco (unnamify (namify deco) deco)]
                              (if dargs (v :call deco dargs) deco))
                      [[name] nil [] nil])))))))
      (base))))

(defn cons-suite [x head suite]
  (copy-source-info x
   (match [head]
     [nil] suite
     [[:suite & body]] (if (empty? body) suite
                           (cons-suite x (first body)
                                       (cons-suite x (vec* :suite (rest body)) suite)))
     :else (match [suite]
             [nil] head
             [[:suite & body]] (vec* :suite head body)
             :else [:suite head suite]))))

(defn make-suite [x list]
  (reduce #(cons-suite x %2 %1) nil (reverse list)))

(defn make-defn [name args return-type body suite x]
  (match [suite]
    [[:defn defs]]
    (copy-source-info x [:defn (vec* [name args return-type body] defs)])
    [[:suite [:defn defs] & more]]
    (cons-suite x (vec* :defn [name args return-type body] defs) more)
    :else
    (cons-suite x [:defn [name args return-type body]] suite)))

(defn &push-suite
  ([] (&push-suite []))
  ([s] (fn [E] (let [r (:suites E)] [nil (assoc E :suites (cons s r))]))))

(defn &pop-suite [E] (let [s (:suites E)] [(first s) (assoc E :suites (rest s))]))
(defn &flush-suite [E] (let [s (:suites E)] [(first s) (assoc E :suites (cons [] (rest s)))]))

(defn &desugar-suite
  ([x xs] (fn [E]
            ((&desugar-suite x)
             (update-in E [:suites] (fn [[s & k]] (cons (concat xs s) k))))))
  ([x] (fn [E]
         (let [[s & k] (:suites E)]
           (if (empty? s)
             [nil E]
             (let [[f & r] s]
               ((&let [a (&desugar f)
                       d (&desugar-suite x)]
                      (cons-suite x a d))
                (assoc-in E [:suites] (cons r k)))))))))

(defn uncomprehend [x]
  (fn [statement comp]
    (copy-source-info
     comp
     (match [comp]
       [[':comp-for target generator]] [:for target generator statement nil]
       [[':comp-if test]] [:if-expr test statement nil]
       :else ($syntax-error x "Not a valid comprehension %s"
                            [:expr :comp] {:expr x :comp comp})))))

(defn &desugar [x]
  (letfn [(i [f] (copy-source-info x f))
          (v [& s] (i (vec s)))
          (w [& s] (i (apply vec* s)))]
    (match [x]
      [nil] &nil

      [[(:or ':unbind ':constant) & _]] (&return x) ;; these two are for recursive desugaring.

      [[(:or ':integer ':float ':string ':bytes ':imaginary
             ':True ':False ':None ':Ellipsis
             ':zero-uple ':empty-list ':empty-dict) & _]] (&return (v :constant x))

      [[':id name]] ;; TODO: handle lexical bindings in macro environment
      (&maybe-expand-macro
       x (&macro :referenced x)
       #(&return x))

      [[':call fun args]]
      (&maybe-expand-macro
       x (&macro :call-referenced fun)
       #(&let [fun (&desugar fun)
               args (&desugar-args args)]
              (v :call fun args)))

      ;; XXXXX HERE IS THE ACTION XXXXX
      [[':def decorators name args return-type body]]
      (&expand-decorators x
       #(&let [args (&desugar-args args)
               return-type (&desugar return-type)
               body (&desugar body)]
              (v :bind name (v :function args return-type (v :suite body (v :constant (v :None)))))))

      [[':from [dots dottedname] imports]] (do (NFN :from) (&return x)) ;; (NIY {:r "&desugar from"})

      [[':import & dotted-as-names]] (do (NFN :import) (&return x)) ;; TODO: process import bindings

      [[(:or ':expression ':interactive) x]] (&desugar x)

      [[':pass]] (&desugar (v :None)) ;; distinguish from an empty :suite, so it can break letfn

      [[':suite & xs]] (&desugar-suite x xs)

      [[':module & xs]] ;; keep it special, but delegate to suite
      (&let [s (&desugar-suite x xs)] (v :module s))

      ;; :builtin is for recursively desugared code.
      ;; :lt :gt :eq :ge :le :ne :in :is :not-in :is_not are transformed into builtin's as well.
      [[(:or ':builtin ':binop ':unaryop ':handler-bind) op & args]] ;; handler-bind has a var name, not op
      (&let [as (&desugar* args)] (w :builtin op as))

      [[':del _ _ & _]] (&desugar (w :suite (map #(v :del %) (rest x))))
      [[':del [:id n] :as i]] (&return (v :unbind i)) ;; del identifier remains as primitive
      [[':del [':subscript obj idx]]] (&return (v :builtin :delitem obj idx))
      [[':del _]] ($syntax-error x "Not a valid thing to del-ete %s")

      [[tag :guard #{:bind :argument :except :raise :unwind-protect
                     :suite :while :break :continue :if :yield :yield-from :all} & args]]
      (&let [s (&desugar* args)] (w tag s))

      [[tag :guard #{:return :assert :list :dict :set :tuple :slice :subscript} & args]]
      (&let [s (&desugar* args)] (w :builtin tag s))

      [[':if-expr test body else]]
      (&let [[test body else] (&desugar* [test body (or else (v :None))])]
            (v :if (v :builtin :truth test) body else))

      [[':for target generator body else]]
      (with-gensyms [gen]
        (&desugar
         (v :suite
            (v :bind gen generator)
            (v :while (v :builtin :gen-next? gen)
               (v :suite
                  (v :bind target (v :builtin :gen-first gen))
                  (v :bind gen (v :builtin :gen-rest gen))
                  body
                  (v :continue))
               else))))

      [[tag :guard #{:global :nonlocal} & args]]
      (if (seq (rest args)) (&desugar (w :suite (map #(v tag %) args))) (&return x))

      [[':attribute expr [:id s] :as n]]
      (&let [x (&desugar expr)]
            (v :builtin :attribute x
               (letfn [(c [& x] (copy-source-info n (vec x)))]
                 (c :constant (c :string s)))))

      [[':compare left ops args]] (&desugar (expand-compare left ops args x))

      [[':cond clauses else]] (&desugar (expand-cond clauses else x))

      [[':lambda args body]] (with-gensyms [fn] (&desugar (v :suite (v :def [] fn args nil body) fn)))

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
             ;; uncomprehend was factored out of here, because the nested match causes AOT to produce overlong filenames
             (reduce (uncomprehend x) (v :yield expr) gen))))

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
                                          (letfn [(vv [& a] (copy-source-info xx (vec a)))]
                                            [(vv :builtin :isinstance type ex)
                                             (vv :unwind-protect
                                                 (vv :suite
                                                     (if target (vv :bind target ex) (vv :suite))
                                                     body)
                                                 (vv :suite
                                                     (if target (vv :unbind target) (vv :suite))
                                                     (vv :unbind ex)))]))
                                        clauses))
                              else)))))]
         (if finally (v :unwind-protect handled finally) handled)))

      [[':class decorators name args body]]
      (&expand-decorators x ;; TODO? recursively add a @method decorator to all function definitions?
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
          (letfn [(c [& a] (copy-source-info ctxmgr (vec a)))]
            (&maybe-expand-macro
             x (&macro :with ctxmgr)
             #(with-gensyms [mgr exception-type exception traceback]
                (&desugar
                 (c :suite
                    (c :bind mgr ctxmgr)
                    (c :bind exception-type (c :None))
                    (c :bind exception (c :None))
                    (c :bind traceback (c :None))
                    (let [enter (c :call (c :attribute mgr (c :id "__enter__"))
                                   [[] nil [] nil])]
                      (if target (c :assign [target] enter) enter))
                    (c :try simpler
                       [(c :except nil nil
                           (c :assign [(c :tuple exception-type exception traceback)]
                              (c :builtin :exc-info)))]
                       nil
                       (c :suite
                          (c :call (c :attribute mgr (c :id "__exit__"))
                             [[exception-type exception traceback] nil [] nil])
                          (c :del exception-type exception traceback))))))))))

      :else ($syntax-error x "Unrecognized form %s"))))

(defn &desugar* [xs] (&map &desugar xs))
(def &desugar-args (&args &desugar))


(defn desugar- [program]
  (let [[x E] ((&desugar program) (initial-desugar-environment))]
    x))
