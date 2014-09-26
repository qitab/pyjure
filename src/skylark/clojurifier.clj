(ns skylark.clojurifier
  (:use [clojure.core.match :only [match]]
        [skylark.debug :exclude [clojurify]]
        [skylark.utilities]
        [skylark.parsing]
        [skylark.runtime]))

;; TODO? have a previous pass transform everything in A-normal form,
;; where all arguments to functions are trivial (constant, function, variable)?
;; TODO: transform every branch point into binding-passing style?

(defrecord Environment [level local outer global])
;; level: 0 for global, incremented when you descend into scope
;; local: map of names to getters, or nil if level is 0
;; outer: the next outer non-global environment, or nil if only global is next
;; global: the global environment

(declare &C &C* &Cargs)

(def reserved-ids ;; otherwise valid identifiers that need be escaped for the sake of Clojure
  #{"def" "if" "let" "do" "quote" "var" "fn" "loop" "recur" "throw" "try" "monitor-enter"
    "new"}) ;; also set! . &

(defn local-id [x] (symbol (if (reserved-ids x) (str "%" x) x)))
(defn &resolve-id [x] (fn [E] [(local-id x) E])) ;; TODO: distinguish global from local
(defn builtin-id [x] (symbol (str "$" (name x))))
(defn &resolve-constant [x]
  (fn [E]
    (DBG :rc x E)
    [(case (first x)
       (:integer :string :bytes) (second x)
       (builtin-id (first x))) E]))

(defn do-conj [a r] (match [r] [(['do & s] :seq)] `(~'do ~a ~@s) :else `(~'do ~a ~r)))

(defn &Csuite [x]
  (letfn [(&let-form [s n a b r]
            (&let [a (&C a) r (&Csuite r)]
                  (copy-source-info n
                                    (if (get-in (meta b) [:capturing :vars s])
                                      `(~'let [~(local-id s) ~a] ~r)
                                      (do-conj a r)))))]
    (match x
      [[:bind [:id s] :as n a] :as b & r] (&let-form s n a b r)
      [[:unbind [:id s] :as n] :as b & r] (&let-form s n nil b r)
      [] (&C [:constant [:None]])
      [a] (&C a)
      [a b & r] (&let [a (&C a) r (&Csuite (cons b r))] (do-conj a r)))))

(defn lookup-var [x E]
  ;; returns the FURAL status of the binding of x in E.
  ;; what if it's defined in an outer scope?
  true)

(defn &C [x]
  (letfn [(m [a] (copy-source-info x a))
          (&r [x] (&return (m x)))]
    (fn [E] (DBG :&C x E) (
      (match [x]
        [nil] &nil
        [[:id s]]
        (&let [sym (&resolve-id s)
               status (fn [E] [(lookup-var s E) E])
               info (&return (source-info x))
               * (do (DBG :&C-id sym status info)
                     (&r (case status
                           (:linear :required) sym
                           (nil false true :affine) `(~'$ensure-not-nil ~sym ~info))))])
        [[:suite & as]] (&Csuite as)
        [[:bind [:id s] :as n a]] (do (DBG :bind s a) (&C a)) ;; no suite to consume the binding
        [[:unbind [:id s]]] &nil
        [[:constant c]] (&resolve-constant c)
        [[:builtin b & as]]
        (&let [as (&C* as)] (m `(~(builtin-id b) ~@as)))
        [[:call f a]] (&let [f (&C f) a (&Cargs a) * (&r `(~'$call ~f ~a))])
        [[:module a]] (&C a) ;; TODO: handle update to global state
        :else (do (comment
        [[:argument id type default]]
        ;; handles argument in the *outer* scope where the function is defined,
        ;; *NOT* in the inner scope that the function defines (see :function for that)
        (&m (&into [:argument id] (&C type) (&C default)))
        [[:function args return-type body]]
        (&m (&let [args (&Cargs args) ; handle type and default
                   return-type (&C return-type)]
                  (let [[_ innerE] ((&map #(&assoc-in [:vars %] {:bound? true :locality :param})
                                          (args-vars args)) nil)
                        [body innerE] ((&C body) innerE)]
                    (M (check-effects x innerE true)
                       :function args return-type body))))
        [[:return a]] (&m (&tag :return (&C a)))
        [[(:or ':from ':import ':break ':continue) & _]] (&x)
        [[:unwind-protect body protection]]
        (&m (&tag :unwind-protect (&C body) (&C protection)))
        [[:handler-bind body handler]] (&m (&tag :handler-bind (&C body) (&C handler)))
        [[h :guard #{:yield :yield-from} a]]
        (&do (&assoc-in [:generator?] true) (&m (&tag h (&C a))))
        [[h :guard #{:suite :raise :while :if} & a]] (&m (&tag* h (&C* a)))
        [[:handler-bind [:id s] :as target body handler]]
        (&let [type (&C type)
               _ (&assoc-in [:vars s :bound?] true)
               body (&C body)]
              (v :except type target body))
        [[:class [:id s] :as name args body]]
        (&m (&let [args (&Cargs args)
                   _ (&assoc-in [:vars s :bound?] true)]
                  (let [[body innerE] ((&C body) nil)]
                    (if (:generator? innerE)
                      ($syntax-error x "invalid yield in class %a" [:name] {:name s})
                      (M (check-effects x innerE false) :class name args body)))))
        :else)
        ($syntax-error x "unexpected expression %s during clojure generation pass"))) E))))

(defn &C* [xs] (&map &C xs))
(def &Cargs (&args &C))

(defn clojurify [x]
  (let [[x E] ((&C x) nil)]
    x))

(comment
  "
def foo(a):
  x=1
  if a: x=2
  return x
"
  (defn foo [a] (let [x 1] (-> ($if a [2] [x]) (fn [[x]] x))))

  "
def foo(a):
  x=1;
  if a:
    return x
  elif bar(a):
    y = 3
  else:
    y = 4
    z = 5
  if y == 4:
    return z
"
  (defn foo [a]
    (let [x 1]
      ((fn [k] ($cond a x
                      ($call bar a) (k 3 nil false)
                      :else (k 4 5 true)))
       ;; no need to pass a witness for y: it is always bound in all branches that reach k
       ;; on the other hand, our analysis doesn't prove that z is unbound, so we check
       (fn [y z z?] ((fn [k] ($cond (= y 4) ($check-bound z? z) ;
                                    :else (k)))
                (fn [] (return $None))))))))
;; Further passes may show that some functions are used only once, and thus may be inlined,

