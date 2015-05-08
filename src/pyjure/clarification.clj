(ns pyjure.clarification
  (:use [clojure.core.match :only [match]]
        [pyjure.utilities]
        [pyjure.parsing]
        [pyjure.runtime]))

;; This pass will
;; * identify for each scope (function or class)
;;  and each variables which is its scoping status:
;;  one of param, local, nonlocal, global.
;;  Issue an error if a variable has conflicting referents.
;; * detect which functions are generators
;;  (a yield statement taints the function even if it's going to be optimized away later)

;; * TODO distinguish module, class and def contexts, and only allow yield and return in def context.
;; * TODO? if we allow for lexical macros, these passes must be merged.
;; * TODO? detect other syntactically present side-effects in the function
;;  (e.g. pyjure rule definition, incremental class definition, etc.)
;;
;; Invariant: if the pass returns without an error, only metadata is changed.
;;
;; Information is stored in metadata for the function or class node.
;; further passes must be careful to NOT blindly copy that meta-data,
;; but only the meta data they know they preserve... ahem.
;; TODO: maybe store analysis in another field?

;; The analysis state is a map with keys :vars and :generators
;; :generators is a boolean telling if :yield or :yield-from is seen within that scope,
;; which will mark it as a generator (even if the statement is unreachable, e.g. after a return).
;; vars maps each variable name to a map with keys :locality :bound? :referred?
;; locality can be :param, :local, :nonlocal, :global
;; bound? can be nil (never bound) or true (bound at some point)
;; referred? can be nil (never referred) or true (referred at some point)
;; referred includes read or unbound.
;;
;; Flow control within a scope is not respected in this phase,
;; so we can't tell whether a variable is bound before it's read.
;; But we can tell that an attempt was made to bind a nonlocal or global variable,
;; or that conflicting locality declarations were made,
;; or that a variable is read but never bound.

(declare &C &C* &Cargs)

(defn declare-locality [l x]
  #(if (or (nil? %) (= % l)) l
       ($syntax-error x "variable declared %s but was already declared %s"
                      [:locality :previous-locality] {:locality (name l) :previous-locality (name %)})))

(defn check-effects [x E in-function?]
  (map (fn [[v e]]
         (let [{l :locality b :bound? r :referred?} e]
           (when (and ({:nonlocal :global} l) b)
             ($syntax-error x "invalid side-effect: trying to bind variable %s declared as %s"
                            [:name :locality] {:name v :locality l}))))
       (:vars E))
  (when-not in-function?
    (when (:return? E)
      ($syntax-error x "invalid return"))
    (when (:generator? E)
      ($syntax-error x "invalid yield")))
  E)

(comment
(defn &C-function [x head args return-type body]
  (letfn [(M [n & a] (with-meta (vec (concat head a)) (merge m n)))]
    (&let [args (&Cargs args) ; handle type and default
           return-type (&C return-type)]
          (let [[_ innerE] ((&map #(&assoc-in [:vars %] {:bound? true :locality :param})
                                  (args-vars args)) nil)
                [body innerE] ((&C body) innerE)]
            (with-meta (into head [:function args return-type body])
              (merge (meta x) (check-effects x innerE true))))))))


(defn &C [x]
  (let [m (meta x)]
    (letfn [(v [& a] (with-meta (vec a) m))
            (w [& a] (with-meta (apply vec* a) m))
            (M [n & a] (with-meta (vec a) (merge m n)))
            (&x [& m] (&do (apply &do m) (&return x)))
            (&m [& a] (&let [x (apply &do a)] (with-meta x m)))]
      (match [x]
        [nil] &nil
        [[:id s]] (&x (&assoc-in [:vars s :referred?] true))
        [[:bind [:id s] :as n a]] (&let [_ (&assoc-in [:vars s :bound?] true)
                                         a (&C a)]
                                        (v :bind n a))
        [[:unbind [:id s]]] (&x (&assoc-in [:vars s :referred?] true))
        [[:nonlocal [:id s]]] (&x (&assoc-in [:vars s :nonlocal] true))
        [[:global [:id s]]] (&x (&assoc-in [:vars s :global] true))
        [[:unwind-protect body protection]] (&m (&tag :unwind-protect (&C body) (&C protection)))
        [[:handler-bind body handler]] (&m (&tag :handler-bind (&C body) (&C handler)))
        [[:builtin f & a]] (&let [a (&C* a)] (w :builtin f a))
        [[(:or ':from ':import ':constant ':break ':continue) & _]] (&x)
        [[h :guard #{:suite :raise :while :if} & a]] (&m (&tag* h (&C* a)))
        [[:return a]]
        (&do (&assoc-in [:return?] true) (&m (&tag :return (&C a))))
        [[h :guard #{:yield :yield-from} a]]
        (&do (&assoc-in [:generator?] true) (&m (&tag h (&C a))))
        [[:handler-bind [:id s] :as target body handler]]
        (&let [type (&C type)
               _ (&assoc-in [:vars s :bound?] true)
               body (&C body)]
              (v :except type target body))
        [[:call f a]] (&m (&tag :call (&C f) (&Cargs a)))
        [[:class [:id s] :as name args body]]
        (&m (&let [args (&Cargs args)
                   _ (&assoc-in [:vars s :bound?] true)]
                  (let [[body innerE] ((&C body) nil)]
                    (if (:generator? innerE)
                      ($syntax-error x "invalid yield in class %a" [:name] {:name s})
                      (M (check-effects x innerE false) :class name args body)))))
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
        [[:defn definitions body]]
        ;; TODO: handle this case!
        (comment ; XXXX
        (&m (&let [args (&Cargs args) ; handle type and default
                   return-type (&C return-type)]
                  (let [[_ innerE] ((&map #(&assoc-in [:vars %] {:bound? true :locality :param})
                                          (args-vars args)) nil)
                        [body innerE] ((&C body) innerE)]
                    (M (check-effects x innerE true)
                       :function args return-type body)))))
        [[:module s]] (&m (&let [s (&C s)] [:module s]))
        ;; any remaining starred expression is a syntax error
        [[:starred & _]]
        ($syntax-error x "starred expressions are only allowed as targets to assignment %s")
        :else ($syntax-error x "unexpected expression %s during clarification pass")))))

(defn &C* [xs] (&map &C xs))
(def &Cargs (&args &C))

(defn clarify [x]
  (let [[x E] ((&C x) nil)]
    (check-effects x E false)
    x))
