(ns skylark.runtime
  (:require [clojure.core :as c]
            [clojure.string :as str]
            [clojure.set :as set])
  (:use [clojure.core.match :only [match]]
        [skylark.utilities]))

(defn $syntax-error
  ([x fmt args map] ($error "Syntax Error" 'syntax-error fmt args
                            (merge map {:expr x :source-info (meta x)})))
  ([x fmt] ($syntax-error x fmt [:expr] {}))
  ([x] ($syntax-error x nil nil {})))

(def $initial-environment {})

(defn register-initial-binding [s v]
  (set! $initial-environment (conj $initial-environment [s v])))

(defn $class [object] (NFN))
(defn $get-method [class name] (NFN))

(def $bytes (Class/forName "[B"))
(defn byte-array? [x] (= (type x) $bytes))

(defn literal? [x]
  (or (integer? x) (float? x) (string? x) (byte-array? x)))

(defn runtime-symbol [x]
  (symbol 'skylark.runtime (str \$ (name x))))

(defn builtin? [x]
  (or (nil? x) (list? x)
      (boolean
       (#{java.lang.Boolean java.lang.Long java.math.BigDecimal java.lang.String
          clojure.lang.PersistentVector clojure.lang.PersistentHashSet clojure.lang.PersistentArrayMap}
        (type x)))))
(def $builtin ;; python builtin types
  {:element? builtin?})

(defn easy-falsity? [x]
  ;; NB: [] catches (), but empty byte-array isn't caught.
  (or (not x) (boolean (#{[] {} #{} 0 0M 0.0 ""} x))))
(def $False false)
(def $easy-falsity ;; python easy falsity
  {:element? easy-falsity?})

(defn easy-truth? [x]
  (or (= x true) (and (not (easy-falsity? x)) (builtin? x))))
(def $True true)
(def $easy-truth ;; python easy truth
  {:element? easy-truth?})

(def $empty-list [])
(def $list ;; python list
  {:element? list?})

(def $empty-tuple [])
(def $tuple ;; python tuple
  {:element? vector?})

(def $None nil)
(def $NoneType ;; python singleton None
  {:element? #(= % nil)})

(def $Boolean java.lang.Boolean)
(def $dict clojure.lang.PersistentArrayMap)
(def $empty-dict {})
(def $set clojure.lang.PersistentHashSet)
(def $empty-set #{})

(defn type? [type x]
  (cond
    (instance? java.lang.Class type) (instance? type x)
    (map? type) ((type :element?) x)
    :else (NIY)))

(defn type-matcher [formals types]
  `(and ~@(map (fn [f t]
                 (assert (instance? clojure.lang.Symbol f))
                 `(type? ~t ~f))
               formals types)))

(defmacro define-operation [name formals & body]
  ;; TODO: 1- automate the name munging from parser keyword to clojure symbol to skylark binding
  ;; 2- define underlying generic function, declare methods, etc.
  ;; 3- in the future, do type-directed inlining.
  `(defn ~name ~formals
     (cond ~@(mapcat
              (fn [[types expr]]
                (list (if (= types :else) :else (type-matcher formals types)) expr))
              (partition 2 body)))))

(defn subtle-truth? [x]
  (let [c ($class x)]
     (if-let [m ($get-method c '__bool__)] (not= $False (m x)) ;; TODO: implement for $bytes
       (if-let [m ($get-method c '__len__)] (not= 0 (m x))
          true))))

(define-operation $truth [x]
  [$Boolean] x
  [$easy-falsity] false
  [$easy-truth] true
  :else (subtle-truth? x))

(defmacro $if
  ([] `$None)
  ([else] else)
  ([test if-true & more] `(if ($truth ~test) ~if-true ($if ~@more))))

(defmacro $or
  ([] `$False) ;; unused by Python itself
  ([x] x)
  ([x & xs] `(let [x# ~x] ($if x# x# ($or ~@xs)))))

(defmacro $and
  ([] `$True) ;; unused by Python itself
  ([x] x)
  ([x & xs] `(let [x# ~x] ($if x# ($and ~@xs) x#))))

(define-operation $add [x y]
  ([java.math.BigDecimal java.math.BigDecimal] (+ x y)
   [java.lang.Double java.lang.Double] (+ x y)
   [java.math.BigDecimal java.lang.Doublejava.lang.Double] (+ x y)
   [java.lang.Doublejava.lang.Double java.math.BigDecimal] (+ x y)
   [$list $list] (concat x y)
   [$tuple $tuple] (concat x y)
   [$set $set] (into x y)
   [$dict $dict] (into x y)
   :else (binary-operation __add__ x y)))

(defn $not [x] (not ($truth x))) ; python: operator.not_(x), but no magic __not__ method delegation.

(defmacro $global [x] `(var-get (var ~x)))


(defn expr-macro [name]
  (NFN))

(defn block-macro [name]
  (NFN))

(defn decorator-macro [name]
  (NFN))

;;; Decorators

(defn decorate [definition decorator]
  ;;; implement decorators...
  (let [[name args] (if (symbol? decorator) [decorator nil] decorator)
        macro (decorator-macro name)]
    (if macro
      (macro args definition)
      (let [[_ defname] definition]
        `(do ~definition
             (set! ~defname (~decorator ~defname)))))))

(defmacro decorated [decorators definition]
  (reduce decorate definition decorators))
