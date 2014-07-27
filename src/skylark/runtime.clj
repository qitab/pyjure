(ns skylark.runtime
  (:require [clojure.core :as c])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [clojure.core.match :only [match]])
  (:use [skylark.utilities]))

(def initial-environment {})

(defn register-initial-binding [s v]
  (set! initial-environment (conj initial-environment [s v])))


(defn $class [object] (NFN))
(defn $get-method [class name] (NFN))


(def $bytes (Class/forName "[B"))
(defn byte-array? [x] (= (type x) $bytes))

(defn builtin? [x]
  (or (nil? x) (list? x)
      (booleanize
       (#{java.lang.Boolean java.lang.Long java.math.BigDecimal java.lang.String
          clojure.lang.PersistentVector clojure.lang.PersistentHashSet clojure.lang.PersistentArrayMap}
        (type x)))))
(def $builtin ;; python builtin types
  {:element? builtin?})

(defn easy-falsy? [x]
  ;; NB: [] catches (), but empty byte-array isn't caught.
  (or (not x) (booleanize (#{[] {} #{} 0 0M 0.0 ""} x))))
(def $False false)
(def $easy-falsy ;; python easy falsy
  {:element? easy-falsy?})

(defn easy-truthy? [x]
  (or (= x true) (and (not (easy-falsy? x)) (builtin? x))))
(def $True true)
(def $easy-truthy ;; python easy truthy
  {:element? easy-truthy?})

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

(defn subtly-truthy? [x]
  (let [c ($class x)]
     (if-let [m ($get-method c '__bool__)] (not= $False (m x)) ;; TODO: implement for $bytes
       (if-let [m ($get-method c '__len__)] (not= 0 (m x))
          true))))

(define-operation truthy? [x]
  [$Boolean] x
  [$easy-falsy] false
  [$easy-truthy] true
  :else (subtly-truthy? x))

(defmacro $if
  ([] `$None)
  ([else] else)
  ([test if-true & more] `(if (truthy? ~test) ~if-true ($if ~@more))))

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
