(ns skylark.runtime
  (:require [clojure.core :as c]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math])
  (:use [clojure.core.match :only [match]]
        [skylark.debug]
        [skylark.utilities]))

;; https://docs.python.org/3/library/operator.html
;; https://docs.python.org/3/reference/datamodel.html#customization

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

(def $None :None)
(def $NoneType ;; python singleton None
  {:isinstance? #(= % $None)})

(defn builtin? [x]
  (or (= x $None) (list? x)
      (boolean
       (#{java.lang.Boolean java.lang.Long clojure.lang.BigInt java.lang.String
          clojure.lang.PersistentVector clojure.lang.PersistentHashSet clojure.lang.PersistentArrayMap}
        (type x)))))
(def $builtin ;; python builtin types
  {:isinstance? builtin?})

(defn easy-falsity? [x]
  ;; NB: [] catches (), but empty byte-array isn't caught.
  (or (not x) (boolean (#{[] {} #{} 0 0M 0.0 "" $None} x))))
(def $False false)
(def $easy-falsity ;; python easy falsity
  {:isinstance? easy-falsity?})

(defn easy-truth? [x]
  (or (= x true) (and (not (easy-falsity? x)) (builtin? x))))
(def $True true)
(def $easy-truth ;; python easy truth
  {:isinstance? easy-truth?})

(def $empty-list [])
(def $list ;; python list
  {:isinstance? list?})

(def $empty-tuple [])
(def $tuple ;; python tuple
  {:isinstance? vector?})

(def $Boolean java.lang.Boolean)
(def $dict clojure.lang.PersistentArrayMap)
(def $empty-dict {})
(def $set clojure.lang.PersistentHashSet)
(def $empty-set #{})

(defn type? [type x]
  (cond
    (instance? java.lang.Class type) (instance? type x)
    (map? type) ((type :isinstance?) x)
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

(defn binary-operation [op x y]
  (NFN))

(define-operation $add [x y]
  [clojure.lang.BigInt clojure.lang.BigInt] (+ ^clojure.lang.BigInt x ^clojure.lang.BigInt y)
  [java.lang.Double java.lang.Double] (+ ^java.lang.Double x ^java.lang.Double y)
  [clojure.lang.BigInt java.lang.Double] (+ ^clojure.lang.BigInt x y)
  [java.lang.Double clojure.lang.BigInt] (+ x ^clojure.lang.BigInt y)
  [$list $list] (concat x y)
  [$tuple $tuple] (concat x y)
  [$set $set] (into x y)
  [$dict $dict] (into x y)
  :else (binary-operation "__add__" x y))

(define-operation $sub [x y]
  [clojure.lang.BigInt clojure.lang.BigInt] (- ^clojure.lang.BigInt x ^clojure.lang.BigInt y)
  [java.lang.Double java.lang.Double] (- ^java.lang.Double x ^java.lang.Double y)
  [clojure.lang.BigInt java.lang.Double] (- ^clojure.lang.BigInt x ^java.lang.Double y)
  [java.lang.Double clojure.lang.BigInt] (- x ^clojure.lang.BigInt ^java.lang.Double y)
  :else (binary-operation "__sub__" x y))

(define-operation $mul [x y]
  [clojure.lang.BigInt clojure.lang.BigInt] (* ^clojure.lang.BigInt x ^clojure.lang.BigInt y)
  [java.lang.Double java.lang.Double] (* ^java.lang.Double x ^java.lang.Double y)
  [clojure.lang.BigInt java.lang.Double] (* ^clojure.lang.BigInt x ^java.lang.Double y)
  [java.lang.Double clojure.lang.BigInt] (* x ^clojure.lang.BigInt ^java.lang.Double y)
  :else (binary-operation "__mul__" x y))

(define-operation $div [x y] ; /
  ;; integer division here is "true division" like python3, not "classic division" like python2
  [clojure.lang.BigInt clojure.lang.BigInt] (/ (.doubleValue ^clojure.lang.BigInt x)
                                                 (.doubleValue ^clojure.lang.BigInt y))
  [java.lang.Double java.lang.Double] (/ ^java.lang.Double x ^java.lang.Double y)
  [clojure.lang.BigInt java.lang.Double] (/ ^clojure.lang.BigInt x ^java.lang.Double y)
  [java.lang.Double clojure.lang.BigInt] (/ ^clojure.lang.BigInt x ^java.lang.Double y)
  :else (binary-operation "__div__" x y))

(define-operation $floordiv [x y] ; //
  [clojure.lang.BigInt clojure.lang.BigInt]
  (quot (- ^clojure.lang.BigInt x (mod ^clojure.lang.BigInt x ^clojure.lang.BigInt y))
        ^clojure.lang.BigInt y)
  [java.lang.Double java.lang.Double] (quot (- x (mod x y) y))
  [clojure.lang.BigInt java.lang.Double] (quot (- x (mod x y) y))
  [java.lang.Double clojure.lang.BigInt] (quot (- x (mod x y) y))
  :else (binary-operation "__floordiv__" x y))

(define-operation $mod [x y] ; //
  [clojure.lang.BigInt clojure.lang.BigInt] (mod ^clojure.lang.BigInt x ^clojure.lang.BigInt y)
  [java.lang.Double java.lang.Double] (mod ^java.lang.Double x ^java.lang.Double y)
  [clojure.lang.BigInt java.lang.Double] (mod ^clojure.lang.BigInt x ^java.lang.Double y)
  [java.lang.Double clojure.lang.BigInt] (mod ^clojure.lang.BigInt x ^java.lang.Double y)
  :else (binary-operation "__mod__" x y))

;; TODO: make these extensible
(defn $and [x y] (and ($truth x) ($truth y)))
(defn $or [x y] (or ($truth x) ($truth y)))
(defn $xor [x y] (not (= ($truth x) ($truth y))))
(defn $rshift [x y] (bit-shift-right x y))
(defn $lshift [x y] (bit-shift-left x y))
(defn $pow [x y] (math/expt x y))
(defn $matmul [x y] (NIY))

(defn $not [x] (not ($truth x))) ; python: operator.not_(x), but no magic __not__ method delegation.

(defmacro $global [x] `(var-get (var ~x)))

(def iop-op {:iadd :add :isub :sub
             :imul :mul :idiv :div :ifloordiv :floordiv :imod :mod
             :iand :and :ior :or :ixor :xor :irshift :rshift :ilshift :lshift
             :ipow :pow :imatmul :matmul})

(defn make-generator [s] {:seq s})

(defn $just-yield [x]
  (make-generator (list x)))

