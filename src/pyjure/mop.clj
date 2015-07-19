(ns pyjure.mop
  (:require [clojure.core :as c]
            [clojure.string :as str])
  (:use [clojure.core.match :only [match]]
        [pyjure.debug]
        [pyjure.utilities]
        [pyjure.names]))

;; Meta-Object Protocol for pyjure objects

;; https://docs.python.org/3/reference/datamodel.html#customization
;; https://docs.python.org/3/library/stdtypes.html#object.__dict__

;; TODO:
;; * most everything
;; * somehow use keywords as keys for python object fields, rather than strings,
;;   since they are much faster (being interned makes comparison trivial).
;;   Yet we must keep implementing python strings as uninterned strings, and
;;   normal python dicts as normal clojure maps;
;;   so when exposing internal class maps as user-visible python dicts,
;;   have a special dict type that has a key adapter.


;;;; Phase one: protocols for objects with attributes

;; The protocols
(defprotocol PyObject
  "Protocol for viewing something as a python-style object"
  ($get [x key] "get an object's attribute value by name")
  ($__dict__ [x] "get a dict of all of an object's attributes"))
(defn $get-via-attributes [x key] (get ($__dict__ x) key))

(defprotocol PyStateful
  "Protocol for stateful manipulation of a python-style object"
  ($assoc [x key value] "set an object's attribute to a new value")
  ($update [x key fun] "update the value of an object's attribute"))

;; Pure user objects
(defrecord $Pure [attr] ;; Pure object with given attribute dict
  PyObject
  ($get [x key] ((:attr x) key))
  ($__dict__ [x] (:attr x)))
(defn Pure [attr] (->$Pure attr))

;; Monotonic objects:
;; we promise to only ever update it but in monotonic ways
(defrecord $Monotonic [aattr]
  PyObject
  ($get [x key] (@(:aattr x) key))
  ($__dict__ [x] @(:aattr x))
  PyStateful
  ($assoc [x key value] (swap! (:aattr x) #(assoc % key value)))
  ($update [x key fun] (swap! (:aattr x) #(update-in % [key] fun))))
(defn Monotonic [attr] (->$Monotonic (atom attr)))

;; Individual Keywords represent a few magic entities
(def $keywords (atom {}))

(comment
(extend Keyword
  PyObject
  ($__dict__ [x] (get @$magic-keywords x))
  ($get [x key] (($__dict__ x) key)))
)

;; (def-keyword $$types :type (Monotonic {:__name__ "class"}))

(defn mkClass [prefix name bases attr]
  (Monotonic
   (merge {:__class__ :type, :__bases__ bases,
           :__name__ name, :__qualname__ (str prefix name),
           :__subclasses__ []}
          attr)))

(defmacro def-py-type [name bases attr spec]
  `(def-py $$types ~name
     (mkClass ~(pyname name) bases attr spec)))


;;; Define some base types

(def-py $None :None)
(def-py $NoneType ;; python singleton None
  {"__instancecheck__" #(= % $None)})


;;; Now for methods and functions

(defn $get-class [x] ($get x :__class__))

(defn $get-method [class name] (NFN '$get-method))
(defn $mro [class] (NIY '$mro))

(defn-py isinstance? [instance class]
  (<- (if-let [i? ($get class :__instancecheck__)] (i? instance))
      (if-let [c ($get instance :__class__)] (or (= c class) (.indexOf ($mro class) c)))
      (NIY))) ;; fallback

