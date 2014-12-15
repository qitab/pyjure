(ns pyjure.names
  (:use [clojure.core.match :only [match]]
        [pyjure.utilities]))

(def $$base (atom {:__name__ "__base__"}))
(def $$types (atom {:__name__ "types"}))

(defn pychar [c]
  (cond (alphanumeric_? c) c
        (= c \-) \_
        :else nil))

(defn pyname [n]
  (cond
   (string? n) (when-let [n (filter char? (map pychar (seq n)))] (apply str n))
   (symbol? n) (pyname (name n))))

(defn register-py [namespaces name value]
  (map
   (let [key (keyword name)]
     (fn [ns] (swap! ns #(assoc % key value))))
   (cond (nil? namespaces) [$$base]
             (seq? namespaces) namespaces
             :else [namespaces])))

(defmacro defpy
  ([namespace name value] `(register-py ~namespace '~name ~value))
  ([name value] `(defpy $$base ~(pyname name) ~value))
  ([name] `(defpy ~name ~name)))

(defmacro def-py
  ([namespace name value] `(do (def ~name ~value) (defpy ~namespace ~(pyname name) ~name)))
  ([name value] `(def-py $$base ~name ~value)))

(defmacro defn-py [name & spec] `(def-py ~name (fn ~@spec)))

(defn get-py
  ([namespace name] (@namespace name))
  ([name] (get-py $$base (keyword (pyname name)))))
