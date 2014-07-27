(ns skylark.utilities
  ;;(:require [leijure.delta-position :as delta])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.edn :as edn]))

;; Miscellaneous general purpose utilities.
;; TODO: move them to leijure?

(defn NIY [& args] (throw (Throwable. "Not Implemented Yet")))
(defn NFN [& args] nil) ;; nil for now

(defn booleanize [x] (if x true false))

(defmacro <- "Nesting macro" ([] nil) ([x] x) ([x & y] `(~@x (<- ~@y)))) ;; like UIOP:NEST in CL

(defmacro ignore-errors
  ([x] `(ignore-errors ~x nil))
  ([x y] `(try ~x (catch Exception ~'_ ~y))))

(defn remove-prefix [prefix string]
  (if (.startsWith string prefix) (subs string (count prefix)) string))

(defn url-filename [url]
  (or (ignore-errors (-> url .toURI java.io.File.))
      (ignore-errors (-> url .getPath java.io.File.))
      (ignore-errors (remove-prefix "file:" (.toString url)))))

(defn ensure-reader-and-filename [s & options]
  (cond
   (instance? java.io.Reader s)
   [s nil]
   (instance? java.io.File s)
   [(java.io.FileInputStream. s) (.getName s)]
   (instance? java.net.URL s)
   [(-> s .openStream java.io.InputStreamReader. java.io.BufferedReader.) (url-filename s)]
   (instance? String s)
   [(java.io.StringReader. s) nil]
   :else (throw (Exception. (format "can't turn %s into a Reader" s)))))

(defn call-with-reader [x f]
  (let [[stream filename] (ensure-reader-and-filename x)]
       (binding [*file* filename]
         (f stream))))

(defn join-bytes [arrays]
  (let [sizes (map count arrays)
        sizes_r (vec (reductions + sizes))
        offsets (cons 0 (drop-last sizes_r))
        total (last sizes_r)
        out (byte-array total)]
    (dorun (map #(System/arraycopy %2 0 out %1 %3) offsets arrays sizes))
    out))
