(ns pyjure.pycall
  (:use [clojure.core.match :only [match]]
        [pyjure.utilities]
        [pyjure.runtime]
        [pyjure.exceptions]))

;; This file handles the semantics of python-like calling conventions
;; https://docs.python.org/3.3/reference/expressions.html#calls

;; Every call site has a CallerShape, that describes its argument-passing pattern
(defrecord CallerShape
    [^Integer pos   ;; number of positional arguments
     ^clojure.lang.PersistentVector keys ;; vector of names
     ^Integer restarg ;; index of the restarg, or false
     ^Integer kwarg]) ;; index of the kwarg, or false

;; Every callable function has a CalleeShape, which together with the list of parameter names
;; describes its parameter-receiving pattern.
;; We separate the names, because if the caller has no keywords,
;; we don't include them in the caller-shape-cache key
;; Note that some key-only arguments may be required because they have no default value.
(defrecord CalleeShape
    [^Integer n-mandatory   ;; number of mandatory parameters
     ^Integer n-positional  ;; number of positional parameters (mandatory + optional (with default))
     ^Integer n-named       ;; number of named parameters (mandatory + optional + key-only)
     ^Integer restparm      ;; index of the restparm, or false (n-named or false)
     ^Integer kwparm])      ;; index of the kwparm, or false (last one, or false)

;; Function pre-frobber takes a caller-shape and source-info and returns a frobber function.
;; The frobber function takes a callee-shape and callee-names and returns a frob function.
;; The frob function takes a caller-argument vector and a function description
;; and returns a callee-parameter vector.

;; Each CallerShape holds a transient cache associating to each callee-shape
;; (or pair of callee-shape and callee-names, if it uses keywords) a frob function.

(defn access-cache [cache key default]
  (or (@cache key) (let [val (default)] (ref-set cache (assoc cache key val)) val)))

(def caller-shape-cache-cache (ref {}))

(defn caller-shape-cache [caller-shape]
  (access-cache caller-shape-cache-cache caller-shape #(ref {})))

;; Since caller-shape is statically known at every call site,
;; for each of the 8 combinations of the caller-shape's shape,
;; we have a specialized pre-frobber.

(defn caller-shape-shape [^CallerShape caller-shape]
  (let [{:keys [keys restarg kwarg]} caller-shape] ;; note: ignoring pos, for simplification.
    (boolean-number (not (empty? keys)) restarg kwarg))) ;; (plus? pos)

;;; Potential Errors

;;(derive ::not-enough-mandatory-arguments :TypeError)
(defn not-enough-mandatory-arguments [caller-shape callee-shape callee-names source-info args func]
  ($error ::not-enough-mandatory-arguments
          "Not enough mandatory arguments in function call %s"
          [source-info func caller-shape args callee-shape callee-names]))
(defn frobber-not-enough-mandatory-arguments [caller-shape callee-shape callee-names source-info]
  (fn [args func]
    (not-enough-mandatory-arguments caller-shape callee-shape callee-names source-info args func)))

;;(derive ::too-many-positional-arguments :TypeError)
(defn too-many-positional-arguments [caller-shape callee-shape callee-names source-info args func]
  ($error ::too-many-positional-arguments
          "Too many positional arguments in function call %s"
          [source-info func caller-shape args callee-shape callee-names]))
(defn frobber-too-many-positional-arguments [caller-shape callee-shape callee-names source-info args func]
  (fn [args func]
    (too-many-positional-arguments caller-shape callee-shape callee-names source-info args func)))

(defn frobber-id [pos] ;; simplest case: everything identical!
  (fn [args func] args))

(defn frobber-pos-defaults [pos] ;; next simplest case: copy some, then initialize defaults
  (fn [args func] (vec (concat args (subvec (:param-defaults func) pos)))))

(defn frobber-pos-restparm [pos n-named]
  (fn [args func] (conj (subvec args 0 pos) (subvec args pos))))

(defn frobber-pos-restparm-kwparm [pos n-named]
  (fn [args func] (into $empty-list (concat (subvec args 0 pos) (subvec args pos)))))

;; no restarg, no keys, no kwarg
(defn pre-frobber-0 [^CallerShape caller-shape cache source-info]
  (let [{pos :pos} caller-shape]
    (fn [^CalleeShape callee-shape ^clojure.lang.PersistentVector callee-names]
      (let [{:keys [n-mandatory n-positional n-named restparm kwparm]} callee-shape]
        ;; fail fast rather than use the cache
        (cond
         (< pos n-mandatory)
         (frobber-not-enough-mandatory-arguments caller-shape callee-shape source-info)
         (and (> pos n-positional) (not restparm))
         (frobber-too-many-positional-arguments caller-shape callee-shape source-info)
         :else
         (access-cache
          cache
          #(case (boolean-number (< pos n-named) restparm kwparm)
             0 (frobber-id pos)
             (1 3 4 5 7) (frobber-pos-defaults pos)
             2 (frobber-pos-restparm pos n-named)
             6 (frobber-pos-restparm-kwparm pos n-named))))))))

;; restarg, no keys, no kwarg
(comment
(defn pre-frobber-1 [^CallerShape caller-shape cache source-info]
  (let [{pos :pos} caller-shape]
    (fn [^CalleeShape callee-shape ^clojure.lang.PersistentVector callee-names]
      (let [{:keys [n-mandatory n-positional n-named restparm kwparm]} callee-shape]
        ;; fail fast rather than use the cache
        (cond
         (< pos n-mandatory)
         (frobber-need-restarg caller-shape callee-shape source-info)
         (and (> pos n-positional) (not restparm))
         (frobber-too-many-positional-arguments caller-shape callee-shape source-info)
         :else
         (access-cache
          cache
          #(case (boolean-number (< pos n-named) restparm kwparm)
             0 (frobber-id pos)
             (1 3 4 5 7) (frobber-pos-defaults pos)
             2 (frobber-pos-restparm pos n-named)
             6 (frobber-pos-restparm-kwparm pos n-named))))))))
)

(def pre-frobber-2)
(def pre-frobber-3)
(def pre-frobber-4)
(def pre-frobber-5)
(def pre-frobber-6)
(def pre-frobber-7)

(defn pre-frobber [^CallerShape caller-shape source-info]
  ((nth [#'pre-frobber-0  #'pre-frobber-1  #'pre-frobber-2  #'pre-frobber-3
         #'pre-frobber-4  #'pre-frobber-5  #'pre-frobber-6  #'pre-frobber-7]
        (caller-shape-shape caller-shape))
   caller-shape (caller-shape-cache caller-shape) source-info))

;; have a dispatcher from callee-shape to frobbing function

(comment ;;; Can we provide a simple uniform representation for frob functions?
(defrecord CallPermutation
    [^Integer n-positional ;; number of positionals to just copy
     ^Integer restarg      ;; restarg argument?
     ^Integer min-restarg  ;; minimal length for restarg? due to missing mandatory and no caller-kwarg
     ^Integer max-restarg  ;; is there a maximal length for restarg? due to kw-provided optional
     ^clojure.lang.PersistentVector poskeys]))    ;; for each keyarg, in order,

;; searching for a name is a linear search:
;; we assume the number of arguments is small, at which point that's faster than any hash-table.

(defn find-name [name names] (.indexOf names name)) ;; test: will that work?

