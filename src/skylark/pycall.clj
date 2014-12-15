(ns pyjure.pycall
  (:use [clojure.core.match :only [match]]
        [pyjure.utilities]
        [pyjure.runtime]
        [pyjure.exceptions]))

;; This file handles the semantics of python-like calling conventions
;; https://docs.python.org/3.3/reference/expressions.html#calls

;; Every call site has a CallerShape, that describes its argument-passing pattern
(defrecord CallerShape
    [^Int pos   ;; number of positional arguments
     ^Vector keys ;; vector of names
     ^Int restarg ;; index of the restarg, or false
     ^Int kwarg]) ;; index of the kwarg, or false

;; Every callable function has a CalleeShape, which together with the list of parameter names
;; describes its parameter-receiving pattern.
;; We separate the names, because if the caller has no keywords,
;; we don't include them in the caller-shape-cache key
(defrecord CalleeShape
    [^Int n-mandatory   ;; number of mandatory parameters
     ^Int n-positional  ;; number of positional parameters (mandatory + optional (with default))
     ^Int n-named       ;; number of named parameters (mandatory + optional + key-only)
     ^Int restparm      ;; index of the restparm, or false (n-named or false)
     ^Int kwparm])      ;; index of the kwparm, or false (last one, or false)

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
;; for each of the 16 combinations of the caller-shape's shape,
;; we have a specialized pre-frobber.

(defn boolean-number
  "given a list of generalized booleans as arguments, what integer do they encode in little-endian binary?"
  ([a] (if a 1 0))
  ([a b] (+ (if a 1 0) (if b 2 0)))
  ([a b c] (+ (if a 1 0) (if b 2 0) (if c 4 0)))
  ([a b c d] (+ (if a 1 0) (if b 2 0) (if c 4 0) (if d 8 0)))
  ([a b c d e] (+ (if a 1 0) (if b 2 0) (if c 4 0) (if d 8 0) (if d 16 0)))
  ([_ _ _ _ _ _ & _ :as l]
     (loop [l l x 1 s 0] (if (seq l) (let [[h & t] l] (recur t (+ x x) (if h (+ s x) s))) s))))

(defn caller-shape-shape [^CallerShape caller-shape]
  (let [{:keys [keys restarg kwarg]} caller-shape] ;; pos
    (boolean-number (not (empty? keys)) restarg kwarg))) ;; (plus? pos)

;;; Potential Errors

(derive ::not-enough-mandatory-arguments :TypeError)
(defn not-enough-mandatory-arguments [caller-shape callee-shape callee-names source-info args func]
  ($error ::not-enough-mandatory-arguments
          "Not enough mandatory arguments in function call %s"
          [source-info func caller-shape args callee-shape callee-names]))
(defn frobber-not-enough-mandatory-arguments [caller-shape callee-shape callee-names source-info]
  (fn [args func]
    (not-enough-mandatory-arguments caller-shape callee-shape callee-names source-info args func)))

(derive ::too-many-positional-arguments :TypeError)
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
  (fn [args func] (concat (subvec args 0 pos) [(subvec args pos) $empty-list))))

(defn pre-frobber-0 [^CallerShape caller-shape cache source-info]
  (let [{pos :pos} caller-shape]
    (fn [^CalleeShape callee-shape ^Vector callee-names]
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

(defn pre-frobber-1 [^CallerShape caller-shape cache source-info]
  (let [{pos :pos} caller-shape]
    (fn [^CalleeShape callee-shape ^Vector callee-names]
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

(defn pre-frobber [^CallerShape caller-shape source-info]
  ((nth [pre-frobber-0  pre-frobber-1  pre-frobber-2  pre-frobber-3
         pre-frobber-4  pre-frobber-5  pre-frobber-6  pre-frobber-7]
        (caller-shape-shape caller-shape))
   caller-shape (caller-shape-cache caller-shape) source-info))

;; have a dispatcher from callee-shape to frobbing function

(comment ;;; Can we provide a simple uniform representation for frob functions?
(defrecord CallPermutation
    [^Int n-positional ;; number of positionals to just copy
     ^Int restarg      ;; if there a restarg argument?
     ^Int min-restarg  ;; is there a minimal length for restarg? due to missing mandatory and no caller-kwarg
     ^Int max-restarg  ;; is there a maximal length for restarg? due to kw-provided optional
     ^Vector poskeys]))   ;; for each keyarg, in order,

;; searching for a name is a linear search:
;; we assume the number of arguments is small, at which point that's faster than any hash-table.

(defn find-name [name names] (.indexOf names name)) ;; test: will that work?

