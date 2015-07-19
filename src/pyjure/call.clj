(ns pyjure.call
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
     ^Integer restarg ;; index of the restarg, or false (or should we mix * and ** with names?)
     ^Integer kwarg]) ;; index of the kwarg, or false

;; Every callable function has a CalleeShape, which together with the list of parameter names
;; describes its parameter-receiving pattern.
;; We separate the names, because if the caller has no keywords,
;; we don't include them in the caller-shape-cache key
;; Note that some key-only arguments may be required because they have no default value.
;; For optimization, we want to have the positionals together at the beginning (allowing for
;; easy tucking of extra hidden bindings in front), group the optionals together,
;; and leave the * and ** at the end, which leaves the mandatory named-only just before * and **.
;; The order of arguments will thus be:
;; mandatory positional parameters, followed by optional positional parameters,
;; followed by optional keyword parameters, followed by mandatory keyword parameters,
;; followed by rest list parameter, followed by keyword dictionary parameter.
(defrecord CalleeShape
    [^Integer mandatory-positional ;; number of mandatory positional parameters
     ^Integer optional-positional  ;; number of optional positional parameters
     ^Integer optional-named-only  ;; number of optional named-only parameters
     ^Integer mandatory-named-only ;; number of mandatory named-only parameters
     ^Integer restparm             ;; index of the restparm, or false (after all named ones)
     ^Integer kwparm])	           ;; index of the kwparm, or false (last one)

;; Function pre-frobber takes a caller-shape and caller-info and returns a frobber function.
;; The frobber function takes a callee-shape and callee-names and returns a frob function.
;; The frob function takes a caller-argument vector and a function description
;; and returns a callee-parameter vector.

;; Each CallerShape holds a transient cache associating to each callee-shape
;; (or pair of callee-shape and callee-names, if it uses keywords) a frob function.

;; trick: default vector include default values for restparm and kwparm

(defn access-cache [cache key default]
  (or (@cache key) (let [val (default)] (ref-set cache (assoc cache key val)) val)))


(def caller-shape-cache-cache (ref {}))

(defn caller-shape-cache [caller-shape]
  (access-cache caller-shape-cache-cache caller-shape #(ref {})))

;;; Potential Errors

;;(derive ::missing-mandatory-arguments :TypeError)
(defn missing-mandatory-arguments [context]
  ($error :missing-enough-mandatory-arguments
          "Not enough mandatory arguments in function call %s" context))

;;(derive ::too-many-positional-arguments :TypeError)
(defn too-many-positional-arguments [context]
  ($error :too-many-positional-arguments
          "Too many positional arguments in function call %s" context))

;;(derive ::duplicate-argument :TypeError)
(defn duplicate-argument [context]
  ($error :duplicate-argument
          "keyword argument already specified in function call %s" context))

;;(derive ::unrecognized-argument :TypeError)
(defn unrecognized-argument [context]
  ($error :unrecognized-argument
          "unrecognized keyword argument in function call %s" context))

;; searching for a name is a linear search:
;; we assume the number of arguments is small, at which point that's faster than any hash-table.
(defn position [name names] (.indexOf names name)) ;; test: will that work?

;;; fallback function for the general case
(defn generic-frob
  [^CallerShape caller-shape
   ^clojure.lang.PersistentVector arguments
   ^CalleeShape callee-shape
   ^clojure.lang.PersistentVector callee-names ;; name of the arguments, including restarg and kwarg
   ^clojure.lang.PersistentVector defaults ;; vector of default values for the default arguments, in order
   caller-info callee-info]
  ;; returns a vector of values for the callee parameters
  (let [{:keys [pos keys restarg kwarg]} caller-shape
        {:keys [mandatory-positional optional-positional optional-named-only mandatory-named-only
                restparm kwparm]} callee-shape
        ;; total number of callee parameters
        positional (+ mandatory-positional optional-positional)
        named-only (+ optional-named-only mandatory-named-only)
        named (+ positional named-only)
        parms (+ named (boolean-number restparm) (boolean-number kwparm))
        ;; positional arguments from the caller
        posargs (subvec arguments 0 pos)
        posargsrest (if restarg (into posargs (nth arguments restarg)) posargs)
        [posvals restval] (if (> (count posargsrest) positional)
                            (if restparm
                              [(subvec posargsrest 0 positional) (subvec posargsrest positional)]
                              (too-many-positional-arguments
                               (varmap caller-shape callee-shape callee-names
                                       caller-info arguments callee-info)))
                            [posargsrest []])
        start-optional mandatory-positional
        end-optional (- named mandatory-named-only)
        n-posvals (count posvals)
        ;; initialize a transient for parameter values, of size n-parms
        values (transient (vec (repeat parms nil)))
        _ (loop [i 0]
            (when (< i n-posvals)
              (assoc! values i (get posvals i))
              (recur (inc i))))
        key-values (map identity keys (subvec arguments pos (+ pos (count keys))))
        rest-key-values (if kwarg (seq (arguments kwarg)) [])
        all-key-values (concat key-values rest-key-values)
        remaining-key-values ;; accept keyword arguments
        (loop [kv all-key-values rk {}]
          (if (empty? kv)
            (into {} rk)
            (let [[[k v] r] kv
                  i (position k callee-names)]
              (if (< -1 i named)
                (if (nil? (values i))
                  (do (assoc! values i v)
                      (recur r rk))
                  (duplicate-argument
                   (varmap caller-shape callee-shape callee-names
                           caller-info arguments callee-info k)))
                (if kwparm
                  (if (rk k)
                    (duplicate-argument
                     (varmap caller-shape callee-shape callee-names
                             caller-info arguments callee-info k))
                    (recur r (conj rk [k v])))
                  (unrecognized-argument
                   (varmap caller-shape callee-shape callee-names
                           caller-info arguments callee-info k)))))))]
    (when restparm (assoc! values restparm restval))
    (when kwparm (assoc! values kwparm remaining-key-values))
    ;; check that everything mandatory is initialized
    (letfn [(check-mandatory [start end]
              (loop [i start]
                (when (< i named)
                  (when (nil? (values i))
                    (missing-mandatory-arguments
                     (varmap caller-shape callee-shape callee-names
                             caller-info arguments callee-info i)))
                  (recur (inc i)))))]
      (if (< posvals mandatory-positional)
        (check-mandatory posvals mandatory-positional))
      (check-mandatory end-optional named))
    ;; use defaults, where applies
    (loop [i (max n-posvals mandatory-positional) j (- i mandatory-positional)]
      (when (< i end-optional)
        (when (nil? (values i))
          (assoc! values i (defaults j)))
        (recur (inc i) (inc j))))
    ;; success!
    (persistent! values)))

(defn generic-frobber [caller-shape cache caller-info]
  (fn [callee-shape callee-names]
    (fn [args callee-info]
      (generic-frob caller-shape args callee-shape callee-names
                    (:defaults callee-info) caller-info callee-info))))

(defn frobber-id [pos] ;; simplest case: everything identical!
  (fn [args func] args))

(defn frobber-pos-defaults [pos start-defaults] ;; next simplest case: copy some, then initialize defaults
  (fn [args func] (vec (concat args (subvec (:param-defaults func) start-defaults)))))

(defn frobber-pos-restparm [pos start-defaults restparm]
  (fn [args func] (vec (concat (subvec args 0 pos)
                               (subvec (:param-defaults func) start-defaults restparm)
                               (list (subvec args pos))))))

(defn frobber-pos-restparm-kwparm [pos start-defaults restparm]
  (fn [args func] (vec (concat (subvec args 0 pos)
                               (subvec (:param-defaults func) start-defaults restparm)
                               (list (subvec args pos) {})))))

;; Caller has no restarg, no keys, no kwarg
(defn pre-frobber-0 [^CallerShape caller-shape caller-info]
  (let [{pos :pos} caller-shape]
    (fn [^CalleeShape callee-shape ^clojure.lang.PersistentVector callee-names]
      (let [{:keys [mandatory-positional optional-positional optional-named-only mandatory-named-only
                    restparm kwparm]} callee-shape
            positional (+ mandatory-positional optional-positional)
            named-only (+ optional-named-only mandatory-named-only)
            named (+ positional named-only)
            start-defaults (- pos mandatory-positional)] ;; assume mandatory-named-only is zero
        ;; In cases known to fail, fail fast rather than use the cache
        (if (or (< pos mandatory-positional) (< 0 mandatory-named-only)
                (and (> pos positional) (not restparm)))
          (generic-frobber caller-shape callee-shape callee-names caller-info) ; let it fail
          (access-cache
           (caller-shape-cache caller-shape)
           (if (and restparm (< positional pos))
             (if kwparm
               (frobber-pos-restparm-kwparm pos start-defaults restparm)
               (frobber-pos-restparm pos start-defaults restparm))
             (if (or (< pos named) restparm kwparm)
               (frobber-pos-defaults pos start-defaults)
               (frobber-id pos)))))))))

(defn no-key-frob
  [^CallerShape caller-shape
   ^clojure.lang.PersistentVector arguments
   ^CalleeShape callee-shape
   ^clojure.lang.PersistentVector callee-names ;; name of the arguments, including restarg and kwarg
   ^clojure.lang.PersistentVector defaults ;; vector of default values for the default arguments, in order
   caller-info callee-info]
  ;; returns a vector of values for the callee parameters
  (let [{:keys [pos keys restarg kwarg]} caller-shape
        {:keys [mandatory-positional optional-positional optional-named-only
                ;; mandatory-named-only is zero by this point
                restparm kwparm]} callee-shape
        ;; total number of callee parameters
        positional (+ mandatory-positional optional-positional)
        named-only optional-named-only
        ;; total number of callee parameters
        named (+ positional named-only)
        parms (+ named (boolean-number restparm) (boolean-number kwparm))
        ;; positional arguments from the caller
        posargs (subvec arguments 0 pos)
        posargsrest (if restarg (into posargs (nth arguments restarg)) posargs)
        [posvals restval] (if (> (count posargsrest) positional)
                            (if restparm
                              [(subvec posargsrest 0 positional) (subvec posargsrest positional)]
                              (too-many-positional-arguments
                               (varmap caller-shape callee-shape callee-names
                                       caller-info arguments callee-info)))
                            [posargsrest []])
        n-posvals (count posvals)
        ;; initialize a transient for parameter values, of size n-parms
        values (transient (vec (repeat parms nil)))
        _ (loop [i 0]
            (when (< i n-posvals)
              (assoc! values i (get posvals i))
              (recur (inc i))))]
    (when restparm (assoc! values restparm restval))
    (when kwparm (assoc! values kwparm {}))
    ;; check that everything mandatory is initialized
    (when (< n-posvals mandatory-positional)
      (missing-mandatory-arguments
       (varmap caller-shape callee-shape callee-names caller-info arguments callee-info)))
    ;; use defaults, where applies
    (loop [i n-posvals j (- n-posvals mandatory-positional)]
      (when (< i named)
        (assoc! values i (defaults j)))
      (recur (inc i) (inc j)))
    ;; success!
    (persistent! values)))

(defn no-key-frobber [caller-shape cache caller-info]
  (fn [callee-shape callee-names]
    (fn [args callee-info]
      (no-key-frob caller-shape args callee-shape callee-names
                   (:param-defaults callee-info) caller-info callee-info))))

;; Called has restarg, no keys, no kwarg
(defn pre-frobber-1 [^CallerShape caller-shape caller-info]
  (let [{:keys [pos restarg]} caller-shape]
    (fn [^CalleeShape callee-shape ^clojure.lang.PersistentVector callee-names]
      (let [{:keys [mandatory-positional optional-positional optional-named-only mandatory-named-only
                    restparm kwparm]} callee-shape
        ;; total number of callee parameters
        positional (+ mandatory-positional optional-positional)
        named-only (+ optional-named-only mandatory-named-only)
        named (+ positional named-only)
        start-defaults (- pos mandatory-positional)] ;; assume mandatory-named-only is zero
        ;; In cases known to fail, fail fast rather than use the cache
        (if (or (< 0 mandatory-named-only) (and (> pos positional) (not restparm)))
          (generic-frobber caller-shape callee-shape callee-names caller-info) ; let it fail
          (access-cache
           (caller-shape-cache caller-shape)
           (cond
            (and restparm (= positional pos) (not kwparm) (not keys))
            (frobber-id (inc pos))
            ;; TODO: more magic when (and restparm (= positional pos))
            ;; and magic still when (and restparm (> pos positional))
            :else ((no-key-frobber caller-shape nil caller-info) callee-shape callee-names))))))))

(def pre-frobber-2 generic-frobber)
(def pre-frobber-3 generic-frobber)

(defn pre-frobber [^CallerShape caller-shape caller-info]
  (let [{:keys [pos keys restarg kwarg]} caller-shape] ; ignore pos
    (if (or kwarg)
      (generic-frobber caller-shape caller-info)
      ((nth [#'pre-frobber-0 #'pre-frobber-1 #'pre-frobber-2 #'pre-frobber-3]
            (boolean-number restarg (not (empty? keys))))
       caller-shape caller-info))))

;; have a dispatcher from callee-shape to frobbing function

(comment ;;; Can we provide a simple uniform representation for frob functions?
(defrecord CallPermutation
    [^Integer n-positional ;; number of positionals to just copy
     ^Integer restarg      ;; restarg argument?
     ^Integer min-restarg  ;; minimal length for restarg? due to missing mandatory and no caller-kwarg
     ^Integer max-restarg  ;; is there a maximal length for restarg? due to kw-provided optional
     ^clojure.lang.PersistentVector poskeys]))    ;; for each keyarg, in order,
