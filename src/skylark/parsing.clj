(ns skylark.parsing
  (:use [clojure.algo.monads]))

;; Parsing monad: a state monad with some extensions.
;; monad PythonParser α = State → α×State
;; The State type ought to define the following methods:
(defmulti done? class) ;; a boolean true if the input is all consumed
(defmulti prev-info class) ;; source information at previous point
(defmulti next-info class) ;; source information at previous point
(defmulti fail-message class) ;; a constant error message to provide to ExceptionInfo

;; Monadic entities have the & prefix.

;; TODO: better error-handling:
;; * remember the furthest point of parse, and use that for the final error.
;; * remember the tree of alternatives at that furthest point?
;; Thus, fail can avoid using expensive exceptions, and we get better, useful error messages.
;; This may require adding new methods above to store that information in the State.

(defn &return [α] (fn [σ] [α σ]))
(def &nil (&return nil))
(defn &bind
  ([Tα fTβ] (fn [σ] (let [[α σ] (Tα σ)] ((fTβ α) σ))))
  ([Tα] Tα)
  ([] &nil)
  ([m1 m2 & ms] (reduce &bind (list* m1 m2 ms))))

(defn fail& [σ details]
  (throw (clojure.lang.ExceptionInfo.
          (fail-message σ) (conj details [:info (prev-info σ)]))))

(defn &error
  ([] (&error {}))
  ([details] (fn [σ] (fail& σ details))))
(def &fail (&error))
(defn try& [f σ]
  (try (f σ) (catch clojure.lang.ExceptionInfo x
               (when-not (= (.getMessage x) (fail-message σ)) (throw x)))))
(defn &or* [ls]
  (fn [σ] (loop [l ls]
            (if (empty? l) (&fail σ)
                (or (try& (first l) σ)
                    (recur (rest l)))))))
(defn &or [& ls] (&or* ls))

(defmonad parsing-m
  [ m-result &return
    m-bind &bind
    m-zero &fail
    m-plus &or ])

;;; Monadic macros

(defmacro &do
  ([] `(&return nil))
  ([m] m)
  ([m & ms] `(&bind ~m (fn [~'_] (&do ~@ms)))))
(defn find-if [pred seq] ;; NB: doesn't distinguish between finding nil and not finding
  (if (empty? seq) nil (let [x (first seq)] (if (pred x) x (recur pred (rest seq))))))
(defmacro &let
  ([bindings]
     `(&let ~bindings ;; return the last result that is not _ or a keyword
            ~(find-if #(not (or (= % '_) (keyword? %))) (reverse (map first (partition 2 bindings))))))
  ([bindings result] `(domonad parsing-m ~bindings ~result)))
(defmacro &do1 [m & ms] `(&let [~'x# ~m ~'_ (&do ~@ms)]))

(defmacro &lift [fun & ms]
  (let [vars (map #(do % (gensym)) ms)
        bindings (into [] (mapcat list vars ms))]
  `(&let ~bindings (~fun ~@vars))))
(defmacro &vector [& ms] `(&lift vector ~@ms))

;;; Monadic combinators

(defn &lift-f
  ([fun] (&return (fun)))
  ([fun m] (&bind m (fn [x] (&return (fun x)))))
  ([fun m & ms] (&bind m (fn [x] (apply &lift-f (partial fun x) ms)))))
(defn &vector-f [& ms] (apply &lift-f vector ms))

(defn &not [l] ;; lookahead that l does not appear here
  (fn [σ] (if (try& l σ) (&fail σ) [nil σ])))
(defn &optional [m] (&or m &nil))

(defn &fold [m f a] (&or (&bind m #(&fold m f (f a %))) (&return a)))
(defn &conj* [m a] (&fold m conj a))
(defn &conj+ [m a] (&let [x m f (&conj* m (conj a x))]))
(defn &repeat [m] (&fold m (constantly nil) nil))
(defn &list
  ([m] (&list m ()))
  ([m a] (&let [r (&conj* m a)] (reverse r))))
(defn &non-empty-list [m] (&bind m (fn [a] (&list m (list a)))))
(defn &repeat [m] (&fold m (constantly nil) nil))


;;; Source information processing

(defn &prev-info [σ] [(prev-info σ) σ])
(defn &next-info [σ] [(next-info σ) σ])

(defn merge-info [[file start-pos _] [filetoo _ end-pos]]
  {:pre [(= file filetoo)]}
  [file start-pos end-pos])

(defmacro &leti [bindings value]
  ;; not hygienic: anaphorically exposes bindings to start& end& info&
  `(&let ~(into `[~'start& &next-info] (into bindings `[~'end& &prev-info]))
         (let [~'info& (merge-info ~'start& ~'end&)] ~value)))
(defmacro &letx [bindings value]
  `(&leti ~bindings (let [~'v& ~value] (and ~'v& (conj ~'v& ~'info&)))))
(defn &info [m] (&letx [x m] x))

