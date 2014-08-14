(ns skylark.parsing
  (:use [skylark.utilities]))

;; TODO: rename to state-monad or just monad?

;; Parsing monad: a state monad with some extensions.
;; monad Parser α = State → α×State
;; The State type ought to define the following methods:

(defprotocol SourceInfoStream
  "Protocol for stream providing source information"
  (prev-info [x] "source information at previous point")
  (next-info [x] "source information at next point"))

;; Monadic entities have the & prefix.

;; TODO: better error-handling:
;; * remember the furthest point of parse, and use that for the final error.
;;   Thus, fail can avoid using expensive exceptions, and we get better, useful error messages.
;; * remember the tree of alternatives at that furthest point?
;;   Thus we can also have shallower stacks, and implement a "cut" in backtracking, just like prolog.
;; This may require adding new methods above to store that information in the State,
;; e.g. a defmulti to access or update a slot "control", or just assume :control is it and use that.

;; TODO: move this to its own library leijure.parsing under leijure?

(defn &return [α] (fn [σ] [α σ]))
(def &nil (&return nil))
(defn &bind
  ([Tα fTβ] (fn [σ] (let [[α σ] (Tα σ)] ((fTβ α) σ))))
  ([Tα] Tα)
  ([] &nil)
  ([m1 m2 & ms] (reduce &bind (list* m1 m2 ms))))

(defn fail&
  ([σ fmt args map] ($error (type σ) fmt args (merge map {:σ σ :source-info (prev-info σ)})))
  ([σ fmt args] (fail& σ fmt args {}))
  ([σ fmt] (fail& σ fmt nil {}))
  ([σ] (fail& σ nil nil {})))

(defn &error [& args] (fn [σ] (apply fail& σ args)))
(def &fail (&error))
(defn try& [f σ]
  (try (f σ) (catch clojure.lang.ExceptionInfo x (when-not ($error? x (type σ)) (throw x)))))
(defn &or* [ls]
  (fn [σ] (loop [l ls]
            (if (empty? l) (&fail σ)
                (or (try& (first l) σ)
                    (recur (rest l)))))))
(defn &or [& ls] (&or* ls))

;;; Monadic macros

;;(clojure.algo.monads/defmonad parsing-m [m-result &return m-bind &bind m-zero &fail m-plus &or]))

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
  ([bindings result] ;; Same as `(clojure.algo.monads/domonad parsing-m ~bindings ~result)
     (if (empty? bindings)
       `(&return ~result) ;; or do we want later binding and use (fn [σ] ~result)?
       (let [[binding parser & more] bindings]
         `(&bind ~parser (fn [~binding] (&let ~more ~result)))))))
(defmacro &do1 [m & ms] `(&let [~'x# ~m ~'_ (&do ~@ms)]))

(defmacro &lift [fun & ms]
  (let [vars (map #(do % (gensym)) ms)
        bindings (into [] (mapcat list vars ms))]
  `(&let ~bindings (~fun ~@vars))))
(defmacro &vector [& ms] `(&lift vector ~@ms))
(defmacro &vec [& ms] `(&lift vec ~@ms))

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

(defn &seq [s] (if-let [[m & r] (seq s)] (&lift cons m (&seq r)) &nil))
(defn &map [f s] (&seq (map f s)))

(defn &into [dest & ms] (&let [s (&seq ms)] (into dest s)))
(defn &into* [dest & ms] (&let [s (&seq ms)] (into dest (apply list* s))))
(defn &tag [tag & ms] (apply &into [tag] ms))
(defn &tag* [tag & ms] (apply &into* [tag] ms))

;;; Source information processing

(defn &prev-info [σ] [(prev-info σ) σ])
(defn &next-info [σ] [(next-info σ) σ])

(defn source-info [x] (:source-info (meta x)))
(defn with-source-info [x i]
  (and x (let [m (meta x)] (if (:source-info m) x (with-meta x (merge m {:source-info i}))))))
(defn copy-source-info [x y] (with-source-info x (source-info y)))

(defn merge-info [[file start-pos _ :as i1] [filetoo _ end-pos :as i2]]
  {:pre [(or (nil? i1) (nil? i2) (= file filetoo))]}
  (cond (nil? i1) i2
        (nil? i2) i1
        :else [file start-pos end-pos]))

(defn merge-source-info [a x y] (with-source-info a (merge-info (source-info x) (source-info y))))

(defn &info [m]
  (&let [start &next-info
         value m
         end &prev-info]
        (with-source-info value (merge-info start end))))
(defmacro &leti [bindings value] `(&info (&let ~bindings ~value)))

;; In a grammar with mutual recursion between non-terminals,
;; you need to declare forward references to some non-terminals to break cycles.
(defmacro def-forward [& names] ;; bind the symbol to the var for the symbol, for late binding
  `(do ~@(map #(do `(do (declare ~%) (def ~% #'~%))) names)))
;; Alternatively, you can explicitly pass #'&other-non-terminal to your regular combinators
;; Maybe you should do THAT anyway, for the sake of extensibility or redefinability.
