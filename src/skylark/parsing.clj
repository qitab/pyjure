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

(defn &do ([] &nil) ([m] m) ([m & ms] (reduce #(&bind % (constantly %2)) m ms)))
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
(defmacro &do1 ([m & ms] `(&let [~'x# ~m ~'_ (&do ~@ms)])))

(defmacro &call [fun & ms] ;; often call &lift
  (let [vars (map #(do % (gensym)) ms)
        bindings (into [] (mapcat list vars ms))]
  `(&let ~bindings (~fun ~@vars))))
(defmacro &vector [& ms] `(&call vector ~@ms))
(defmacro &vec [& ms] `(&call vec ~@ms))

;;; Monadic combinators

(defn &call-f
  ([fun] (&return (fun)))
  ([fun m] (&bind m (fn [x] (&return (fun x)))))
  ([fun m & ms] (&bind m (fn [x] (apply &call-f (partial fun x) ms)))))
(defn &vector-f [& ms] (apply &call-f vector ms))

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

(defn &seq [s] (if-let [[m & r] (seq s)] (&call cons m (&seq r)) &nil))
(defn &map [f s] (&seq (map f s)))

(defn &into [dest & ms] (&let [s (&seq ms)] (into dest s)))
(defn &into* [dest & ms] (&let [s (&seq ms)] (into dest (apply list* s))))
(defn &tag [tag & ms] (apply &into [tag] ms))
(defn &tag* [tag & ms] (apply &into* [tag] ms))

(defn &args [f] ;; monadically handling our representation for Python arguments.
  (fn [x]
    (let [[args star-arg more-args kw-arg] x]
    (&vector (&vec (&map f args)) (f star-arg) (&vec (&map f more-args)) (f kw-arg)))))


;;; Source information processing

(defn &prev-info [σ]
  ;;{:post [(or (info? (first %)) (DBG :bad-prev-info σ %))]}
  [(prev-info σ) σ])
(defn &next-info [σ]
  ;;{:post [(or (info? (first %)) (DBG :bad-next-info σ %))]}
  [(next-info σ) σ])

(defn info? [x]
  (or (nil? x)
      (and (vector? x) (= 3 (count x))
           (let [[file start end] x]
             (and (or (nil? file) (string? file))
                  (every? (fn [v] (and (vector? v) (= 2 (count v)) (every? integer? v)))
                          [start end]))))))

(defn source-info [x]
  ;; {:post [(or (info? %) (DBG :bsi x %))]}
  (:source-info (meta x)))
(defn with-source-info [x i]
  (and x (let [m (meta x)] (if (:source-info m) x (with-meta x (merge m {:source-info i}))))))
(defn copy-source-info [x y] (with-source-info x (source-info y)))

(defn merge-info [x y]
  ;;{:pre [(info? x) (info? y) (or (nil? x) (nil? y) (= (first x) (first y)))]
  ;; :post [(info? %)]}
  (cond (nil? x) y
        (nil? y) x
        :else (let [[file start-pos _] x [_ _ end-pos] y] [file start-pos end-pos])))

(defn merge-source-info [a x y]
  (with-source-info a (merge-info (source-info x) (source-info y))))

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

;; Manipulating the environment
(defn &assoc-in [keys value] (fn [E] [nil (assoc-in E keys value)]))
(defn &update-in [keys fun & args] (fn [E] [nil (apply update-in E keys fun args)]))
(defn &get-in
  ([keys] (fn [E] [(get-in E keys) E]))
  ([keys not-found] (fn [E] [(get-in E keys not-found) E])))

(defmacro &DBG [tag f & x]
  `(let [x# ~(vec x)
         tag# ~tag]
     (fn [E#]
       (print tag#) (print " => ") (prn x#)
       (let [[v# E#] ((apply ~f x#) E#)]
         (print tag#) (print " <= ") (prn v#)
         [v# E#]))))
