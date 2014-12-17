(ns pyjure.utilities
  [:require [clojure.repl]])

;; Miscellaneous general purpose utilities.
;; TODO: move them to leijure?
;; or to one of https://github.com/weavejester/medley or https://github.com/flatland/useful

(defmacro <- "Nesting macro" ([] nil) ([x] x) ([x & y] `(~@x (<- ~@y)))) ;; like UIOP:NEST in CL

(defn vec*
  ([l] (vec l))
  ([a l] (into [a] l))
  ([a b l] (into [a b] l))
  ([a b c & l] (let [v (vec l)] (into [a b c] (concat (pop v) (last v))))))

(defmacro varmap [& vars] (into {} (vec (map #(vector (keyword %) %) vars))))

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

(defn tryf [fun]
  (try (fun) (catch clojure.lang.ExceptionInfo x
               (println (.data x)) (clojure.repl/pst x) (println (.data x)) x)))

(defn thread-args [front S is E]
  ;; given a semantic function S, a list of inputs is, and an environment E,
  ;; compute for each i in is the semantic function given the preceding environment,
  ;; yielding an output o and a new environment, and
  ;; return a vec of (0) the list of outputs as appended to front and (1) the last environment.
  (let [[o E] (reduce (fn [[os E] i] (let [[on En] (S i E)] [(conj os on) En]))
                      [(reverse front) E] is)]
    [(reverse o) E]))

(defn copy-meta [x y] (with-meta x (meta y)))

(defn update-in-multiple [record fields f & args]
  (reduce #(apply update-in % [%2] f args) record fields))

(defmacro DBG [tag & exprs]
    "debug macro for print-debugging:
tag is typically a constant string or keyword to identify who is printing,
but can be an arbitrary expression returning a tag to be princ'ed first;
if the tag evaluates to falsy (false or nil), nothing is printed.
exprs are expressions, which when the TAG was truthy are evaluated in order,
with their source code then their return values being printed each time.
The last expresion is *always* evaluated and its multiple values are returned,
but its source and return values are only printed if the tag was truthy;
previous expressions are not evaluated at all if TAG was falsy.
The macro expansion has relatively low overhead in space or time."
  (let [last-expr (last exprs)
        other-exprs (butlast exprs)
        thunk (gensym "thunk_")]
    `(let [tag# ~tag]
       (letfn ~(if exprs `[(~'thunk [] ~last-expr)] [])
         (if tag#
             (DBG-helper tag#
                         [~@(map #(do `['~% (fn [] ~%)]) other-exprs)]
                         ~(when exprs `['~last-expr ~'thunk]))
             ~(if exprs `(~'thunk) nil))))))

(defn DBG-helper [tag xts last-xt]
  ;; Helper for the above debugging macro
  (letfn [(foo [[expression thunk]]
            (print "  ") (pr expression) (print " => ")
            (let [val (thunk)]
              (prn val)
              val))]
    (println tag)
    (doseq [xt xts] (foo xt))
    (when last-xt (foo last-xt))))

(defn mapmap [f m] (into {} (for [[k v] m] [k (f v)])))
(defn mapcombine [f m1 m2]
  (loop [m {} m1 (seq m1) m2 m2]
    (if m1
      (let [[[k v1] & r1] m1 v2 (get m2 k) r2 (dissoc m2 k)]
        (recur (assoc m k (f v1 v2)) r1 r2))
      (into m (map (fn [[k v2]] [k (f nil v2) ]) m2)))))

;; Simple compile-time logic:
;; :⊥ \bot(tom) means
;;     at runtime "no observable branch of executable code that does anything"
;;     or at compile-time "no possible value returned"
;; :⊤ \top means "anything may happen at runtime, any and all return values could be returned"

(defn ∨ [x y] ;; negative additive. $or --- one of several branches of execution may be taken.
  (cond (= x :⊥) y
        (= y :⊥) x
        (= x y) x
        :else :⊤))

(defn $true? [x] (= x true))
(defn $false? [x] (= x false))
(defn $unknown? [x] (= x nil))
(defn ∧ [x y] (if (= x y) x nil)) ;; negative multiplicative, $and: whichever branch is taken will have the properties of both these branches.


(defn $error
  ([tag fmt args m] (throw (ex-info "$error" (merge m {::tag tag ::format fmt ::args args}))))
  ([tag m] ($error tag nil nil m)))

(defn $error?
  ([x] (second (find (ex-data x) ::tag)))
  ([x tag] (= tag ($error? x))))

(defn $error-string [ex]
  (if-let [{tag ::tag info ::source-info fmt ::format args ::args :as m} (ex-data ex)]
    (str tag (when info (str " at " info))
         (when fmt (str ": " (apply format fmt (map m args)))))))

(defn $syntax-error
  ([x fmt args map] ($error 'syntax-error fmt args
                            (merge map {:expr x :source-info (:source-info x)})))
  ([x fmt] ($syntax-error x fmt [:expr] {}))
  ([x] ($syntax-error x nil nil {})))

(defn $warning [tag fmt & args]
  ;; ignore tag for now, and always print
  ;; TODO: pretty print source location if available
  (print "WARNING: ") (println (apply format fmt args)))

(defn NIY [& args] (apply $error :not-implemented-yet args))
(defn NFN [& args] nil) ;; nil for now



;;; FURAL logic for variables and effects:
;;; (F) false or nil (unused), (U) true (unconstrained), (R) :required, (A) :affine, (L) :linear
;;;

(defn f-falsable? [x]
  (case x
    (nil false :affine true) true
    (:linear :required) false))

(defn f-at-least-once [x]
  (case x
    (nil false :linear :affine) :linear
    (:required true) :required))

(defn f-once-more [x]
  (case x
    (nil false) :linear
    (:linear :affine :required true) :required))

(defn f-maybe-once-more [x]
  (case x
    (nil false) :affine
    (:linear :required) :required
    (:affine true) true))

(defn f-many-more [x]
  :required)

(defn f-maybe-many-more [x]
  (case x (:linear :required) :required true))

(defn f-both [x y] ;; in a suite, use resource x times, then y times
  (case x
   (nil false) y
   (:linear) (f-once-more y)
   (:affine) (f-maybe-once-more y)
   (:required) x
   (true) (f-maybe-many-more y)))

(defn f-maybe [x] ;; may happen or not
  (case x
    (nil false) false
    (:linear :affine) :affine
    (:required true) true))

(defn f-either [x y] ;; in two branches, use resource x times or y times
  (cond
   (= x y) x
   (not x) (f-maybe y)
   (not y) (f-maybe x)
   (= x :linear) y ;; :affine, :required, true are the remaining choices for y
   (= y :linear) x
   :else true)) ;; either one is true, or one is :required and the other :affine

(defn f-repeat [x] ;; repeat x many times
  (case x
    (nil false) false
    (:linear :required) :required
    (:affine true) true))

(defn f-maybe-repeat [x] ;; repeat x zero, one or many times
  (boolean x))

(defn f-* [x y] ;; multiply occurrence x by occurrence y
  (case x
    (nil false) false
    (:linear) y
    (:affine) (f-maybe y)
    (:required) (f-repeat y)
    (true) (f-maybe-repeat y)))

(defn f-multiplier [x] ;; curried variant of f-*
  (case x
    (nil false) (fn [_] false)
    (:linear) (fn [y] y)
    (:affine) f-maybe
    (:required) f-repeat
    (true) f-maybe-repeat))

(defn f-min [x y] ;; minimum of the two
  (cond
   (or (not x) (not y)) false
   (or (= x :linear) (= y :linear)) :linear
   (= x :affine) (if (= y :required) :linear :affine)
   (= y :affine) (if (= x :required) :linear :affine)
   (or (= x :required) (= y :required)) :required
   :else true))


;;; Basic character kinds
(defn reduce-compare [compare l]
  (or (empty? l) (empty? (rest l))
      (loop [x (first l) l (rest l)]
        (or (empty? l)
            (let [[y & r] l] (and (compare x y) (recur y r)))))))

(defn char<=
  ([] true) ([x] true)
  ([x y] (<= (int x) (int y)))
  ([x y z & t] (reduce-compare char<= (concat [x y z] t))))
(defn char<
  ([] true) ([x] true)
  ([x y] (< (int x) (int y)))
  ([x y z & t] (reduce-compare char< (concat [x y z] t))))
(defn char>
  ([] true) ([x] true)
  ([x y] (> (int x) (int y)))
  ([x y z & t] (reduce-compare char> (concat [x y z] t))))
(defn char>=
  ([] true) ([x] true)
  ([x y] (>= (int x) (int y)))
  ([x y z & t] (reduce-compare char>= (concat [x y z] t))))

(defn in-char-range? [c first length]
  (when-let [n (and c (- (int c) (int first)))]
    (and (< -1 n length) n)))

(defn uppercase-letter? [c] (in-char-range? c \A 26))
(defn lowercase-letter? [c] (in-char-range? c \a 26))
(defn letter? [c] (or (uppercase-letter? c) (lowercase-letter? c)))
(defn digit? [c] (in-char-range? c \0 10))
(defn alphanumeric? [c] (or (digit? c) (when-let [i (letter? c)] (+ i 10))))
(defn octal-digit? [c] (in-char-range? c \0 8))
(defn hexadecimal-digit? [c] (let [n (alphanumeric? c)] (and n (<= n 16) n)))

(defn letter_? [c] (or (letter? c) (= c \_)))
(defn alphanumeric_? [c] (or (alphanumeric? c) (= c \_)))

(defn downcase-char [c]
  ;; (char (java.lang.Character/toLowerCase (int c)))))
  (when c (if-let [n (uppercase-letter? c)] (char (+ n (int \a))) c)))



(defn boolean-number
  "given a list of generalized booleans as arguments, what integer do they encode in little-endian binary?"
  ([a] (if a 1 0))
  ([a b] (+ (if a 1 0) (if b 2 0)))
  ([a b c] (+ (if a 1 0) (if b 2 0) (if c 4 0)))
  ([a b c d] (+ (if a 1 0) (if b 2 0) (if c 4 0) (if d 8 0)))
  ([a b c d e] (+ (if a 1 0) (if b 2 0) (if c 4 0) (if d 8 0) (if d 16 0)))
  ([a b c d e f & g]
     (loop [l (vec* a b c d e f g) x 1 s 0]
       (if (seq l) (let [[h & t] l] (recur t (+ x x) (if h (+ s x) s))) s))))


;;; Reexporting things from another namespace
(defmacro reexport [ns & xs]
  `(do ~@(map #(do `(def ~% ~(symbol (str ns) (str %)))) xs)))
(defmacro reexport-macro [ns & xs]
  `(do ~@(map #(do `(defmacro ~% [& a#] `(~'~(symbol (str ns) (str %)) ~@a#))) xs)))
(defmacro reexport-deferred [ns & xs]
  `(do ~@(map #(do `(defn ~% [& a#] (apply (find-var (symbol ~(str ns) ~(str %))) a#))) xs)))
(defmacro reexport-macro-deferred [ns & xs]
  `(do ~@(map #(do `(defmacro ~% [& a#] `(~(symbol ~(str ns) ~(str %)) ~@a#))) xs)))

