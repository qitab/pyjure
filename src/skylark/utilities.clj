(ns skylark.utilities)

;; Miscellaneous general purpose utilities.
;; TODO: move them to leijure?
;; or to one of https://github.com/weavejester/medley or https://github.com/flatland/useful

(defn NIY [& args] (throw (Throwable. "Not Implemented Yet")))
(defn NFN [& args] nil) ;; nil for now

(defmacro <- "Nesting macro" ([] nil) ([x] x) ([x & y] `(~@x (<- ~@y)))) ;; like UIOP:NEST in CL

(defn vec*
  ([l] (vec l))
  ([a l] (into [a] l))
  ([a b l] (into [a b] l))
  ([a b c & l] (let [v (vec l)] (concat [a b c] (pop v) (last v)))))

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

(defn tryf [fun] (try (fun) (catch clojure.lang.ExceptionInfo x (.data x))))

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

;;(defn $lift [f & args] (if ...

(defn $true? [x] (= x true))
(defn $false? [x] (= x false))
(defn $unknown? [x] (= x nil))
(defn ∧ [x y] (if (= x y) x nil)) ;; negative multiplicative, $and: whichever branch is taken will have the properties of both these branches.


(defn $error
  ([msg tag fmt args m] (throw (ex-info "$error" (merge m {::tag tag ::format fmt ::args args}))))
  ([msg tag m] ($error msg tag nil nil m)))

(defn $error?
  ([x] (second (find (ex-data x) ::tag)))
  ([x tag] (= tag ($error? x))))

(defn $error-string [ex]
  (if-let [{tag ::tag info ::source-info fmt ::format args ::args :as m} (ex-data ex)]
    (str tag (when info (str " at " info))
         (when fmt (str ": " (apply format fmt (map m args)))))))
