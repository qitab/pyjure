(ns skylark.lexer
  (:require [leijure.delta-position :as delta])
  (:use [clojure.algo.monads]))

;; This lexer is written in monadic style.
;; type State = InputStream×IndentStack×OutputStream
;; monad PythonLexer α = State → α×State
;;
;; The output is a sequence of vector of the form: [type data info]
;; where type is a keyword, data is some data depending on the keyword,
;; and info is a vector [file start end] of a filename (or nil),
;; start and end position (each a vector [line column] counting from 1).

;; File information, if any, to include together with token position
(def ^:dynamic *file* false)

(defn init-state [input]
  [(delta/positioned-stream input {:line-offset 1 :column-offset 0}) '(0) ()])

(defn σ-in [σ] (σ 0))
(defn σ-indent-stack [σ] (σ 1))
(defn σ-out [σ] (σ 2))

(defn in-char [in] (let [[[x]] in] x))
(defn in-position [in] (let [[[_ l c]] in] [l c]))
(defn in-column [in] (let [[[_ l c]] in] c))
(defn done? [σ] (empty? (σ-in σ)))

(def $fail-msg "python lexer failure")
(defn fail ;; for richer throws, use slingshot ?
  ([] (fail {}))
  ([details] (throw (clojure.lang.ExceptionInfo. $fail-msg details))))

(defn lex-result [α]
  (fn [σ] [α σ]))
(defn lex-bind [Tα fTβ]
  (fn [σ] (let [[α σ] (Tα σ)] ((fTβ α) σ))))
(defmacro lex-do
  ([] `(lex-result nil))
  ([m] m)
  ([m & ms] `(lex-bind ~m (fn [~'_] (lex-do ~@ms)))))
(defn lex-error
  ([]
     (lex-error {}))
  ([details]
     (fn [[in _ _]]
       (let [[l c] (in-position in)]
         (fail (conj details [:file *file*] [:line l] [:column c]))))))
(defn lex-or* [lexes]
  (fn [σ]
    (loop [l lexes]
      (if (empty? l) ((lex-error) σ)
          (or (try ((first l) σ)
                   (catch clojure.lang.ExceptionInfo x
                     (when-not (= (.getMessage x) $fail-msg)
                       (throw x))))
              (recur (rest l)))))))
(defn lex-or [& lexes]
  (lex-or* lexes))
(def lex-nil (lex-result nil))

(defmonad pylex-m
  [ m-result lex-result
    m-bind lex-bind
    m-zero (lex-error)
    m-plus lex-or ])

(defmacro dolex [& r] `(domonad pylex-m ~@r))

(defn lex-peek-char [σ] [(in-char (σ-in σ)) σ])
(defn lex-position [σ] [(in-column (σ-in σ)) σ])
(defn lex-column [σ] [(in-column (σ-in σ)) σ])
(defn lex-indent-stack [σ] [(σ-indent-stack σ) σ])
(defn lex-read-char [[in is out]] [(in-char in) [(rest in) is out]])
(defn lex-emit [& α] (fn [[in is out]] [nil [in is (apply conj out α)]]))
(defn lex-done? [σ] [(done? σ) σ])
(defn lex-flush [[in is out]] [(reverse out) [in is ()]])
(defn lex-neof [σ] ((if (done? σ) (lex-error {:reason "unexpected EOF"}) lex-nil) σ))
(defn lex-eof [σ] ((if (done? σ) lex-nil (lex-error {:reason "expected EOF"})) σ))

(defn lex-char-if [pred]
  (dolex
    [x lex-read-char
     :if (not (and x (pred x)))
     :then [_ (lex-error {:reason "expected char" :pred pred})]
     :else [_ lex-nil]]
    x))

(defn lex-char-if-not [pred]
  (lex-char-if #(not (pred %))))

(defn lex-char= [c]
  (lex-char-if #(= % c)))

(defn lex-string=
  ([s] (lex-string= s 0))
  ([s start] (lex-string= s start (.length s)))
  ([s start end]
     (if (< start end)
       (lex-bind (lex-char= (nth s start)) (fn [_] (lex-string= s (inc start) end)))
       lex-nil)))

(defn lex-maybe [m]
  (lex-or m lex-nil))

(defn lex-fold [m f a]
  (lex-or (lex-bind m #(lex-fold m f (f a %))) (lex-result a)))

(defn lex-revlist [m]
  (lex-fold m conj ()))

(defn lex-repeat [m]
  (lex-fold m #(do %2) nil))

(def $whitespace #{\space \tab})
(def $crlf #{\return \newline})

(def lex-backslash (lex-char= \\))

(def lex-eol
  (lex-or (lex-do (lex-char= \return) (lex-maybe (lex-char= \newline)))
          (lex-char= \newline)
          lex-eof))

(def lex-whitespace
  (lex-repeat (lex-char-if $whitespace)))

(def lex-leading-whitespace-raw
  (lex-do lex-whitespace lex-column))

(def lex-line-continuation
  (lex-do lex-backslash lex-eol))

(def lex-leading-whitespace-continued
  (dolex
    [column lex-leading-whitespace-raw
     _ lex-line-continuation]
    column))

(def lex-leading-whitespace
  (dolex
    [col0 (lex-fold lex-leading-whitespace-continued + 0)
     col1 lex-leading-whitespace-raw]
    (+ col0 col1)))

(def lex-line ;; XXX this function is test purposes only: put the rest of the line into a string.
  (dolex
    [text (lex-revlist (lex-char-if-not $crlf))
     _ (lex-emit (clojure.string/join (reverse text)))
     _ lex-eol]
    nil))

(defn lex-indent [column]
  (fn [[in [top :as is] out :as σ]]
    (if (> column top)
      [nil [in (conj is column) (conj out :INDENT)]]
      (loop [[top & ris :as is] is
             out out]
        (cond
         (= column top) [nil [in is out]]
         (or (empty? ris) (> column top)) ((lex-error {:reason "invalid dedentation"}) σ)
         :else (recur ris (conj out :DEDENT)))))))

(def lex-comment-eol
  (lex-do (lex-maybe (lex-do (lex-char= \#) (lex-repeat (lex-char-if-not $crlf)))) lex-eol))

(defn char-range [first last]
  (map char (range (int first) (inc (int last)))))

(def $uppercase (into #{} (char-range \A \Z)))
(def $lowercase (into #{} (char-range \a \z)))
(def $digits (into #{} (char-range \0 \9)))
(def $letters_ (clojure.set/union $uppercase $lowercase #{\_}))
(def $letters_digits (clojure.set/union $letters_ $digits))

(def $keywords
  (into #{} (map #(do [% (keyword %)])
                 ["and" "as" "assert" "break" "class" "continue"
                  "def" "del" "elif" "else" "except" "exec"
                  "finally" "for" "from" "global" "if" "import" "in" "is"
                  "lambda" "not" "or" "pass" "print" "raise" "return" "try"
                  "while" "with" "yield"])))

(def lex-ident-or-keyword
  (dolex
   [start lex-position
    c (lex-char-if $letters_)
    l (lex-fold (lex-char-if $letters_digits) conj (list c))
    end lex-position]
   (let [s (clojure.string/join (reverse l))
         info [*file* start end]]
     (if-let [k ($keywords s)]
       [k nil info]
       [:ident s info]))))

(defn char-lower-case [c]
  (char (java.lang.Character/toLowerCase (int c))))

(def lex-string-quote
  (dolex
   [q (lex-char-if #{\' \"})
    lq (lex-maybe (lex-do (lex-char= q) (lex-char= q)))]
   (if lq [true q] [false q])))

(defn lex-string-contents [long? q ub r]
  (lex-error {:reason "NOT IMPLEMENTED YET"}))

(def lex-string-literal
  (dolex
   [start lex-position
    ub (lex-maybe (lex-char-if #{\u \U \b \B}))
    r (lex-maybe (lex-char-if #{\r \R}))
    sq lex-string-quote
    s (let [ub (char-lower-case ub)
            r (char-lower-case r)
            [long? q] sq]
        (lex-string-contents long? q ub r))
    end lex-position]
   [:string s [*file* start end]]))

(def lex-token
  (lex-bind (lex-or lex-string-literal lex-ident-or-keyword) lex-emit))

(def lex-logical-line
  (dolex
    [_ lex-neof
     column lex-leading-whitespace
     _ (lex-or lex-comment-eol ;; skip blank lines
               (lex-do
                (lex-indent column)
                (lex-repeat (lex-do lex-token lex-whitespace))
                lex-comment-eol
                (lex-bind lex-position
                          (fn [pos] (lex-emit [:newline nil [*file* pos pos]])))))]
    nil))

(defn lex-finish [[in is out]]
  (let [pos (in-position in)
        info [*file* pos pos]]
    (loop [[top & ris] is
           out out
           dedent? false]
      (if (> top 1) (recur ris (conj out [:dedent nil info]) true)
          [(reverse (conj (if dedent? (conj out [:newline nil info]) out) [:endmarker nil info]))
           [() '(1) ()]]))))

(def lex-python
  (lex-do (lex-repeat lex-logical-line) lex-eof lex-finish))

(defn python-lexer [input]
  (first (lex-python (init-state input))))
