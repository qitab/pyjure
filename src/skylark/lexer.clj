(ns skylark.lexer
  (:require [leijure.delta-position :as delta])
  (:require [clojure.string :as str])
  (:use [clojure.algo.monads]))

;; See Python 2 Documentation: https://docs.python.org/2/reference/lexical_analysis.html

;; This lexer is written in monadic style.
;; type State = InputStream×OutputStream×IndentStack×DelimStack
;; monad PythonLexer α = State → α×State
;;
;; The output is a sequence of vector of the form: [type data info]
;; where type is a keyword, data is some data depending on the keyword,
;; and info is a vector [file start end] of a filename (or nil),
;; start and end position (each a vector [line column] counting from 1).

;; File information, if any, to include together with token position
(def ^:dynamic *file* nil)

(defn init-state [input]
  [(delta/positioned-stream input {:line-offset 1 :column-offset 0}) () '(0) ()])

(defn σ-in [σ] (σ 0))
(defn σ-out [σ] (σ 1))
(defn σ-indent-stack [σ] (σ 2))
(defn σ-delim-stack [σ] (σ 3))

(defn in-char [in] (let [[[x]] in] x))
(defn in-position [in] (let [[[_ l c]] in] [l c]))
(defn in-column [in] (let [[[_ l c]] in] c))
(defn done? [σ] (empty? (σ-in σ)))

(def fail-msg "python lexer failure")
(defn fail ;; for richer throws, use slingshot ?
  ([] (fail {}))
  ([details] (throw (clojure.lang.ExceptionInfo. fail-msg details))))

;; Monadic lexer entities have the & prefix.

(defn &return [α]
  (fn [σ] [α σ]))
(defn &bind [Tα fTβ]
  (fn [σ] (let [[α σ] (Tα σ)] ((fTβ α) σ))))
(defmacro &do
  ([] `(&return nil))
  ([m] m)
  ([m & ms] `(&bind ~m (fn [~'_] (&do ~@ms)))))
(defn &error
  ([]
     (&error {}))
  ([details]
     (fn [[in]]
       (let [[l c] (in-position in)]
         (fail (conj details [:file *file*] [:line l] [:column c]))))))
(def &fail (&error))
(defn try-lex [f σ]
  (try (f σ)
       (catch clojure.lang.ExceptionInfo x
         (when-not (= (.getMessage x) fail-msg) (throw x)))))
(defn &or* [ls]
  (fn [σ]
    (loop [l ls]
      (if (empty? l) ((&error) σ)
          (or (try-lex (first l) σ)
              (recur (rest l)))))))
(defn &or [& ls] (&or* ls))
(def &nil (&return nil))
(defn &not [l] ;; lookahead that l does not appear here
  (fn [σ] (if (try-lex l σ) (fail) [nil σ])))

(defmonad pylex-m
  [ m-result &return
    m-bind &bind
    m-zero (&error)
    m-plus &or ])

(defmacro &let [& r] `(domonad pylex-m ~@r))

(defn &peek-char [σ] [(in-char (σ-in σ)) σ])
(defn &position [σ] [(in-column (σ-in σ)) σ])
(defn &column [σ] [(in-column (σ-in σ)) σ])
(defn &indent-stack [σ] [(σ-indent-stack σ) σ])
(defn &read-char [[in out is ds]] [(in-char in) [(rest in) out is ds]])
(defn &emit [& α] (fn [[in out is ds]] [nil [in (apply conj out α) is ds]]))
(defn &neof [σ] ((if (done? σ) (&error {:r "unexpected EOF"}) &nil) σ))
(defn &eof [σ] ((if (done? σ) &nil (&error {:r "expected EOF"})) σ))

(defn &char-if [pred]
  (&let
    [x &read-char
     :if (not (and x (pred x)))
     :then [_ (&error {:r "expected char" :pred pred})]
     :else [_ &nil]]
    x))

(defn &char-if-not [pred]
  (&char-if #(not (pred %))))

(defn &char= [c]
  (&char-if #(= % c)))

(defn &string=
  ([s] (&string= s 0))
  ([s start] (&string= s start (.length s)))
  ([s start end]
     (if (< start end)
       (&bind (&char= (nth s start)) (fn [_] (&string= s (inc start) end)))
       (&return s))))

(defn &optional [m]
  (&or m &nil))

(defn &fold [m f a]
  (&or (&bind m #(&fold m f (f a %))) (&return a)))

(defn &chars
  ([m] (&chars m nil))
  ([m prefix] (&bind (&fold m conj (list prefix)) (fn [r] (&return (str/join (reverse r)))))))

(defn &repeat [m]
  (&fold m (constantly nil) nil))

(def whitespace #{\space \tab \formfeed})

(def crlf #{\return \newline})

(def &eol
  (&or (&do (&char= \return) (&optional (&char= \newline)))
       (&char= \newline)
       &eof))

(def &comment-eol
  (&do (&optional (&do (&char= \#) (&repeat (&char-if-not crlf)))) &eol))

(def &backslash (&char= \\))

(def &leading-whitespace-raw
  (&do &whitespace &column))

(def &implicit-line-continuation
  (fn [[in out is ds :as σ]]
    ((if (empty? ds) &fail &comment-eol) σ)))

(def &line-continuation
  (&or
   &implicit-line-continuation
   (&do &backslash &eol)))

(def &whitespace
  (&repeat (&or (&char-if whitespace)
                &line-continuation)))

(def &leading-whitespace-continued
  (&let
    [column &leading-whitespace-raw
     _ &line-continuation]
    column))

(def &leading-whitespace
  (&let
    [col0 (&fold &leading-whitespace-continued + 0)
     col1 &leading-whitespace-raw]
    (+ col0 col1)))

(defn &indent [column]
  (fn [[in out [top :as is] ds :as σ]]
    (let [pos (in-position in)
          info [*file* pos pos]
          tok+ #(conj % [%2 nil info])]
      (if (> column top)
        [nil [in (tok+ out :indent) (conj is column) ds]]
        (loop [[top & ris :as is] is
               out out]
          (cond
           (= column top) [nil [in out is ds]]
           (or (empty? ris) (> column top)) ((&error {:r "invalid dedentation"}) σ)
           :else (recur ris (tok+ out :dedent))))))))

(defn char-range [first last]
  (map char (range (int first) (inc (int last)))))

(def uppercase (into #{} (char-range \A \Z)))
(def lowercase (into #{} (char-range \a \z)))
(def digits (into #{} (char-range \0 \9)))
(def letters_ (clojure.set/union uppercase lowercase #{\_}))
(def letters_digits (clojure.set/union letters_ digits))

(def keywords
  (let [l '(and as assert break class continue
            def del elif else except exec
            finally for from global if import in is
            lambda not or pass print raise return try
            while with yield)]
    (into {} (map #(vector % (keyword %)) l))))

(def &ident-or-keyword
  (&let
   [start &position
    c (&char-if letters_)
    x (&chars (&char-if letters_digits) c)
    end &position]
   (let [s (symbol x)
         info [*file* start end]]
     (if-let [k (keywords s)]
       [k nil info]
       [:id s info]))))

(defn char-lower-case [c]
  (when c (char (java.lang.Character/toLowerCase (int c)))))

(def char-name-chars (clojure.set/union uppercase #{\space}))

(def &named-char
  (&let
    [  (&char= \{)
     name (&chars (&char-if char-name-chars))
     _ (&char= \})
       (&error {:r "Not implemented yet" :function '&named-char :name name})]
    nil))

(defmacro int<-digits [base & digits]
  (reduce #(do `(+ (* ~% ~base) ~%2)) digits))

(defn oct-digit [c]
  (when-let [n (and c (int c))]
    (when (<= (int \0) n (int \7)) (- n (int \0)))))

(def &oct-digit (&let [c &read-char] (or (oct-digit c) (fail))))

(def &octal-char
  (&let [o0 &oct-digit o1 &oct-digit o2 &oct-digit]
     (char (int<-digits 8 o0 o1 o2))))

(defn dec-digit [c]
  (when-let [n (and c (int c))]
    (when (<= (int \0) n (int \9)) (- n (int \0)))))

(def &dec-digit (&let [c &read-char] (or (dec-digit c) (fail))))

(defn hex-digit [c]
  (when-let [n (and c (int c))]
    (cond (<= (int \0) n (int \9)) (- n (int \0))
          (<= (int \a) n (int \f)) (+ n (- 10 (int \a)))
          (<= (int \A) n (int \F)) (+ n (- 10 (int \A))))))

(def &hex-digit (&let [c &read-char] (or (hex-digit c) (fail))))

(def &latin1-char
  (&let [x0 &hex-digit x1 &hex-digit]
     (char (int<-digits 16 x0 x1))))

(def &unicode-char-16
  (&let [x0 &hex-digit x1 &hex-digit x2 &hex-digit x3 &hex-digit]
     (char (int<-digits 16 x0 x1 x2 x3))))

(def &unicode-char-32
  (&let [x0 &hex-digit x1 &hex-digit x2 &hex-digit x3 &hex-digit
         x4 &hex-digit x5 &hex-digit x6 &hex-digit x7 &hex-digit]
     (char (int<-digits 16 x0 x1 x2 x3 x4 x5 x6 x7))))

(defn &escape-seq [ub r]
  (&let
   [_ &backslash
    c (&char-if (constantly true))
    x (if r
        (&return (str \\ c))
        (condp = c ;; There is a bug with case in clojure 1.6.0 !!!
          \newline &nil
          \return (&do (&optional (&char= \newline)) &nil)
          \' (&return c)
          \" (&return c)
          \a (&return \u0007) ;; ASCII Bell (BEL)
          \b (&return \u0008) ;; ASCII Backspace (BS)
          \f (&return \u000c) ;; ASCII Formfeed (FF)
          \n (&return \u000a) ;; ASCII Linefeed (LF)
          \r (&return \u000d) ;; ASCII Carriage Return (CR)
          \t (&return \u0009) ;; ASCII Horizontal Tab (TAB)
          \v (&return \u000b) ;; ASCII Vertical Tab (VT)
          \N (if (= ub \u) &named-char &fail)
          \x &latin1-char ;; \xhh Character with hex value hh
          \u &unicode-char-16 ;; \uxxxx Character with 16-bit hex value xxxx (Unicode only)
          \U &unicode-char-32 ;; \Uxxxxxxxx Character with 32-bit hex value xxxxxxxx (Unicode only)
          (if (oct-digit c) &octal-char &fail)))]
   x))

(defn &string-item [long? q ub r]
  (&or
   (&char-if-not (if long? #{q \\} #{q \\ \newline \return}))
   (&escape-seq ub r)
   (if long? (let [l (&char= q)] (&do l (&not (&do l l)) (&return q))) &fail)))

(def &string-start-quote
  (&let
   [q (&char-if #{\' \"})
    lq (&optional (&do (&char= q) (&char= q)))]
   (if lq [true q] [false q])))

(defn &string-end-quote [long? q]
  (let [l (&char= q)]
    (if long? (&do l l l) l)))

(def &string-literal
  (&let
   [ub (&optional (&let [c (&char-if #{\u \U \b \B})] (char-lower-case c)))
    r (&optional (&let [c (&char-if #{\r \R})] (char-lower-case c)))
    [long? q] &string-start-quote
    s (&chars (&string-item long? q ub r))
    _ (&string-end-quote long? q)]
   [:string [s long? q ub r]]))

(def &numeric-literal
  (&error {:r "&numeric-literal is not implemented yet"}))

(def delimiters
  '("@" "," ":" "." "`" "=" ";"
    "+=" "-=" "*=" "/=" "//" "%="
    "&=" "|=" "^=" ">>=" "<<=" "**="))

(def operators
  '("+" "-" "*" "**" "/" "//" "%"
    "<<" ">>" "&" "|" "^" "~"
    "<=" ">=" "==" "!=" "<>" "<" ">"))

(def delimiters-and-operators
  ;; Order matters: prefixes must come afterwards, or we must use a better decision algorithm.
  (sort-by count > (concat delimiters operators)))

(def &delimiter-or-operator
  (&let [s (&or* (map #(&string= %) delimiters-and-operators))]
    [(keyword s) nil]))

(def paren-closer {\( \) \[ \] \{ \}})

(def &paren
  (&or
   (&bind (&char-if #{\( \[ \{})
          #(fn [[in out is ds]]
             [[% nil] [in out is (conj ds (paren-closer %))]]))
   (&bind (&char-if #{\) \] \}})
          #(fn [[in out is ds :as σ]]
             [[% nil]
              [in out is (if (= % (first ds)) (rest ds) ((&error {:r "Unmatched delimiter"}) σ))]]))))

(def &token
  (&let
   [start &position
    [type data] (&or &string-literal &numeric-literal
                     &paren &ident-or-keyword &delimiter-or-operator)
    end &position
      (&emit [type data [*file* start end]])]
   nil))

(def &logical-line
  (&let
    [_ &neof
     column &leading-whitespace
     _ (&or &comment-eol ;; skip blank lines
            (&do
             (&indent column)
             (&repeat (&do &token &whitespace))
             &comment-eol
             (&bind &position
                       (fn [pos] (&emit [:newline nil [*file* pos pos]])))))]
    nil))

(defn &finish [[in out is ds :as σ]]
  (let [pos (in-position in)
        info [*file* pos pos]
        tok+ #(conj % [%2 nil info])]
    (loop [[top & ris] is
           out out]
      (if (> top 1) (recur ris (tok+ out :dedent))
          [(reverse (tok+ out :endmarker))
           [in () '(0) ds]]))))

(def &python
  (&do (&repeat &logical-line) &eof &finish))

(defn python-lexer [input]
  (first (&python (init-state input))))

(comment
  (defn test-lex* [input]
    (try (python-lexer input) (catch clojure.lang.ExceptionInfo x (.data x))))

  (defn test-lex [input]
    (map (fn [[a b _]] [a b]) (test-lex* input)))

  (test-lex* "
def hello (world, *more)
  print()
  \"a b\" + 'c d' + \\
  foo['abcd',
bar, \"1 2 3\",
     # comment
  baz]
def quux ()
  {ur\"x\": \"a\"}")
  (try-python-lexer "def hello abcd\n  foo\n  baz\ndef quux\n  ur\"x\\\"\"")
);comment
