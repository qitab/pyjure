(ns skylark.lexer
  (:require [leijure.delta-position :as delta])
  ;;(:require [skylark.semantics :as s])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:require [clojure.edn :as edn])
  (:use [clojure.algo.monads]))

;; See Python 2 Documentation: https://docs.python.org/2/reference/lexical_analysis.html
;; See Python 3 Documentation: https://docs.python.org/3.5/reference/lexical_analysis.html

;; This lexer is written in monadic style using the State monad, where
;; type State = InputStream×PrevInfo×OutputStream×IndentStack×DelimStack
;; monad PythonLexer α = State → α×State
;;
;; The output is a sequence of vector of the form: [type data info]
;; where type is a keyword, data is some data depending on the keyword,
;; and info is a vector [file start end] of a filename (or nil),
;; start and end position (each a vector [line column] counting from 1).

(defn mkσ
  ([input] (let [pos {:line-offset 1 :column-offset 0}]
                (mkσ (delta/positioned-stream input pos) pos () '(0) ())))
  ([in pp out is ds] [in pp out is ds]))
(defn σ-in [σ] (σ 0))
(defn σ-prev-position [σ] (σ 1))
(defn σ-out [σ] (σ 2))
(defn σ-indent-stack [σ] (σ 3))
(defn σ-delim-stack [σ] (σ 4))

(defn in-char [in] (let [[[x]] in] x))
(defn in-position [in] (let [[[_ l c]] in] [l c]))
(defn in-column [in] (let [[[_ l c]] in] c))

(defn done? [σ] (empty? (σ-in σ)))
(defn σ-position [σ] (if (done? σ) (σ-prev-position σ) (in-position (σ-in σ))))

(def fail-msg "python lexer failure")
(defn fail ;; for richer throws, use slingshot ?
  ;; For more efficient failing, we ought to use a monad that knows about continuations
  ([] (fail {}))
  ([details] (throw (clojure.lang.ExceptionInfo. fail-msg details))))

;; Monadic lexer entities have the & prefix.

(defn &return [α] (fn [σ] [α σ]))
(def &nil (&return nil))
(defn &bind
  ([Tα fTβ] (fn [σ] (let [[α σ] (Tα σ)] ((fTβ α) σ))))
  ([Tα] Tα)
  ([] &nil)
  ([m1 m2 & ms] (reduce &bind (list* m1 m2 ms))))
(defmacro &do
  ([] `(&return nil))
  ([m] m)
  ([m & ms] `(&bind ~m (fn [~'_] (&do ~@ms)))))
(defn &error
  ([] (&error {}))
  ([details] (fn [σ] (let [[l c] (σ-position σ)]
                          (fail (conj details [:file *file*] [:line l] [:column c]))))))
(def &fail (&error))
(defn try-lex [f σ]
  (try (f σ) (catch clojure.lang.ExceptionInfo x
               (when-not (= (.getMessage x) fail-msg) (throw x)))))
(defn &or* [ls]
  (fn [σ] (loop [l ls]
            (if (empty? l) ((&error) σ)
                (or (try-lex (first l) σ)
                    (recur (rest l)))))))
(defn &or [& ls] (&or* ls))
(defn &not [l] ;; lookahead that l does not appear here
  (fn [σ] (if (try-lex l σ) (fail) [nil σ])))

(defmonad lexer-m
  [ m-result &return
    m-bind &bind
    m-zero (&error)
    m-plus &or ])

(defn find-if [pred seq] ;; NB: doesn't distinguish between finding nil and not finding
  (if (empty? seq) nil (let [x (first seq)] (if (pred x) x (recur pred (rest seq))))))
(defmacro &let
  ([bindings]
     `(&let ~bindings ;; return the last result that is not _ or a keyword
            ~(find-if #(not (or (= % '_) (keyword? %))) (reverse (map first (partition 2 bindings))))))
  ([bindings result] `(domonad lexer-m ~bindings ~result)))
(defmacro &do1 [m & ms] `(&let [~'x# ~m ~'_ (&do ~@ms)]))

(defn &peek-char [σ] [(in-char (σ-in σ)) σ])
(defn &position [σ] [(σ-position σ) σ])
(defn &column [σ] [(in-column (σ-in σ)) σ])
(defn &indent-stack [σ] [(σ-indent-stack σ) σ])
;; NB: the two below functions match State
(defn &read-char [[in pp out is ds :as σ]] [(in-char in) (mkσ (rest in) (σ-position σ) out is ds)])
(defn &emit [& α] (fn [[in pp out is ds]] [nil (mkσ in pp (apply conj out α) is ds)]))
(defn &neof [σ] ((if (done? σ) (&error {:r "unexpected EOF"}) &nil) σ))
(defn &eof [σ] ((if (done? σ) &nil (&error {:r "expected EOF"})) σ))

(defn &char-if [pred]
  (&let [x &read-char
         :if (not (and x (pred x)))
         :then [_ (&error {:r "expected char" :pred pred})]
         :else [_ &nil]]))
(defn &char-if-not [pred] (&char-if #(not (pred %))))
(defn &char= [c] (&char-if #(= % c)))

(defn &string=
  ([s] (&string= s 0))
  ([s start] (&string= s start (.length s)))
  ([s start end]
     (if (< start end)
       (&bind (&char= (nth s start)) (fn [_] (&string= s (inc start) end)))
       (&return s))))

(defn &optional [m] (&or m &nil))
(defn &fold [m f a] (&or (&bind m #(&fold m f (f a %))) (&return a)))
(defn &conj* [m a] (&fold m conj a))
(defn &repeat [m] (&fold m (constantly nil) nil))

(defn stringify [r] (str/join (reverse r)))
(defn bytify [r] (byte-array (map int (stringify r))))

(defn &chars
  ([m] (&chars m ()))
  ([m prefix] (&chars m prefix stringify))
  ([m prefix finalize] (&let [s (&conj* m prefix)] (finalize s))))

(def whitespace #{\space \tab \formfeed})
(def crlf #{\return \newline})
(def &eol (&or (&do (&char= \return) (&optional (&char= \newline)))
               (&char= \newline)
               &eof))
(def &comment-eol (&do (&optional (&do (&char= \#) (&repeat (&char-if-not crlf)))) &eol))
(def &backslash (&char= \\))
(def &implicit-line-continuation (fn [σ] ((if (empty? (σ-delim-stack σ)) &fail &comment-eol) σ)))
(def &explicit-line-continuation (&do &backslash &eol))
(def &line-continuation (&or &implicit-line-continuation &explicit-line-continuation))
(def &whitespace (&repeat (&or (&char-if whitespace) &line-continuation)))
(def &leading-whitespace-raw (&do &whitespace &column))
(def &leading-whitespace-continued (&do1 &leading-whitespace-raw &explicit-line-continuation))
(def &leading-whitespace (&let [col0 (&fold &leading-whitespace-continued + 0)
                                col1 &leading-whitespace-raw] (+ col0 col1)))

(defn &indent [column]
  (fn [[in pp out [top :as is] ds :as σ]] ;; match State
    (let [pos (σ-position σ)
          info [*file* pos pos]
          tok+ #(conj % [%2 nil info])]
      (if (> column top)
        [nil (mkσ in pp (tok+ out :indent) (conj is column) ds)]
        (loop [[top & ris :as nis] is
               nout out]
          (cond
           (= column top) [nil (mkσ in pp nout nis ds)]
           (or (empty? ris) (> column top)) ((&error {:r "invalid dedentation"}) σ)
           :else (recur ris (tok+ out :dedent))))))))

(defn char-range [first last] (map char (range (int first) (inc (int last)))))
(def uppercase (into #{} (char-range \A \Z)))
(def lowercase (into #{} (char-range \a \z)))
(def digits (into #{} (char-range \0 \9)))
(def letters_ (set/union uppercase lowercase #{\_}))
(def letters_digits (set/union letters_ digits))

(def keywords ;; keywords in the Python 3 sense, here symbols on the Clojure side.
  (let [l '("False" "None" "True"
            "and" "as" "assert" "break" "class" "continue"
            "def" "del" "elif" "else" "except"
            "finally" "for" "from" "global" "if" "import" "in" "is"
            "lambda" "nonlocal" "not" "or" "pass" "raise" "return" "try"
            "while" "with" "yield")]
    (into {} (map #(vector % (symbol %)) l))))

;; TODO: python 3 accepts unicode letters, too. See Python 3 documentation above.
(def &ident-or-keyword
  (&let [c (&char-if letters_)
         s (&chars (&char-if letters_digits) (list c))]
        (if-let [k (keywords s)]
          [k nil] ;; skylark keywords as clojure symbols
          [:id s]))) ;; identifiers as strings (for now... can be interned later)

(defn char-lower-case [c] (when c (char (java.lang.Character/toLowerCase (int c)))))
(def char-name-chars (set/union uppercase #{\space}))
(def &named-char
  (&let [  (&char= \{)
         name (&chars (&char-if char-name-chars))
         _ (&char= \})
           (&error {:r "Not implemented yet" :function '&named-char :name name})]))

(defmacro int<-digits [base & digits] (reduce #(do `(+ (* ~% ~base) ~%2)) digits))

(defn octal-digit [c]
  (when-let [n (and c (int c))]
    (when (<= (int \0) n (int \7)) (- n (int \0)))))
(def &octal-digit (&let [c &read-char] (or (octal-digit c) (fail))))
(def &octal-char
  (&let [o0 &octal-digit o1 &octal-digit o2 &octal-digit] (char (int<-digits 8 o0 o1 o2))))

(defn decimal-digit [c]
  (when-let [n (and c (int c))]
    (when (<= (int \0) n (int \9)) (- n (int \0)))))
(def &decimal-digit (&let [c &read-char] (or (decimal-digit c) (fail))))

(defn hexadecimal-digit [c]
  (when-let [n (and c (int c))]
    (cond (<= (int \0) n (int \9)) (- n (int \0))
          (<= (int \a) n (int \f)) (+ n (- 10 (int \a)))
          (<= (int \A) n (int \F)) (+ n (- 10 (int \A))))))
(def &hexadecimal-digit (&let [c &read-char] (or (hexadecimal-digit c) (fail))))
(def &latin1-char
  (&let [x0 &hexadecimal-digit x1 &hexadecimal-digit] (char (int<-digits 16 x0 x1))))
(def &unicode-char-16
  (&let [x0 &hexadecimal-digit x1 &hexadecimal-digit x2 &hexadecimal-digit x3 &hexadecimal-digit]
     (char (int<-digits 16 x0 x1 x2 x3))))
(def &unicode-char-32
  (&let [x0 &hexadecimal-digit x1 &hexadecimal-digit x2 &hexadecimal-digit x3 &hexadecimal-digit
         x4 &hexadecimal-digit x5 &hexadecimal-digit x6 &hexadecimal-digit x7 &hexadecimal-digit]
     (char (int<-digits 16 x0 x1 x2 x3 x4 x5 x6 x7))))

(defn &escape-seq [b r]
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
          \N (if b &fail &named-char)
          \x &latin1-char ;; \xhh Character with hex value hh
          \u (if b &fail &unicode-char-16) ;; \uxxxx Character with 16-bit hex value xxxx
          \U (if b &fail &unicode-char-32) ;; \Uxxxxxxxx Character with 32-bit hex value xxxxxxxx
          (if (octal-digit c) &octal-char &fail)))]))

(defn ascii-char? [c] (and (char? c) (< (int c) 128)))

(defn &string-item [long? q b r]
  (&or
   (&char-if (let [p (if long? #{q \\} #{q \\ \newline \return})]
               (if b #(and (ascii-char? %) (not (p %))) (complement p))))
   (&escape-seq b r)
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
   ;; Python 3 only has U, R, B BR (case insensitive, order insensitive)
   [ubr (&optional (&let [c (&char-if #{\u \U \b \B \r \R})] (char-lower-case c)))
    br (cond (= ubr \b) (&optional (&char-if #{\r \R}))
             (= ubr \r) (&optional (&char-if #{\b \B}))
             :else &nil)
    [b r] (&return (let [br (char-lower-case br)]
                     \u [false false]
                     \r [(= br \b) true]
                     \b [true (= br \r)]
                     nil [false false]))
    [long? q] &string-start-quote
    s (&chars (&string-item long? q b r) () (if b bytify stringify))
    _ (&string-end-quote long? q)]
   [(if b :bytes :string) s]))

(defn &intpart [prefix]
  (&conj* (&char-if decimal-digit) prefix))

(defn &fraction [prefix]
  (&do (&char= \.) (&intpart (conj prefix \.))))

(defn &exponent [prefix]
  (&let [c (&char-if #{\e \E})
         s (&optional (&char-if #{\+ \-}))
         e (&intpart (list* s c prefix))]))

(def &point-float
  (&or (&bind (&optional (&intpart ())) &fraction)
       (&let [i (&intpart ()) c (&char= \.)] (conj i c))))

(def &exponent-float
  (&bind (&or &point-float (&intpart ())) &exponent))

(def &float-literal
  (&let [x (&or &exponent-float &point-float)]
        [:float (Double/parseDouble (stringify x))]))

(def &decimal-integer
  (&chars (&char-if decimal-digit) '("10r")))

(def &octal-integer
  (&chars (&char-if octal-digit) '("8r")))

(def &hexadecimal-integer
  (&chars (&char-if hexadecimal-digit) '("16r")))

(def &binary-integer
  (&chars (&char-if #{\0 \1}) '("2r")))

(def &integer-literal
  (&let [c &peek-char
         i (cond
            (= c \0) (&bind (&do &read-char &peek-char)
                            (fn [c] (cond (#{\o \O} c) (&do &read-char &octal-integer)
                                          (#{\x \X} c) (&do &read-char &hexadecimal-integer)
                                          (#{\b \B} c) (&do &read-char &binary-integer)
                                          ;; (octal-digit c) &octal-integer ;; Python 2 ism
                                          :else (&do (&repeat (&char= \0)) (&return "0")))))
            (decimal-digit c) &decimal-integer
            :else &fail)]
    [:integer (edn/read-string i)]))

(def &numeric-literal
  (&let [n (&or &float-literal &integer-literal)
         j (&optional (&char-if #{\j \J}))]
     (if j [:imaginary n] n)))

(def delimiters ;; Should we prefix them all with s/ ? Also operators, keywords...
  '(("," comma) (":" colon) ("." dot) (";" semicolon) ("@" matmul) ("=" assign) ("..." ellipsis)
    ;; Augmented assignment operators, with the name of corresponding magic operator, as per
    ;; http://www.rafekettler.com/magicmethods.html
    ("+=" iadd) ("-=" isub) ("*=" imul) ("/=" imul) ("//=" ifloordiv) ("%=" imod)
    ("&=" iand) ("|=" ior) ("^=" ixor) (">>=" irshift) ("<<=" ilshift) ("**=" ipow) ("@=" imatmul)
    ;; ("`" repr) ;; Python 2 has `x` for repr(x), but it's deprecated and not in Python 3.
    ("->" rarrow))) ;; PEP 3107 syntax to denote function types, yet omitted in Python 3 lexer doc.

(def operators
  '(("+" add) ("-" sub) ("*" mul) ("**" pow) ("/" div) ("//" floordiv) ("%" mod)
    ("<<" lshift) (">>" rshift) ("&" and_) ("|" or_) ("^" xor) ("~" invert)
    ;; Comparison operators:
    ;; ("<>" ne) ;; deprecated Python 2 ism
    ("<" lt) (">" gt) ("<=" le) (">=" ge) ("==" eq) ("!=" ne)))

(def delimiters-and-operators
  ;; Order matters: prefixes must come afterwards,
  ;; or we must use a better decision algorithm that knows about prefixes
  (sort-by #(count (first %)) > (concat delimiters operators)))

(def &delimiter-or-operator
  (&let [x (&or* (map (fn [[s x]] (&do (&string= s) (&return x))) delimiters-and-operators))]
        [x nil]))

(def paren-closer {\( \) \[ \] \{ \}})

(def &paren
  (&or
   (&bind (&char-if #{\( \[ \{})
          #(fn [[in pp out is ds]] ;; match State
             [[% nil] (mkσ in pp out is (conj ds (paren-closer %)))]))
   (&bind (&char-if #{\) \] \}})
          #(fn [[in pp out is ds :as σ]]
             [[% nil]
              (mkσ in pp out is
                   (if (= % (first ds)) (rest ds) ((&error {:r "Unmatched delimiter"}) σ)))]))))

(def &token ;; accepts a token and emits it.
  (&let
   [start &position
    [type data] (&or &string-literal &numeric-literal
                     &paren &ident-or-keyword &delimiter-or-operator)
    end &position
    x (&emit [type data [*file* start end]])]))

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

(defn &finish [[in pp out is ds :as σ]] ;; match State
  (let [pos (σ-position σ)
        info [*file* pos pos]
        tok+ #(conj % [%2 nil info])]
    (loop [[top & ris] is
           out out]
      (if (> top 1) (recur ris (tok+ out :dedent))
          [(reverse (tok+ out :endmarker))
           (mkσ in pp () '(0) ds)]))))

(def &python
  (&do (&repeat &logical-line) &eof &finish))

(defn python-lexer [input]
  (first (&python (mkσ input))))

