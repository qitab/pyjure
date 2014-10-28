(ns skylark.lexer
  (:require [leijure.delta-position :as delta]
            [clojure.string :as str]
            [clojure.set :as set])
  (:use [skylark.utilities]
        [skylark.parsing]))

;; See Python 2 Documentation: https://docs.python.org/2/reference/lexical_analysis.html
;; See Python 3 Documentation: https://docs.python.org/3.5/reference/lexical_analysis.html

;; This lexer is written in monadic style using the State monad, where
;; type State = InputStream×FileName×PrevPosition×OutputStream×IndentStack×DelimStack
;; monad PythonLexer α = State → α×State
;;
;; The output is a sequence of vector of the form: [type data info]
;; where type is a keyword, data is some data depending on the keyword,
;; and info is a vector [file start end] of a filename (or nil),
;; start and end position (each a vector [line column] counting from 1).

;; TODO: use ICU4J to support Python 3 Unicode identifiers.

(defn in-char [in] (let [[[x]] in] x))
(defn in-position [in] (let [[[_ l c]] in] [l c]))
(defn in-column [in] (let [[[_ l c]] in] c))

(defn done? [σ] (empty? (:in σ)))
(defn position [σ] (if (done? σ) (:prev-position σ) (in-position (:in σ))))

(defrecord LexerState [in file prev-position out indent-stack delim-stack]
  SourceInfoStream
  (prev-info [σ] (let [{x :prev-position f :file} σ] [f x x]))
  (next-info [σ] (let [x (position σ)] [(:file σ) x x])))


;;; Basic input

(defn &peek-char [σ] [(in-char (:in σ)) σ])
(defn &column [σ] [(in-column (:in σ)) σ])
(defn &indent-stack [σ] [(:indent-stack σ) σ])
(defn &read-char [{in :in :as σ}] [(in-char in) (assoc σ :in (rest in) :prev-position (position σ))])
(defn &emit [& α] (fn [σ] [nil (assoc σ :out (apply conj (:out σ) α))]))
(defn &neof [σ] ((if (done? σ) (&error "unexpected EOF") &nil) σ))
(defn &eof [σ] ((if (done? σ) &nil (&error "expected EOF nil")) σ))

(defn &char-if [pred]
  (&let [x &read-char
         r (if (and x (pred x)) (&return x)
               (&error "expected char %s" [:pred] {:pred pred}))]))
(defn &char-if-not [pred] (&char-if #(not (pred %))))
(defn &char= [c] (&char-if #(= % c)))

(defn &string=
  ([s] (&string= s 0))
  ([s start] (&string= s start (.length s)))
  ([s start end]
     (if (< start end)
       (&bind (&char= (nth s start)) (fn [_] (&string= s (inc start) end)))
       (&return s))))

;;; Input helpers

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
(def &implicit-line-continuation (fn [σ] ((if (empty? (:delim-stack σ)) &fail &comment-eol) σ)))
(def &explicit-line-continuation (&do &backslash &eol))
(def &line-continuation (&or &implicit-line-continuation &explicit-line-continuation))
(def &whitespace (&repeat (&or (&char-if whitespace) &line-continuation)))
(def &leading-whitespace-raw (&do &whitespace &column))
(def &leading-whitespace-continued (&do1 &leading-whitespace-raw &explicit-line-continuation))
(def &leading-whitespace (&let [col0 (&fold &leading-whitespace-continued + 0)
                                col1 &leading-whitespace-raw] (+ col0 col1)))

(defn &indent [column]
  (fn [{out :out [top :as is] :indent-stack :as σ}]
    (let [pos (position σ)
          info [(:file σ) pos pos]
          tok+ #(conj % (with-source-info info [%2]))]
      (if (> column top)
        [nil (assoc σ :out (tok+ out :indent) :indent-stack (conj is column))]
        (loop [[top & ris :as nis] is
               nout out]
          (cond
           (= column top) [nil (assoc σ :out nout :indent-stack nis)]
           (or (empty? ris) (> column top)) ((&error "invalid dedentation") σ)
           :else (recur ris (tok+ nout :dedent))))))))

(defn char-range [first last] (map char (range (int first) (inc (int last)))))
(def uppercase (into #{} (char-range \A \Z)))
(def lowercase (into #{} (char-range \a \z)))
(def digits (into #{} (char-range \0 \9)))
(def letters_ (set/union uppercase lowercase #{\_}))
(def letters_digits (set/union letters_ digits))

(defn sym [x] (keyword x)) ;; (symbol "skylark.semantics" x)

(def keywords ;; keywords in the Python 3 sense, here symbols on the Clojure side.
  (let [l '("False" "None" "True"
            "and" "as" "assert" "break" "class" "continue"
            "def" "del" "elif" "else" "except"
            "finally" "for" "from" "global" "if" "import" "in" "is"
            "lambda" "nonlocal" "not" "or" "pass" "raise" "return" "try"
            "while" "with" "yield")]
    (into {} (map #(vector % (sym %)) l))))

;; TODO: python 3 accepts unicode letters, too. See Python 3 documentation above.
(def &ident-or-keyword
  (&let [c (&char-if letters_)
         s (&chars (&char-if letters_digits) (list c))]
        (if-let [k (keywords s)]
          [k] ;; skylark keywords as clojure symbols
          [:id s]))) ;; identifiers as strings (for now... can be interned later)

(defn char-lower-case [c] (when c (char (java.lang.Character/toLowerCase (int c)))))
(def char-name-chars (set/union uppercase #{\space}))
(def &named-char
  (&let [  (&char= \{)
         name (&chars (&char-if char-name-chars))
         _ (&char= \})
           (&error "Function %s not implemented yet (char name %s)"
                   [:function :name] {:function '&named-char :name name})]))

(defmacro int<-digits [base & digits] (reduce #(do `(+ (* ~% ~base) ~%2)) digits))

(defn octal-digit [c]
  (when-let [n (and c (int c))]
    (when (<= (int \0) n (int \7)) (- n (int \0)))))
(def &octal-digit (&let [c &read-char x (if (octal-digit c) (&return c) &fail)]))
(def &octal-char
  (&let [o0 &octal-digit o1 &octal-digit o2 &octal-digit] (char (int<-digits 8 o0 o1 o2))))

(defn decimal-digit [c]
  (when-let [n (and c (int c))]
    (when (<= (int \0) n (int \9)) (- n (int \0)))))
(def &decimal-digit (&let [c &read-char x (if (decimal-digit c) (&return c) &fail)]))

(defn hexadecimal-digit [c]
  (when-let [n (and c (int c))]
    (cond (<= (int \0) n (int \9)) (- n (int \0))
          (<= (int \a) n (int \f)) (+ n (- 10 (int \a)))
          (<= (int \A) n (int \F)) (+ n (- 10 (int \A))))))
(def &hexadecimal-digit (&let [c &read-char x (if (hexadecimal-digit c) (&return c) &fail)]))
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

(defn &string-end-quote [long? q] (let [l (&char= q)] (if long? (&do l l l) l)))

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

(defn &intpart [prefix] (&conj+ (&char-if decimal-digit) prefix))
(defn &fraction [prefix] (&do (&char= \.) (&intpart (conj prefix \.))))
(defn &exponent [prefix]
  (&let [c (&char-if #{\e \E})
         s (&optional (&char-if #{\+ \-}))
         * (&intpart (list* s c prefix))]))
(def &point-float (&or (&bind (&optional (&intpart ())) &fraction)
                       (&call conj (&intpart ()) (&char= \.))))
(def &exponent-float (&bind (&or &point-float (&intpart ())) &exponent))
(def &float-literal
  (&let [x (&or &exponent-float &point-float)]
        [:float (Double/parseDouble (stringify x))]))

(defn integerize [x] (+ 0N (clojure.core/read-string (stringify x))))
(def &decimal-integer (&chars (&char-if decimal-digit) '("10r") integerize))
(def &octal-integer (&chars (&char-if octal-digit) '("8r") integerize))
(def &hexadecimal-integer (&chars (&char-if hexadecimal-digit) '("16r") integerize))
(def &binary-integer (&chars (&char-if #{\0 \1}) '("2r") integerize))

(def &integer-literal
  (&let [c &peek-char
         i (cond
            (= c \0) (&bind (&do &read-char &peek-char)
                            (fn [c] (cond (#{\o \O} c) (&do &read-char &octal-integer)
                                          (#{\x \X} c) (&do &read-char &hexadecimal-integer)
                                          (#{\b \B} c) (&do &read-char &binary-integer)
                                          ;; (octal-digit c) &octal-integer ;; Python 2 ism
                                          :else (&do (&repeat (&char= \0)) (&return 0N)))))
            (decimal-digit c) &decimal-integer
            :else &fail)]
    [:integer i]))
(def &numeric-literal
  (&let [n (&or &float-literal &integer-literal)
         j (&optional (&char-if #{\j \J}))]
        (if j [:imaginary n] n)))

(def delimiters ;; Should we prefix them all with s/ ? Also operators, keywords...
  '(("," comma) (":" colon) ("." dot) (";" semicolon)
    ("@" matmul) ("=" assign) ("..." Ellipsis)
    ;; Augmented assignment operators, with the name of corresponding magic operator, as per
    ;; http://www.rafekettler.com/magicmethods.html
    ("+=" iadd) ("-=" isub) ("*=" imul) ("/=" idiv) ("//=" ifloordiv) ("%=" imod)
    ("&=" iand) ("|=" ior) ("^=" ixor) (">>=" irshift) ("<<=" ilshift)
    ("**=" ipow) ("@=" imatmul)
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
  (map (fn [[s x]] [s (sym x)]) (sort-by #(count (first %)) > (concat delimiters operators))))

(def &delimiter-or-operator
  (&vector (&or* (map (fn [[s x]] (&do (&string= s) (&return x))) delimiters-and-operators))))

(def paren-closer {\( \) \[ \] \{ \}})

(def &paren
  (&or (&bind (&char-if #{\( \[ \{})
              #(fn [σ]
                 [[%] (assoc σ :delim-stack (conj (:delim-stack σ) (paren-closer %)))]))
       (&bind (&char-if #{\) \] \}})
              #(fn [{[d & ds] :delim-stack :as σ}]
                 [[%]
                  (assoc σ :delim-stack
                         (if (= % d) ds ((&error "Unmatched delimiter") σ)))]))))

(def &token ;; accepts a token and emits it.
  (&bind (&info (&or &string-literal &numeric-literal
                     &paren &ident-or-keyword &delimiter-or-operator)) &emit))

(def &logical-line
  (&let [_ &neof
         column &leading-whitespace
         _ (&or &comment-eol ;; skip blank lines
                (&do
                 (&indent column)
                 (&repeat (&do &token &whitespace))
                 &comment-eol
                 (&bind &prev-info #(&emit (with-source-info % [:newline])))))]
    nil))

(defn &finish [{out :out is :indent-stack :as σ}]
  (let [pos (position σ)
        info [(:file σ) pos pos]
        tok+ #(conj % (with-source-info info [%2]))]
    (loop [[top & ris] is
           out out]
      (if (> top 1) (recur ris (tok+ out :dedent))
          [(reverse (tok+ out :endmarker))
           (assoc σ :out () :indent-stack '(0))]))))

(def &python
  (&do (&repeat &logical-line) &eof &finish))

(defn position-stream [[input filename]]
  [(delta/positioned-stream input {:line-offset 1 :column-offset 0}) filename])

(defn mkLexerState [[positioned-stream filename]]
  (->LexerState positioned-stream filename [0 0] () '(0) ()))

(defn lex [[positioned-stream filename]]
  (first (&python (mkLexerState [positioned-stream filename]))))
