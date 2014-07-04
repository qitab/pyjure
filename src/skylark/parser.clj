(ns skylark.parser
  (:require [skylark.lexer :as lex])
  (:use [clojure.algo.monads]))

;; monad T:
;;   bind: T α → (α→T β) → T β
;;   result: α → T α
;;   bind_result_id: bind (result x) k = k x
;;   result_bind_id: bind x return = x
;;   bind_bind: bind m (fn [x] (bind (k x) h)) = (bind (bind m k) h)
;;
;; MonadZero T extends Monad:
;;   zero: T α
;;   bind_left_zero: bind zero k = zero
;;
;; MonadFail T extends Monad:
;;   fail: String → T α
;;   bind_left_fail: bind (fail s) k = (fail s)
;;
;; MonadZero T extends Monad T:
;;   plus: T α → T α → T α
;;   plus_left_zero: plus zero α = α
;;   plus_right_zero: plus α zero = α
;;   plus_associativity: (plus α β) γ = plus α (plus β γ)
;;
;;   plus_distributes_bind: bind (plus a b) k = plus (bind a k) (bind b k)
;;   plus_left_catch: plus (result a) b = result a



;; type Input = Stream×IndentStack
;; monad Parser α = Input → List(α×Input)

;; (def zero-input '[() (0)]) ;; no input available, empty indentation stack
(defn accept-fail [in] []) ;; always fail to have any valid parse.
(defn accept-or [& parsers] (fn [in] (mapcat #(% in) parsers)))

(defmonad parser-m
  [ m-result (fn [α] (fn [in] [[α in]]))
    m-bind (fn [Tα fTβ] (fn [in] (mapcat fTβ (Tα in))))
    m-zero accept-fail
    m-plus accept-or
    m-one accept-epsilon ])


;; type Input = Stream×IndentStack
;; monad DeterministicParser α = Input → α×Input
;;   bind: T α → (α→T β) → T β
;;   result: α → T α

(defmonad deterministic-parser-m
  [ m-result (fn [α] (fn [in] [α in]))
    m-bind (fn [Tα fTβ] (fn [in] (let [[α in] (Tα in)] ((fTβ α) in))))
    m-zero (fn [in] (throw :fail))
    m-one (fn [in] [nil in]) ])

(defn m-peek-char [in] [[(in-char in) in]])
(defn m-getpos [in] [[(in-charpos in) in]])
(defn m-read-char [in] [[(in-char in) (rest in)]])

;; type Output = Stream
;; monad Emitter α = α×Output

(defmonad emitter-m
  [ m-result (fn [α] (fn [out] [α out]))
    m-bind (fn [[α out] fTβ] (let [[β more-out] (fTβ α)] [β (append more-out out))))
    m-zero (fn [out] (throw :fail))
    m-plus (fn [& emitters] 

(defn m-out [out token] [[nil (conj out token)]])
(defn m-done [out] [(reverse out) nil])

(defn in-char [in] (let [[[x]] in] x))
(defn in-charpos [in] (let [[[x l c]] in] [x l c]))

(defn accept-python [in]
  (lazy-seq
   (when (in-char in)
     (let [[tokens in] (expect-logical-line in)]
       (concat tokens (expect-python pos-stream indent-stack))))))

(def expect-logical-line
  (domonad parser-m
    [l (expect-leading-whitespace)

(defn python-lex-stream (stream)
  (expect-python [(delta/positioned-stream stream) '(0)]))
