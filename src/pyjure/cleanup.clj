(ns pyjure.cleanup
  (:use [clojure.core.match :only [match]]
        [pyjure.debug]
        [pyjure.utilities]))

;; TODO? maintain a lexical environment, resolve bindings,
;;       error on binding any but a local variable.
;; TODO? insert vars for type inference
;; TODO? group together with another phase?

(defn c [x]
  (letfn [(m [f] (copy-meta f x))
          (v [& s] (m (vec s)))
          (w [& s] (m (apply vec* s)))
          (c* [s] (map c s))
          (c-args [[args rarg moreargs kwarg]]
            (v (vec (c* args)) (c rarg) (vec (c* moreargs)) (c kwarg)))
          (flatten [acc x]
            (match [x]
              [[:suite & _]] (reduce flatten acc (rest x))
              [[(:or ':nonlocal ':global) _]] acc
              :else (conj acc (c x))))]
    (when x
      (assert (vector? x))
      (let [[h & s] x]
        (case h
          ;; Most of the language passes through unchanged
          (:id :bind :unbind) x
          (:try) (let [[body excepts else finally] s] (v :try (c body) (c* excepts) (c else) (c finally)))
          (:from :import :constant :break :continue) x
          (:builtin :argument) (let [[f & a] s] (w h f (c* a)))
          (:except) (let [[f & a] s] (w h f (c* a)))
          (:return :raise :while :if :yield :yield-from) (w h (c* s))
          (:call) (let [[f a] s] (v :call (c f) (c-args a)))
          (:class) (let [[name args body] s] (v :class name (c-args args) (c body)))
          ;; Generators: mark them as their own thing.
          ;; If delimited continuations are available, macroexpand to a wrapper that uses them here.
          (:defn)
          (let [[args return-type body] s
                tag (if (:generator? (meta x)) :generator :function)]
            ;; TODO: either use a different code generator, or implement and use delimited continuations
            (v tag (c-args args) (c return-type) (c body)))
          ;; Suites: flatten them. Empty suites are eliminated, even in last position.
          ;; but if the overall suite is still empty, replace it by None.
          ;; This can't be merged into previous pass, because
          ;; (1) if you recurse into suite before you process, you miss the nonlocal and global elimination
          ;; (2) if you recurse into suite after you process, you pay quadratic instead of linear cost
          ;; and miss the elimination of empty suites.
          (:suite) (w :suite (c* s))
          (:module) (w :module (c* s))
          (:nonlocal :global) (v :suite) ;; empty suite, unlike None, can later be merged away.
          ($syntax-error x "unexpected expression %s during cleanup pass"))))))


(defn cleanup- [x]
  (c x))
