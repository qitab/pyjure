;; pyjure.user: a namespace in which to run code.
(ns pyjure.user
  (:refer-clojure :include [])
  (:require [clojure.tools.trace :as trace]
            [clojure.repl :as repl])
  (:require [pyjure.runtime :as r]
            [pyjure.utilities :as u])
  (:use [pyjure.runtime :include
         ;; (vec (filter #(= \$ (-> % name first)) (map first (ns-publics 'pyjure.runtime))))
         [$NoneType $add $easy-falsity $None $empty-set $not $easy-truth $global $get-method $list $empty-list $True $Boolean $truth $False $just-yield $dict $empty-tuple $class $bytes $initial-environment $set $builtin $empty-dict $tuple]
         ]))
