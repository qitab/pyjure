;; skylark.user: a namespace in which to run code.
(ns skylark.user
  (:refer-clojure :include [])
  (:require [clojure.tools.trace :as trace]
            [clojure.repl :as repl])
  (:require [skylark.runtime :as r]
            [skylark.utilities :as u])
  (:use [skylark.runtime :include
         ;; (vec (filter #(= \$ (-> % name first)) (map first (ns-publics 'skylark.runtime))))
         [$NoneType $add $easy-falsity $None $empty-set $not $easy-truth $global $get-method $list $empty-list $True $Boolean $truth $False $just-yield $dict $empty-tuple $class $bytes $initial-environment $set $builtin $empty-dict $tuple]
         ]))
