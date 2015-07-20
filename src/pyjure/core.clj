;; pyjure.core: this file just combines together all the compilation passes.
(ns pyjure.core
  (:use
   [pyjure.debug]
   [pyjure.passes]
   [pyjure.utilities]
   [pyjure.parsing])
  (:require
   ;; Our passes, in order:
   [leijure.delta-position]
   [pyjure.lexer]
   [pyjure.parser]
   [pyjure.desugar]
   [pyjure.clarification]
   [pyjure.cleanup]
   [pyjure.anormalization]
   [pyjure.continuation-analysis]
   [pyjure.effect-analysis]
   [pyjure.clojurifier]
   [pyjure.eval]
   [pyjure.user]))
