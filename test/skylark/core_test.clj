(ns skylark.core-test
  (:require [me.raynes.conch :only [programs with-programs let-programs] :as sh])
  (:use [clojure.tools.trace]
        [clojure.tools.nrepl]
        [clojure.repl]
        [clojure.test]
        [skylark.core]))
