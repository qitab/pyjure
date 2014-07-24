(ns skylark.runtime
  (:require [clojure.core :as c])
  (:require [clojure.string :as str])
  (:require [clojure.set :as set])
  (:use [clojure.core.match :only [match]]))

(def initial-environment {})
