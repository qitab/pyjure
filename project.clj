(defproject skylark "0.0.1-SNAPSHOT"
  :description "Experimental implementation of Skylark in Clojure"
  :url "TBD"
  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/algo.monads "0.1.4"]
                 ;;[leijure/delta-position "0.1.0-SNAPSHOT"]

                 ;;; Not using these, but experimenting...
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/core.match "0.2.2-SNAPSHOT"]
                 [matchure "0.10.1"]
                 ;;[nconc "1.0.0-SNAPSHOT"]
                 [instaparse "1.3.2"]
                 [factual/fnparse "2.3.0"]
                 ;;[the/parsatron "0.0.8-SNAPSHOT"]
                 [org.van-clj/zetta-parser "0.0.4"]
                 [squarepeg "0.6.1"]]
  :repositories { "sonatype" {:url "https://oss.sonatype.org/content/repositories/snapshots/"}})
