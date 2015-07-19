(defproject pyjure "0.0.1-SNAPSHOT"
  :description "Pyjure, a pure dialect of Python, as implemented in Clojure"
  ;; :url "TBD"
  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/"}
  :main pyjure.core
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [leijure/delta-position "0.1.0.1"]
                 [org.clojure/math.numeric-tower "0.0.4"]

                 ;;; TODO: actually use these
                 [com.ibm.icu/icu4j "52.1"] ;; add Unicode character support to the lexer

                 ;;; Not using these, but experimenting...
                 [org.clojure/tools.trace "0.7.8"]
                 [org.clojure/core.match "0.2.2-SNAPSHOT"]
                 [org.clojure/tools.analyzer "0.3.0"]
                 [org.clojure/tools.analyzer.jvm "0.3.0"]
                 ;;[org.clojure/algo.monads "0.1.4"]
                 ;;[matchure "0.10.1"]
                 ;;[nconc "1.0.0-SNAPSHOT"]
                 ;;[instaparse "1.3.2"]
                 ;;[factual/fnparse "2.3.0"]
                 ;;[the/parsatron "0.0.8-SNAPSHOT"]
                 ;;[org.van-clj/zetta-parser "0.0.4"]
                 ;;[squarepeg "0.6.1"]
                 ]
  :repositories {"sonatype" {:url "https://oss.sonatype.org/content/repositories/snapshots/"}}
  :resource-paths ["resources/prod"]
  :aot :all
  :profiles {:uberjar {:aot :all}
             :dev {:resource-paths ["resources/test"]
                   :dependencies [[me.raynes/conch "0.8.0"]
                                  [cider/cider-nrepl "0.9.0"]]
                   :repl-options {:nrepl-middleware
                                  [cider.nrepl.middleware.classpath/wrap-classpath
                                   ;;cider.nrepl.middleware.complete/wrap-complete ;; messes up nrepl-mode
                                   cider.nrepl.middleware.info/wrap-info
                                   cider.nrepl.middleware.inspect/wrap-inspect
                                   cider.nrepl.middleware.macroexpand/wrap-macroexpand
                                   cider.nrepl.middleware.stacktrace/wrap-stacktrace
                                   cider.nrepl.middleware.test/wrap-test
                                   cider.nrepl.middleware.trace/wrap-trace
                                   ;; cider.nrepl.middleware.undef/wrap-undef ;; fails to load
                                   ]}}})
