(defproject model "0.1.0-SNAPSHOT"
  :description "Anglican model implementation of game"
  :url "https://github.com/susverwimp/T208-Probabilistic-Programming-A-Case-Study"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [anglican "1.0.0"]]
  :plugins [[lein-gorilla "0.4.0"]]
  :resource-paths ["programs"]
  :main ^:skip-aot anglican.core)
