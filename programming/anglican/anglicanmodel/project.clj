(defproject anglicanmodel "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [anglican "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :java-source-paths ["src/ox_captcha"]
  :main ^:skip-aot anglicanmodel.model
  :jvm-opts ["-Xmx6g" "-Xms4g"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})