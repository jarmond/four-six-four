(defproject four-six-four "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "GPL-3.0"
            :url "https://www.gnu.org/licenses/gpl-3.0-standalone.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [byte-streams "0.2.4"]]
  :main ^:skip-aot four-six-four.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})