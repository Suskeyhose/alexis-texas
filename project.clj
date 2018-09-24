(defproject alexis-texas "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474"]
                 [com.rpl/specter "1.1.1"]
                 [discljord "0.1.1"]]
  :jvm-opts ["--add-modules" "java.xml.bind"]
  :profiles {:uberjar {:aot :all}}
  :main alexis-texas.core)
