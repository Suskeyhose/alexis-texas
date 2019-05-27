(defproject alexis-texas "0.2.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/core.async "0.4.490"]
                 [com.rpl/specter "1.1.2"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.suskalo/discljord "0.2.4"]]
  :jvm-opts []
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.elide-meta=[:doc :added]"
                                  "-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[org.clojure/test.check "0.9.0"]]}}
  :main alexis-texas.core)
