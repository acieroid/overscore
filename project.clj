(defproject overscore "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/tools.logging "0.2.3"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [overtone "0.7.1"]
                 [incanter "1.5.0-SNAPSHOT"]
                 [org.encog/encog-core "3.1.0"]]
  :offline? true
  :main overscore.main)
