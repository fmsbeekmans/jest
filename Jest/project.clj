(defproject jest "0.0.0-SNAPSHOT"
  :description "Multi touch game for the city of Delft"
  :url "http://www.github.com/fmsbeekmans/Jest.git"
  :license {:name "GPL 3 and higher"
            :url " https://gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [YAJTL/YAJTL "0.0.1"]]
  :repositories {"pievolution" "http://pievolution.org/maven-repo/"}
  :profiles {:dev {:plugins [[lein-midje "3.0.0"]]
                   :dependencies [[midje "1.5.1"]]
                   }
             }
  )
