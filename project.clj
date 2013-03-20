(defproject Manta-Ray "1.0.0-SNAPSHOT"
  :description "Simple ray tracer"
  :jvm-opts ["-Xss16M"]
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/algo.generic "0.1.1"]]
  :main Manta-Ray.core)