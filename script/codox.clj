(ns codox
  (:require [codox.main :as codox]))

(defn -main [& _args]
  (codox/generate-docs
    {:source-paths ["src"]
     :output-path "doc/0.5/api"
     :src-dir-uri "https://github.com/hugoduncan/criterium/blob/develop"
     :src-linenum-anchor-prefix "L"}))
