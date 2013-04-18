{:doc
 {:dependencies [[codox-md "0.1.0"]]
  :codox {:writer codox-md.writer/write-docs
          :output-dir "doc/0.4/api"
          :src-dir-uri "https://github.com/hugoduncan/criterium/blob/develop"
          :src-linenum-anchor-prefix "L"}
  :aliases {"marg" ["marg" "-d" "doc/0.4/"]
            "codox" ["doc"]
            "doc" ["do" "codox," "marg"]}}
 :release
 {:plugins [[lein-set-version "0.3.0"]]
  :set-version
  {:updates [{:path "README.md" :no-snapshot true}]}}
 :dev {:aliases {"impl-perf" ["with-profile" "+impl" " perforate" "--quick"]}
       :plugins [[codox/codox.leiningen "0.6.4"]
                 [lein-marginalia "0.7.1"]]}
 :impl {:perforate
        {:environments
         [{:name :array
           :namespaces [criterium.implementation]
           :fixtures [criterium.implementation.array/with-array]}
          {:name :volatile
           :namespaces [criterium.implementation]
           :fixtures [criterium.implementation.type/with-volatile]}
          {:name :unsynchronized
           :namespaces [criterium.implementation]
           :fixtures [criterium.implementation.type/with-unsynchronized]}]}}}
