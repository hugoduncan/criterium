{:doc
 {:dependencies [[codox-md "0.1.0"]]
  :codox {:writer codox-md.writer/write-docs}
  :autodoc {:name "Criterium"
            :description "A benchmarking library."
            :copyright "Copyright Hugo Duncan 2010, 2011, 2012. All rights reserved."
            :web-src-dir "http://github.com/hugoduncan/criterium/blob/"
            :web-home "http://hugoduncan.github.com/criterium/" }}
 :release
 {:plugins [[lein-set-version "0.2.1"]]
  :set-version
  {:updates [{:path "README.md" :no-snapshot true}]}}
 :dev {:aliases {"impl-perf" ["with-profile" "+impl" " perforate" "--quick"]}}
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
