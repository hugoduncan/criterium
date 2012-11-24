(defproject criterium "0.3.1-SNAPSHOT"
  :description "Benchmarking library"
  :url "https://github.com/hugoduncan/criterium"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :local-repo-classpath true
  :codox {:writer codox-md.writer/write-docs}
  :autodoc {:name "Criterium"
            :description "A benchmarking library."
            :copyright "Copyright Hugo Duncan 2010, 2011, 2012. All rights reserved."
            :web-src-dir "http://github.com/hugoduncan/criterium/blob/"
            :web-home "http://hugoduncan.github.com/criterium/" })
