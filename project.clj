(defproject criterium "0.0.1-SNAPSHOT"
  :description "Benchmarking library"
  :url "http://github.com/hugoduncan/criterium"
  :dependencies [[org.clojure/clojure "1.2.0-master-SNAPSHOT"]]
  :dev-dependencies [[leiningen/lein-swank "1.1.0"]
		     [org.clojure/clojure-contrib "1.2.0-master-SNAPSHOT"]
                     [autodoc "0.7.0"]]
  :autodoc {:name "Criterium"
	    :description "A benchmarking library."
	    :copyright "Copyright Hugo Duncan 2010. All rights reserved."
	    :web-src-dir "http://github.com/hugoduncan/criterium/blob/"
	    :web-home "http://hugoduncan.github.com/criterium/" })
