(ns user)

;; require some things that have reflection or boxed warnings
(try
  (require 'aero.alpha.core)
  (catch Exception _))
(try
  (require 'clojure.test.check)
  (catch Exception _))
(try
  (require 'clojure.test.check.clojure-test)
  (catch Exception _))
(try
  (require 'clojure.tools.cli)
  (catch Exception _))
(try
  (require 'clojure.tools.reader)
  (catch Exception _))
(try
  (require 'fipp)
  (catch Exception _))
(try
  (require 'kaocha.plugin.profiling)
  (require 'kaocha.report)
  (require 'kaocha.runner)
  (catch Exception _))
(try
  (require 'lambdaisland.deep-diff2)
  (catch Exception _))
(try
  (require 'nrepl.core)
  (require 'nrepl.middleware)
  (require 'nrepl.middleware.session)
  (catch Exception _))

(alter-var-root #'*unchecked-math* (constantly :warn-on-boxed))
(alter-var-root #'*warn-on-reflection* (constantly true))
