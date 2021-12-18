(ns criterium.util.ziggurat-test
  (:require
   [clojure.test :refer [is]]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.test-utils :refer [abs-error]]
   [criterium.util.stats :as stats]
   [criterium.util.ziggurat :as ziggurat]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec random-normal-zig-test-property 10
  (prop/for-all
   [random-seed gen/small-integer]
   (let [random-source  (java.util.Random. random-seed)
         values         (->> #(.nextDouble random-source)
                             repeatedly
                             ziggurat/random-normal-zig
                             (take 10000)
                             vec)
         mean           (stats/mean values)
         variance       (stats/variance values)
         mean-error     (abs-error mean 0.0)
         variance-error (abs-error variance 1.0)
         mean-tol       1e-1
         variance-tol   15e-1]
     (is (< mean-error mean-tol))
     (is (< variance-error variance-tol))
     (and (< mean-error mean-tol)
          (< variance-error variance-tol)))))
