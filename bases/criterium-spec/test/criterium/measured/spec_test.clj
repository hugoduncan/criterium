(ns criterium.measured.spec-test
  (:require
   [clojure.spec.test.alpha]
   [criterium.measured :as measured]
   [criterium.measured.spec]
   [orchestra.spec.test :as stest]))

(stest/instrument
 [`measured/expr
  `measured/generate-state
  `measured/invoke
  `measured/measured
  `measured/measured?
  `measured/symbolic])

#_(clojure.spec.test.alpha/check
   [`measured/expr
    `measured/generate-state
    `measured/invoke
    `measured/measured
    `measured/measured?
    `measured/symbolic])
