(ns criterium.time.spec-test
  (:require
   [clojure.test :as t]
   [criterium.analyse :as analyse]
   [criterium.view :as view]
   [criterium.sample-scheme :as sample-scheme]
   [criterium.time.spec :as time-spec]
   [orchestra.spec.test :as stest]))


(stest/unstrument)

(stest/unstrument
 [`analyse/analyse
  `sample-scheme/sample
  `view/report
  `time/measure])


;; (testing "makes the timing data available with last-time"
;;   (is (s/conform ::pipeline/sample (criterium/last-time))))
