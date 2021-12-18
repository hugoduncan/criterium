(ns criterium.util.format-test
  (:require
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.util.format :as format]))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec format-non-dimensional-value-test
  (prop/for-all [i gen/small-integer]
                (= (str i) (format/format-value :non-dimensional i))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec format-time-value-ns-test
  (prop/for-all [t (gen/double* {:max 1e-6 :min 0 :NaN? false})]
                (= (format "%3.3g ns" (* t 1e9))
                   (format/format-value :time t))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec format-time-value-us-test
  (prop/for-all [t (gen/double* {:max 1e-3 :min 1e-6 :NaN? false})]
                (= (format "%3.3g µs" (* t 1e6))
                   (format/format-value :time t))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec format-time-value-ms-test
  (prop/for-all [t (gen/double* {:max (- 1 (Math/ulp 1.0)) :min 1e-3 :NaN? false})]
                (= (format "%3.3g ms" (* t 1e3))
                   (format/format-value :time t))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec format-time-value-s-test
  (prop/for-all [t (gen/double* {:max 60 :min 1 :NaN? false})]
                (= (format "%3.3g s" t)
                   (format/format-value :time t))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defspec format-time-value-min-test
  (prop/for-all [t (gen/double* {:min       (+ 60 (Math/ulp 60.0))
                                 :infinite? false
                                 :NaN?      false})]
                (= (format "%3.3g min" (/ t 60.0))
                   (format/format-value :time t))))
