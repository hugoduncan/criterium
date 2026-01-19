(ns criterium.viewer.common.core-tail-test
  ;; Tests for tail analysis table preparation functions in common.core.
  ;; Verifies correct formatting of GPD/Hill summary, tail ratios, and
  ;; high quantiles for display in portal and kindly viewers.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common.core :as core]))

(def identity-transforms
  "Identity transforms for testing - `:sample->` must be a list of functions."
  {:sample-> (list identity)
   :->sample [identity]})

;;; Tail Summary Table Tests

(deftest tail-summary-table-test
  ;; Tests tail-summary-table which prepares GPD/Hill parameters for display.
  ;; Verifies threshold formatting, exceedances count, and parameter rows.
  (testing "tail-summary-table"
    (testing "includes all parameters when present"
      (let [tail-data {:threshold 1.5e-6  ; 1.5 microseconds in seconds
                       :gpd {:xi 0.3
                             :sigma 0.5e-6
                             :exceedances-count 50}
                       :hill {:stable-estimate 0.82
                              :k-range [10 20 30 40 50]
                              :estimates [0.8 0.85 0.82 0.81 0.83]}}
            result (core/tail-summary-table tail-data identity-transforms)]
        (is (= 5 (count result)) "Expected 5 rows")
        (is (some #(= "Threshold" (:parameter %)) result))
        (is (some #(= "Exceedances (k)" (:parameter %)) result))
        (is (some #(= "GPD shape (ξ)" (:parameter %)) result))
        (is (some #(= "GPD scale (σ)" (:parameter %)) result))
        (is (some #(= "Hill estimate" (:parameter %)) result))))

    (testing "formats threshold with SI units"
      (let [tail-data {:threshold 1.5e-6  ; nanoseconds
                       :gpd {:xi 0.3 :sigma 0.5e-6 :exceedances-count 50}
                       :hill {:stable-estimate 0.82 :k-range [10 20]}}
            result (core/tail-summary-table tail-data identity-transforms)
            threshold-row (first (filter #(= "Threshold" (:parameter %)) result))]
        (is (some? threshold-row))
        (is (string? (:value threshold-row)))))

    (testing "includes k-range in Hill estimate"
      (let [tail-data {:threshold 1e-6
                       :gpd {:xi 0.3 :sigma 0.5e-6 :exceedances-count 50}
                       :hill {:stable-estimate 0.82 :k-range [10 20 30]}}
            result (core/tail-summary-table tail-data identity-transforms)
            hill-row (first (filter #(= "Hill estimate" (:parameter %)) result))]
        (is (some? hill-row))
        ;; Should include k-range bounds
        (is (re-find #"k: \d+-\d+" (:value hill-row)))))

    (testing "handles missing optional fields"
      (let [tail-data {:gpd {:xi 0.3}}
            result (core/tail-summary-table tail-data identity-transforms)]
        (is (= 1 (count result)) "Expected only GPD shape row")
        (is (= "GPD shape (ξ)" (:parameter (first result))))))

    (testing "returns empty vector when no data present"
      (let [tail-data {}
            result (core/tail-summary-table tail-data identity-transforms)]
        (is (empty? result))))))

;;; Tail Ratios Table Tests

(deftest tail-ratios-table-data-test
  ;; Tests tail-ratios-table-data which prepares tail ratios for display.
  ;; Verifies ratio formatting and inclusion of percentile values.
  (testing "tail-ratios-table-data"
    (testing "includes all ratios when present"
      (let [tail-data {:tail-ratios {:p99-p95 1.5
                                     :p999-p99 1.8
                                     :p999-p95 2.7}
                       :empirical-quantiles {:p95 10.0
                                             :p99 15.0
                                             :p999 27.0}}
            result (core/tail-ratios-table-data tail-data)]
        (is (= 3 (count result)) "Expected 3 ratio rows")
        (is (some #(= "p99/p95" (:ratio %)) result))
        (is (some #(= "p999/p99" (:ratio %)) result))
        (is (some #(= "p999/p95" (:ratio %)) result))))

    (testing "formats ratio values with 3 decimal places"
      (let [tail-data {:tail-ratios {:p99-p95 1.5678}
                       :empirical-quantiles {:p95 10.0 :p99 15.0}}
            result (core/tail-ratios-table-data tail-data)
            row (first result)]
        (is (= "1.568" (:value row)))))

    (testing "includes percentile values in row"
      (let [tail-data {:tail-ratios {:p99-p95 1.5}
                       :empirical-quantiles {:p95 10.0 :p99 15.0}}
            result (core/tail-ratios-table-data tail-data)
            row (first result)]
        (is (some? (:p95 row)))
        (is (some? (:p99 row)))))

    (testing "handles partial ratios"
      (let [tail-data {:tail-ratios {:p99-p95 1.5}
                       :empirical-quantiles {:p95 10.0 :p99 15.0}}
            result (core/tail-ratios-table-data tail-data)]
        (is (= 1 (count result)))))

    (testing "returns empty vector when no ratios present"
      (let [tail-data {:tail-ratios {}
                       :empirical-quantiles {:p95 10.0}}
            result (core/tail-ratios-table-data tail-data)]
        (is (empty? result))))))

;;; High Quantiles Table Tests

(deftest tail-high-quantiles-table-test
  ;; Tests tail-high-quantiles-table which prepares high quantile estimates.
  ;; Verifies SI unit formatting and quantile label generation.
  (testing "tail-high-quantiles-table"
    (testing "formats quantiles with SI units"
      (let [tail-data {:high-quantiles {0.99 1.5e-6
                                        0.999 2.0e-6
                                        0.9999 3.0e-6}}
            result (core/tail-high-quantiles-table tail-data identity-transforms)]
        (is (= 3 (count result)) "Expected 3 quantile rows")
        (is (every? #(contains? % :quantile) result))
        (is (every? #(contains? % :estimate) result))
        ;; Estimates should be formatted strings
        (is (every? #(string? (:estimate %)) result))))

    (testing "sorts quantiles in ascending order"
      (let [tail-data {:high-quantiles {0.9999 3.0e-6
                                        0.99 1.5e-6
                                        0.999 2.0e-6}}
            result (core/tail-high-quantiles-table tail-data identity-transforms)
            quantiles (mapv :quantile result)]
        ;; Quantiles formatted as p%.4g which gives e.g. p99.00, p99.90, p99.99
        (is (= ["p99.00" "p99.90" "p99.99"] quantiles))))

    (testing "handles missing high-quantiles"
      (let [tail-data {}
            result (core/tail-high-quantiles-table tail-data identity-transforms)]
        (is (nil? result))))

    (testing "handles empty high-quantiles"
      (let [tail-data {:high-quantiles {}}
            result (core/tail-high-quantiles-table tail-data identity-transforms)]
        (is (nil? result))))))
