(ns criterium.stats.outliers-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.stats.outliers :as outliers]))

;; Tests for outlier detection functions.
;; Tests verify contract: thresholds are computed correctly from quartiles.

(deftest boxplot-outlier-thresholds-test
  (testing "boxplot-outlier-thresholds"
    (testing "returns [severe-low mild-low mild-high severe-high]"
      (is (= [-4.0 -1.0 7.0 10.0] (outliers/boxplot-outlier-thresholds 2.0 4.0))))))
