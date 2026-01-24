(ns criterium.viewer.kindly.shape-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly.core :as kindly.core]
   [criterium.viewer.kindly.shape]))

;; Tests multimethod registration, data processing, and output formatting
;; for shape statistics views in kindly viewer.
;;
;; Shape stats include skewness, kurtosis, and coefficient of variation (CV)
;; with their classifications.

(deftest shape-stats-kindly-test
  (testing "shape-stats*"
    (testing "produces table with shape statistics"
      (let [tables (atom [])
            data-map (test-data/bootstrap-stats-with-shape-map)]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-table #(swap! tables conj %)]
          (view/shape-stats* :kindly {} data-map))
        (is (= 1 (count @tables)))
        (let [rows (first @tables)]
          (is (seq rows))
          (is (some #(contains? % :metric) rows))
          (is (some #(contains? % :skewness) rows))
          (is (some #(contains? % :skewness-interpretation) rows))
          (is (some #(contains? % :kurtosis) rows))
          (is (some #(contains? % :kurtosis-interpretation) rows))
          (is (some #(contains? % :cv) rows))
          (is (some #(contains? % :cv-interpretation) rows)))))

    (testing "handles missing bootstrap data gracefully"
      (let [tables (atom [])]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-table #(swap! tables conj %)]
          (is (nil? (view/shape-stats* :kindly {} {}))))
        (is (empty? @tables))))

    (testing "uses custom bootstrap-stats-id"
      (let [tables (atom [])
            data-map (test-data/bootstrap-stats-with-shape-map)
            custom-map {:my-bootstrap (:bootstrap-stats data-map)}]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-table #(swap! tables conj %)]
          (view/shape-stats* :kindly {:bootstrap-stats-id :my-bootstrap} custom-map))
        (is (= 1 (count @tables)))))))
