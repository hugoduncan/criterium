(ns criterium.viewer.portal.shape-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.portal.core :as portal.core]
   [criterium.viewer.portal.shape]))

;; Tests multimethod registration, data processing, and output formatting
;; for shape statistics views in portal viewer.
;;
;; Shape stats include skewness, kurtosis, and coefficient of variation (CV)
;; with their classifications.

(deftest shape-stats-portal-test
  (testing "shape-stats*"
    (testing "produces table with shape statistics"
      (let [tapped (atom [])
            data-map (test-data/bootstrap-stats-with-shape-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/shape-stats* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (seq rows))
          (is (some #(contains? % :metric) rows))
          (is (some #(contains? % :skewness) rows))
          (is (some #(contains? % :skewness-interpretation) rows))
          (is (some #(contains? % :kurtosis) rows))
          (is (some #(contains? % :kurtosis-interpretation) rows))
          (is (some #(contains? % :cv) rows))
          (is (some #(contains? % :cv-interpretation) rows)))))

    (testing "handles missing bootstrap data gracefully"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (is (nil? (view/shape-stats* :portal {} {}))))
        (is (empty? @tapped))))

    (testing "uses custom bootstrap-stats-id"
      (let [tapped (atom [])
            data-map (test-data/bootstrap-stats-with-shape-map)
            custom-map {:my-bootstrap (:bootstrap-stats data-map)}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/shape-stats*
           :portal
           {:bootstrap-stats-id :my-bootstrap}
           custom-map))
        (is (= 1 (count @tapped)))))))
