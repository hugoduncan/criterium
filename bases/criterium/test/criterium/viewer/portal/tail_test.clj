(ns criterium.viewer.portal.tail-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.portal.core :as portal.core]
   [criterium.viewer.portal.tail]))

;; Tests multimethod registration, data processing, and output formatting
;; for tail analysis views.
;;
;; Tail analysis includes GPD/Hill parameter estimation, tail ratios
;; (p99/p95, p999/p99), high quantile extrapolation, and related charts.

(deftest tail-summary-portal-test
  ;; Tests the portal viewer output for tail summary (GPD/Hill parameters).
  (testing "tail-summary*"
    (testing "produces table with summary data"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/tail-summary* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (seq rows))
          (is (some #(contains? % :parameter) rows)))))

    (testing "handles missing tail analysis gracefully"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (is (nil? (view/tail-summary* :portal {} {}))))
        (is (empty? @tapped))))

    (testing "uses custom tail-analysis-id"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)
            custom-map {:my-tail (:tail-analysis data-map)}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/tail-summary* :portal {:tail-analysis-id :my-tail} custom-map))
        (is (= 1 (count @tapped)))))))

(deftest tail-ratios-portal-test
  ;; Tests the portal viewer output for tail ratios.
  (testing "tail-ratios*"
    (testing "produces table with ratio data"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/tail-ratios* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (seq rows)))))

    (testing "handles missing tail analysis gracefully"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (is (nil? (view/tail-ratios* :portal {} {}))))
        (is (empty? @tapped))))))

(deftest tail-high-quantiles-portal-test
  ;; Tests the portal viewer output for high quantile estimates.
  (testing "tail-high-quantiles*"
    (testing "produces table with quantile estimates"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/tail-high-quantiles* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (seq rows)))))

    (testing "handles missing tail analysis gracefully"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (is (nil? (view/tail-high-quantiles* :portal {} {}))))
        (is (empty? @tapped))))))

(deftest tail-chart-views-portal-test
  ;; Tests the portal viewer chart views produce Vega-Lite specs.
  (testing "tail chart views"
    (testing "tail-ratios-chart* produces Vega-Lite spec"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/tail-ratios-chart* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "hill-plot* produces Vega-Lite spec"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/hill-plot* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "mrl-plot* produces Vega-Lite spec"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/mrl-plot* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "zipf-plot* produces Vega-Lite spec when samples exist"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/zipf-plot* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "exponential-qq-plot* produces Vega-Lite spec"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/exponential-qq-plot* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "gpd-qq-plot* produces Vega-Lite spec"
      (let [tapped (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (view/gpd-qq-plot* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (is (map? (first @tapped)))))

    (testing "charts return nil when tail analysis not found"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega-lite #(swap! tapped conj %)]
          (is (nil? (view/tail-ratios-chart* :portal {} {})))
          (is (nil? (view/hill-plot* :portal {} {})))
          (is (nil? (view/mrl-plot* :portal {} {})))
          (is (nil? (view/zipf-plot* :portal {} {})))
          (is (nil? (view/exponential-qq-plot* :portal {} {})))
          (is (nil? (view/gpd-qq-plot* :portal {} {}))))
        (is (empty? @tapped))))))
