(ns criterium.viewer.kindly.tail-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly.core :as kindly.core]
   [criterium.viewer.kindly.tail]))

;; Tests multimethod registration, data processing, and output formatting
;; for tail analysis views.
;;
;; Tail analysis includes GPD/Hill parameter estimation, tail ratios
;; (p99/p95, p999/p99), high quantile extrapolation, and related charts.

(deftest tail-summary-kindly-test
  ;; Tests the kindly viewer output for tail summary (GPD/Hill parameters).
  (testing "tail-summary*"
    (testing "produces table with summary data"
      (let [tables (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading
                      (fn [_])
                      kindly.core/kindly-table
                      (fn [rows & _] (swap! tables conj rows))]
          (view/tail-summary* :kindly {} data-map))
        (is (= 1 (count @tables)))
        (let [rows (first @tables)]
          (is (seq rows))
          (is (some #(contains? % :parameter) rows)))))

    (testing "handles missing tail analysis gracefully"
      (let [tables (atom [])]
        (with-redefs [kindly.core/kindly-heading
                      (fn [_])
                      kindly.core/kindly-table
                      (fn [rows & _] (swap! tables conj rows))]
          (is (nil? (view/tail-summary* :kindly {} {}))))
        (is (empty? @tables))))

    (testing "uses custom tail-analysis-id"
      (let [tables (atom [])
            data-map (test-data/tail-analysis-data-map)
            custom-map {:my-tail (:tail-analysis data-map)}]
        (with-redefs [kindly.core/kindly-heading
                      (fn [_])
                      kindly.core/kindly-table
                      (fn [rows & _] (swap! tables conj rows))]
          (view/tail-summary* :kindly {:tail-analysis-id :my-tail} custom-map))
        (is (= 1 (count @tables)))))))

(deftest tail-ratios-kindly-test
  ;; Tests the kindly viewer output for tail ratios.
  (testing "tail-ratios*"
    (testing "produces table with ratio data"
      (let [tables (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading
                      (fn [_])
                      kindly.core/kindly-table
                      (fn [rows & _] (swap! tables conj rows))]
          (view/tail-ratios* :kindly {} data-map))
        (is (= 1 (count @tables)))
        (let [rows (first @tables)]
          (is (seq rows)))))

    (testing "handles missing tail analysis gracefully"
      (let [tables (atom [])]
        (with-redefs [kindly.core/kindly-heading
                      (fn [_])
                      kindly.core/kindly-table
                      (fn [rows & _] (swap! tables conj rows))]
          (is (nil? (view/tail-ratios* :kindly {} {}))))
        (is (empty? @tables))))))

(deftest tail-high-quantiles-kindly-test
  ;; Tests the kindly viewer output for high quantile estimates.
  (testing "tail-high-quantiles*"
    (testing "produces table with quantile estimates"
      (let [tables (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading
                      (fn [_])
                      kindly.core/kindly-table
                      (fn [rows & _] (swap! tables conj rows))]
          (view/tail-high-quantiles* :kindly {} data-map))
        (is (= 1 (count @tables)))
        (let [rows (first @tables)]
          (is (seq rows)))))

    (testing "handles missing tail analysis gracefully"
      (let [tables (atom [])]
        (with-redefs [kindly.core/kindly-heading
                      (fn [_])
                      kindly.core/kindly-table
                      (fn [rows & _] (swap! tables conj rows))]
          (is (nil? (view/tail-high-quantiles* :kindly {} {}))))
        (is (empty? @tables))))))

(deftest tail-chart-views-kindly-test
  ;; Tests the kindly viewer chart views produce Vega-Lite specs.
  (testing "tail chart views"
    (testing "tail-ratios-chart* produces Vega-Lite spec"
      (let [charts (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-vega-lite #(swap! charts conj %)]
          (view/tail-ratios-chart* :kindly {} data-map))
        (is (= 1 (count @charts)))
        (is (map? (first @charts)))
        (is (contains? (first @charts) :width))))

    (testing "hill-plot* produces Vega-Lite spec"
      (let [charts (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-vega-lite #(swap! charts conj %)]
          (view/hill-plot* :kindly {} data-map))
        (is (= 1 (count @charts)))
        (is (map? (first @charts)))
        (is (contains? (first @charts) :width))))

    (testing "mrl-plot* produces Vega-Lite spec"
      (let [charts (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-vega-lite #(swap! charts conj %)]
          (view/mrl-plot* :kindly {} data-map))
        (is (= 1 (count @charts)))
        (is (map? (first @charts)))
        (is (contains? (first @charts) :width))))

    (testing "zipf-plot* produces Vega-Lite spec when samples exist"
      (let [charts (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-vega-lite #(swap! charts conj %)]
          (view/zipf-plot* :kindly {} data-map))
        (is (= 1 (count @charts)))
        (is (map? (first @charts)))
        (is (contains? (first @charts) :width))))

    (testing "exponential-qq-plot* produces Vega-Lite spec"
      (let [charts (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-vega-lite #(swap! charts conj %)]
          (view/exponential-qq-plot* :kindly {} data-map))
        (is (= 1 (count @charts)))
        (is (map? (first @charts)))
        (is (contains? (first @charts) :width))))

    (testing "gpd-qq-plot* produces Vega-Lite spec"
      (let [charts (atom [])
            data-map (test-data/tail-analysis-data-map)]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-vega-lite #(swap! charts conj %)]
          (view/gpd-qq-plot* :kindly {} data-map))
        (is (= 1 (count @charts)))
        (is (map? (first @charts)))
        (is (contains? (first @charts) :width))))

    (testing "charts return nil when tail analysis not found"
      (let [charts (atom [])]
        (with-redefs [kindly.core/kindly-heading (fn [_])
                      kindly.core/kindly-vega-lite #(swap! charts conj %)]
          (is (nil? (view/tail-ratios-chart* :kindly {} {})))
          (is (nil? (view/hill-plot* :kindly {} {})))
          (is (nil? (view/mrl-plot* :kindly {} {})))
          (is (nil? (view/zipf-plot* :kindly {} {})))
          (is (nil? (view/exponential-qq-plot* :kindly {} {})))
          (is (nil? (view/gpd-qq-plot* :kindly {} {}))))
        (is (empty? @charts))))))
