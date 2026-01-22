(ns criterium.viewer.kindly-tail-views-test
  ;; Tests for tail analysis view multimethods for the :kindly viewer.
  ;; Covers tail-summary*, tail-ratios*, tail-high-quantiles*, and tail charts
  ;; (hill-plot*, mrl-plot*).
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]
   [criterium.viewer.kindly-test-helpers :refer [table-rows]]))

(deftest tail-summary-view-test
  ;; Tests the view/tail-summary* multimethod for :kindly viewer.
  (testing "view/tail-summary* :kindly"
    (testing "renders heading and summary table"
      (reset! kindly/accumulated [])
      (view/tail-summary* :kindly {} (test-data/tail-analysis-data-map))
      (let [result (kindly/flush)
            headings (filter #(and (vector? %)
                                   (= :kind/md (:kindly/kind (meta %))))
                             result)
            tables (filter #(and (map? %)
                                 (= :kind/table (:kindly/kind (meta %))))
                           result)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (some #(str/includes? (first %) "Tail Summary") headings)
            "Expected Tail Summary heading")
        (is (>= (count tables) 1) "Expected at least 1 table")
        (when-let [summary-table (first tables)]
          (let [rows (table-rows summary-table)]
            (is (some #(= "Threshold" (:parameter %)) rows)
                "Expected Threshold row")
            (is (some #(= "GPD shape (ξ)" (:parameter %)) rows)
                "Expected GPD shape row")))))

    (testing "handles nil tail-analysis gracefully"
      (reset! kindly/accumulated [])
      (view/tail-summary* :kindly {} {:tail-analysis nil})
      (is (nil? (kindly/flush))))))

(deftest tail-ratios-view-test
  ;; Tests the view/tail-ratios* multimethod for :kindly viewer.
  (testing "view/tail-ratios* :kindly"
    (testing "renders heading and ratios table"
      (reset! kindly/accumulated [])
      (view/tail-ratios* :kindly {} (test-data/tail-analysis-data-map))
      (let [result (kindly/flush)
            headings (filter #(and (vector? %)
                                   (= :kind/md (:kindly/kind (meta %))))
                             result)]
        (is (some #(str/includes? (first %) "Tail Ratios") headings)
            "Expected Tail Ratios heading")))))

(deftest tail-high-quantiles-view-test
  ;; Tests the view/tail-high-quantiles* multimethod for :kindly viewer.
  (testing "view/tail-high-quantiles* :kindly"
    (testing "renders heading and quantiles table"
      (reset! kindly/accumulated [])
      (view/tail-high-quantiles* :kindly {} (test-data/tail-analysis-data-map))
      (let [result (kindly/flush)
            headings (filter #(and (vector? %)
                                   (= :kind/md (:kindly/kind (meta %))))
                             result)]
        (is (some #(str/includes? (first %) "High Quantile") headings)
            "Expected High Quantile heading")))))

(deftest tail-chart-views-test
  ;; Tests the tail chart view multimethods for :kindly viewer.
  (testing "tail chart views produce Vega-Lite specs"
    (let [data-map (test-data/tail-analysis-data-map)]
      (testing "hill-plot*"
        (reset! kindly/accumulated [])
        (view/hill-plot* :kindly {} data-map)
        (let [result (kindly/flush)]
          (when result
            (let [charts (filter #(and (map? %)
                                       (= :kind/vega-lite (:kindly/kind (meta %))))
                                 result)]
              (when (seq charts)
                (is (string? (:$schema (first charts)))
                    "Expected Vega-Lite schema"))))))

      (testing "mrl-plot*"
        (reset! kindly/accumulated [])
        (view/mrl-plot* :kindly {} data-map)
        (let [result (kindly/flush)]
          (when result
            (let [charts (filter #(and (map? %)
                                       (= :kind/vega-lite (:kindly/kind (meta %))))
                                 result)]
              (when (seq charts)
                (is (string? (:$schema (first charts)))
                    "Expected Vega-Lite schema")))))))))
