(ns criterium.viewer.kindly-test
  ;; Tests for the Kindly viewer accumulator and flush mechanism.
  ;; Verifies that values accumulate correctly, flush returns a kind/fragment,
  ;; and the accumulator clears after flush.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]))

(deftest kindly-add-test
  (testing "kindly-add"
    (testing "appends values to the accumulator"
      (reset! kindly/accumulated [])
      (kindly/kindly-add {:a 1})
      (kindly/kindly-add {:b 2})
      (is (= [{:a 1} {:b 2}] @kindly/accumulated)))

    (testing "returns nil"
      (reset! kindly/accumulated [])
      (is (nil? (kindly/kindly-add {:x 1}))))))

(deftest kindly-heading-test
  (testing "kindly-heading"
    (testing "adds markdown heading with :kind/md metadata"
      (reset! kindly/accumulated [])
      (kindly/kindly-heading "Test Section")
      (let [result (first @kindly/accumulated)]
        (is (= ["**Test Section**"] result))
        (is (= :kind/md (:kindly/kind (meta result))))))

    (testing "returns nil"
      (reset! kindly/accumulated [])
      (is (nil? (kindly/kindly-heading "Heading"))))))

(deftest kindly-table-test
  (testing "kindly-table"
    (testing "adds table data with :kind/table metadata"
      (reset! kindly/accumulated [])
      (let [table-data [{:col1 "a" :col2 1}]]
        (kindly/kindly-table table-data)
        (let [result (first @kindly/accumulated)]
          (is (= table-data result))
          (is (= :kind/table (:kindly/kind (meta result)))))))

    (testing "returns nil"
      (reset! kindly/accumulated [])
      (is (nil? (kindly/kindly-table []))))))

(deftest kindly-vega-lite-test
  (testing "kindly-vega-lite"
    (testing "adds Vega-Lite spec with :kind/vega-lite metadata and schema"
      (reset! kindly/accumulated [])
      (let [spec {:data {:values []} :mark "point"}]
        (kindly/kindly-vega-lite spec)
        (let [result (first @kindly/accumulated)]
          (is (= "https://vega.github.io/schema/vega-lite/v5.json"
                 (:$schema result)))
          (is (= {:values []} (:data result)))
          (is (= "point" (:mark result)))
          (is (= :kind/vega-lite (:kindly/kind (meta result)))))))

    (testing "returns nil"
      (reset! kindly/accumulated [])
      (is (nil? (kindly/kindly-vega-lite {}))))))

(deftest flush-test
  (testing "flush"
    (testing "returns accumulated values as kind/fragment"
      (reset! kindly/accumulated [])
      (kindly/kindly-add {:a 1})
      (kindly/kindly-add {:b 2})
      (let [result (kindly/flush)]
        (is (= [{:a 1} {:b 2}] result))
        (is (= :kind/fragment (:kindly/kind (meta result))))))

    (testing "clears accumulator after flush"
      (reset! kindly/accumulated [])
      (kindly/kindly-add {:x 1})
      (kindly/flush)
      (is (= [] @kindly/accumulated)))

    (testing "returns nil when accumulator is empty"
      (reset! kindly/accumulated [])
      (is (nil? (kindly/flush))))))

(deftest flush-viewer-test
  (testing "flush-viewer :kindly"
    (testing "dispatches to flush function"
      (reset! kindly/accumulated [])
      (kindly/kindly-heading "Section")
      (kindly/kindly-table [{:a 1}])
      (let [result (view/flush-viewer :kindly)]
        (is (= 2 (count result)))
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= [] @kindly/accumulated))))))

(deftest stats-view-test
  ;; Tests the view/stats* multimethod for :kindly viewer.
  ;; Verifies that stats data is rendered as a heading and table with correct
  ;; Kindly metadata and column structure.
  (testing "view/stats* :kindly"
    (testing "renders stats as heading and table with bench-stats-map"
      (reset! kindly/accumulated [])
      (view/stats* :kindly {} (:data (test-data/bench-stats-map)))
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Summary stats**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= [{:_metric           "Elapsed Time ns",
                   :mean              100.0
                   :min-val           89.0
                   :mean-minus-3sigma 88.0
                   :mean-plus-3sigma  112.0
                   :max-val           114.0}]
                 table)))))

    (testing "renders stats via analyse pipeline"
      (reset! kindly/accumulated [])
      (let [data-map   (:data (test-data/samples-with-2-values-map))
            stats      (analyse/stats)
            view-stats (view/stats)]
        (->> data-map
             stats
             (view-stats :kindly))
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= [{:_metric           "Elapsed Time ns",
                     :mean              1.00,
                     :min-val           1.00,
                     :mean-minus-3sigma 1.00,
                     :mean-plus-3sigma  1.00,
                     :max-val           1.00}]
                   table))))))))

(deftest quantiles-view-test
  ;; Tests the view/quantiles* multimethod for :kindly viewer.
  ;; Verifies that quantiles data is rendered as a heading and table with correct
  ;; Kindly metadata and structure.
  (testing "view/quantiles* :kindly"
    (testing "renders quantiles as heading and table"
      (reset! kindly/accumulated [])
      (view/quantiles* :kindly {} (:data (test-data/quantiles-map)))
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Quantiles**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 1 (count table))
              "Expected one metric row")
          (is (string? (:metric (first table)))
              "Expected :metric column")
          (is (number? (get (first table) 0.5))
              "Expected numeric 0.5 quantile"))))))

(deftest outlier-counts-view-test
  ;; Tests the view/outlier-counts* multimethod for :kindly viewer.
  ;; Verifies that outlier counts data is rendered as heading and table with
  ;; correct Kindly metadata and outlier type columns.
  (testing "view/outlier-counts* :kindly"
    (testing "renders outliers as heading and table"
      (reset! kindly/accumulated [])
      (view/outlier-counts* :kindly {} (:data (test-data/outlier-count-map)))
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Outliers**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= [{:_metric     "Elapsed Time"
                   :low-severe  0
                   :low-mild    2
                   :high-mild   3
                   :high-severe 0}]
                 table)))))))

(deftest collect-plan-view-test
  ;; Tests the view/collect-plan* multimethod for :kindly viewer.
  ;; Verifies that collect plan data is rendered as heading and table with
  ;; correct Kindly metadata and phase information.
  (testing "view/collect-plan* :kindly"
    (testing "renders collect plan as heading and table"
      (reset! kindly/accumulated [])
      (view/collect-plan* :kindly {} (:data (test-data/collect-plan-map)))
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Collect plan**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 3 (count table))
              "Expected 3 phases: sample, warmup, estimation")
          (is (= #{:sample :warmup :estimation}
                 (set (map :phase table)))
              "Expected all phases present"))))))

(deftest samples-view-test
  ;; Tests the view/samples* multimethod for :kindly viewer.
  ;; Verifies that samples data is rendered as a heading and Vega-Lite scatter
  ;; plot with outlier coloring.
  (testing "view/samples* :kindly"
    (testing "renders samples as heading and Vega-Lite chart"
      (reset! kindly/accumulated [])
      (view/samples* :kindly {} (:data (test-data/samples-with-2-values-map)))
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and chart")
        (let [[heading chart] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Samples**"] heading))
          (is (= :kind/vega-lite (:kindly/kind (meta chart))))
          (is (string? (:$schema chart))
              "Expected Vega-Lite schema")
          (is (= [{:elapsed-time 1.0 :index 0 :outlier ""}
                  {:elapsed-time 1.0 :index 1 :outlier ""}]
                 (-> chart :vconcat first :layer first :data :values))))))

    (testing "renders samples with transformed data"
      (reset! kindly/accumulated [])
      (view/samples* :kindly {} (:data (test-data/samples-with-transformed-values-map)))
      (let [result (kindly/flush)
            [_heading chart] result]
        (is (= [{:elapsed-time 1.0 :index 0 :outlier ""}
                {:elapsed-time 2.0 :index 1 :outlier ""}
                {:elapsed-time 4.0 :index 2 :outlier ""}]
               (-> chart :vconcat first :layer first :data :values)))))

    (testing "renders samples with outlier coloring via analyse pipeline"
      (reset! kindly/accumulated [])
      (let [data-map  (:data (test-data/samples-with-outliers-values-map))
            quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            outliers  (analyse/outliers)
            stats     (analyse/stats)
            view      (view/samples)]
        (->> data-map
             quantiles
             outliers
             stats
             (view :kindly))
        (let [result (kindly/flush)
              [_heading chart] result]
          (is (= [{:elapsed-time 9.0 :index 0 :outlier ""}
                  {:elapsed-time 10.0 :index 1 :outlier ""}
                  {:elapsed-time 9.0 :index 2 :outlier ""}
                  {:elapsed-time 10.0 :index 3 :outlier ""}
                  {:elapsed-time 9.0 :index 4 :outlier ""}
                  {:elapsed-time 10.0 :index 5 :outlier ""}
                  {:elapsed-time 10000.0 :index 6 :outlier :high-severe}]
                 (-> chart :vconcat first :layer first :data :values))))))))

(deftest histogram-view-test
  ;; Tests the view/histogram* multimethod for :kindly viewer.
  ;; Verifies that histogram data is rendered as a heading and Vega-Lite bar
  ;; chart with optional normal PDF overlay.
  (testing "view/histogram* :kindly"
    (testing "renders histogram as heading and Vega-Lite chart"
      (reset! kindly/accumulated [])
      (let [data-map       (:data (test-data/samples-with-outliers-values-map))
            quantiles      (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            outliers       (analyse/outliers)
            stats          (analyse/stats)
            histogram      (analyse/histogram)
            view-histogram (view/histogram)]
        (->> data-map
             quantiles
             outliers
             stats
             histogram
             (view-histogram :kindly))
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and chart")
          (let [[heading chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Histogram**"] heading))
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (string? (:$schema chart))
                "Expected Vega-Lite schema")
            (let [histogram-data (-> chart :vconcat first :layer first :data :values)]
              (is (vector? histogram-data))
              (is (pos? (count histogram-data)))
              (is (every? #(and (contains? % "elapsed-time")
                                (contains? % "end")
                                (contains? % "density"))
                          histogram-data)))))))))
