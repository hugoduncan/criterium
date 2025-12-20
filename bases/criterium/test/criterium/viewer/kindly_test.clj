(ns criterium.viewer.kindly-test
  ;; Tests for the Kindly viewer accumulator and flush mechanism.
  ;; Verifies that values accumulate correctly, flush returns a kind/fragment,
  ;; and the accumulator clears after flush.
  (:require
   [clojure.string :as str]
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

    (testing "stores fragment in last-fragment atom"
      (reset! kindly/accumulated [])
      (reset! kindly/last-fragment nil)
      (kindly/kindly-add {:a 1})
      (kindly/kindly-add {:b 2})
      (kindly/flush)
      (is (= [{:a 1} {:b 2}] @kindly/last-fragment))
      (is (= :kind/fragment (:kindly/kind (meta @kindly/last-fragment)))))

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
          (is (= [{:_metric "Elapsed Time ns",
                   :mean 100.0
                   :min-val 89.0
                   :mean-minus-3sigma 88.0
                   :mean-plus-3sigma 112.0
                   :max-val 114.0}]
                 table)))))

    (testing "renders stats via analyse pipeline"
      (reset! kindly/accumulated [])
      (let [data-map (:data (test-data/samples-with-2-values-map))
            stats (analyse/stats)
            view-stats (view/stats)]
        (->> data-map
             stats
             (view-stats :kindly))
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= [{:_metric "Elapsed Time ns",
                     :mean 1.00,
                     :min-val 1.00,
                     :mean-minus-3sigma 1.00,
                     :mean-plus-3sigma 1.00,
                     :max-val 1.00}]
                   table))))))

    (testing "outputs nothing when metric-ids filter yields no matching metrics"
      (reset! kindly/accumulated [])
      (view/stats* :kindly
                   {:metric-ids [:nonexistent-metric]}
                   (:data (test-data/bench-stats-map)))
      (is (nil? (kindly/flush))))))

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
          (is (= [{:_metric "Elapsed Time"
                   :low-severe 0
                   :low-mild 2
                   :high-mild 3
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
  ;; plot with outlier coloring and notebook-friendly dimensions.
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
          (is (= 700 (-> chart :vconcat first :width)) "Expected notebook-friendly width")
          (is (= 350 (-> chart :vconcat first :height))
              "Expected notebook-friendly height")
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
      (let [data-map (:data (test-data/samples-with-outliers-values-map))
            quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            outliers (analyse/outliers)
            stats (analyse/stats)
            view (view/samples)]
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
  ;; chart with optional normal PDF overlay and notebook-friendly dimensions.
  (testing "view/histogram* :kindly"
    (testing "renders histogram as heading and Vega-Lite chart"
      (reset! kindly/accumulated [])
      (let [data-map (:data (test-data/samples-with-outliers-values-map))
            quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            outliers (analyse/outliers)
            stats (analyse/stats)
            histogram (analyse/histogram)
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
            (is (= 700 (-> chart :vconcat first :width)) "Expected notebook-friendly width")
            (is (= 350 (-> chart :vconcat first :height))
                "Expected notebook-friendly height")
            (let [histogram-data (-> chart :vconcat first :layer first :data :values)]
              (is (vector? histogram-data))
              (is (pos? (count histogram-data)))
              (is (every? #(and (contains? % "elapsed-time")
                                (contains? % "end")
                                (contains? % "density"))
                          histogram-data)))))))))

(deftest sample-percentiles-view-test
  ;; Tests the view/sample-percentiles* multimethod for :kindly viewer.
  ;; Verifies that sample percentiles data is rendered as a heading and
  ;; Vega-Lite percentile distribution chart with notebook-friendly dimensions.
  (testing "view/sample-percentiles* :kindly"
    (testing "renders percentiles as heading and Vega-Lite chart"
      (reset! kindly/accumulated [])
      (view/sample-percentiles* :kindly {} (:data (test-data/samples-with-2-values-map)))
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and chart")
        (let [[heading chart] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Percentiles**"] heading))
          (is (= :kind/vega-lite (:kindly/kind (meta chart))))
          (is (string? (:$schema chart))
              "Expected Vega-Lite schema")
          (is (= 700 (-> chart :vconcat first :width)) "Expected notebook-friendly width")
          (is (= 350 (-> chart :vconcat first :height))
              "Expected notebook-friendly height")
          (let [percentile-data (-> chart :vconcat first :layer first :data :values)]
            (is (vector? percentile-data))
            (is (= 2 (count percentile-data))
                "Expected 2 data points for 2 samples")
            (is (every? #(and (contains? % :elapsed-time)
                              (contains? % :p)
                              (contains? % :x))
                        percentile-data))))))

    (testing "renders percentiles with transformed data via analyse pipeline"
      (reset! kindly/accumulated [])
      (let [data-map (:data (test-data/samples-with-outliers-values-map))
            view-sample-percent (view/sample-percentiles)]
        (view-sample-percent :kindly data-map)
        (let [result (kindly/flush)
              [_heading chart] result
              percentile-data (-> chart :vconcat first :layer first :data :values)]
          (is (= 7 (count percentile-data))
              "Expected 7 data points for 7 samples")
          (is (number? (:p (first percentile-data)))
              "Expected numeric percentile value")
          (is (number? (:x (first percentile-data)))
              "Expected numeric x value"))))))

(deftest metrics-view-test
  ;; Tests the view/metrics* multimethod for :kindly viewer.
  ;; Verifies that metrics data is rendered as a heading and table.
  (testing "view/metrics* :kindly"
    (testing "renders metrics as heading and table"
      (reset! kindly/accumulated [])
      (view/metrics* :kindly {} (:data (test-data/samples-with-2-values-map)))
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Metrics**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 2 (count table))
              "Expected two metric rows (elapsed-time and expr-value)")
          (is (every? #(string? (:metric %)) table)
              "Expected :metric column in all rows")
          (is (every? #(string? (:value %)) table)
              "Expected :value column in all rows"))))))

(deftest event-stats-view-test
  ;; Tests the view/event-stats* multimethod for :kindly viewer.
  ;; Verifies that event stats data is rendered as a heading and table.
  (testing "view/event-stats* :kindly"
    (testing "renders event stats as heading and table via analyse pipeline"
      (reset! kindly/accumulated [])
      (let [data-map (:data (test-data/samples-for-event-stats-map))
            event-stats (analyse/event-stats)
            view (view/event-stats)]
        (->> data-map
             event-stats
             (view :kindly))
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Event stats**"] heading))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (pos? (count table))
                "Expected at least one event stats row")
            (is (every? #(contains? % :metric) table)
                "Expected :metric column in all rows")))))))

(deftest outlier-significance-view-test
  ;; Tests the view/outlier-significance* multimethod for :kindly viewer.
  ;; Verifies that outlier significance data is rendered as heading and table.
  (testing "view/outlier-significance* :kindly"
    (testing "renders outlier significance as heading and table"
      (reset! kindly/accumulated [])
      (view/outlier-significance* :kindly {} (:data (test-data/outlier-significance-map)))
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Outlier Significance**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 1 (count table))
              "Expected one metric row")
          (let [row (first table)]
            (is (= :moderate (:effect row))
                "Expected moderate effect")
            (is (= 0.25 (:significance row))
                "Expected 0.25 significance")))))))

(deftest sample-diffs-view-test
  ;; Tests the view/sample-diffs* multimethod for :kindly viewer.
  ;; Verifies that sample diffs data is rendered as heading and Vega-Lite chart
  ;; with notebook-friendly dimensions.
  (testing "view/sample-diffs* :kindly"
    (testing "renders sample diffs as heading and Vega-Lite chart"
      (reset! kindly/accumulated [])
      (let [data-map (:data (test-data/samples-with-2-values-map))
            ;; Add metric-configs needed by sample-diffs
            data-map (assoc-in data-map [:samples :metric-configs]
                               [{:path [:elapsed-time]
                                 :label "Elapsed Time"
                                 :scale 1}])]
        (view/sample-diffs* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and chart")
          (let [[heading chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Sample diffs**"] heading))
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (string? (:$schema chart))
                "Expected Vega-Lite schema")
            (is (= 700 (-> chart :vconcat first :width)) "Expected notebook-friendly width")
            (is (= 350 (-> chart :vconcat first :height))
                "Expected notebook-friendly height")))))))

(deftest noop-views-test
  ;; Tests that noop multimethods do not add anything to the accumulator.
  (testing "noop views"
    (testing "bootstrap-stats* produces no output"
      (reset! kindly/accumulated [])
      (view/bootstrap-stats* :kindly {} {})
      (is (empty? @kindly/accumulated)))

    (testing "final-gc-warnings* produces no output"
      (reset! kindly/accumulated [])
      (view/final-gc-warnings* :kindly {} {})
      (is (empty? @kindly/accumulated)))

    (testing "os* produces no output"
      (reset! kindly/accumulated [])
      (view/os* :kindly {} {})
      (is (empty? @kindly/accumulated)))

    (testing "runtime* produces no output"
      (reset! kindly/accumulated [])
      (view/runtime* :kindly {} {})
      (is (empty? @kindly/accumulated)))))

(deftest domain-extract-view-test
  ;; Tests the view/domain-extract* multimethod for :kindly viewer.
  ;; Verifies that domain extract data is rendered as a single consolidated
  ;; table with metrics as columns. Values use SI scaling with unit in header.
  ;; Single-key coords use the key name as column header and display raw values.
  (testing "view/domain-extract* :kindly"
    (testing "renders single-impl extract as consolidated table with single-key coords"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} 1e6]
                                                  [{:n 1000} 1e7]
                                                  [{:n 10000} 1e8]]}}}}]
        (view/domain-extract* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Domain Extract**"] heading))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 3 (count table))
                "Expected 3 rows for 3 data points")
            ;; Single-key coords use key name as column header
            (is (every? #(contains? % :n) table)
                "Expected :n column for single-key coords")
            ;; Rows should be sorted numerically
            (is (= [100 1000 10000] (mapv :n table))
                "Expected rows sorted numerically by coord value")
            ;; Column header is metric-name with SI unit
            (let [col-key (first (filter #(clojure.string/starts-with?
                                           (str %) "elapsed-time")
                                         (keys (first table))))]
              (is col-key "Expected elapsed-time column")
              (is (clojure.string/includes? (str col-key) "(")
                  "Expected unit in parentheses"))))))

    (testing "renders multi-key coords with :coordinate column"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100 :m 1} 1e6]
                                                  [{:n 1000 :m 2} 1e7]]}}}}]
        (view/domain-extract* :kindly {} data-map)
        (let [result (kindly/flush)
              [_ table] result]
          (is (every? #(contains? % :coordinate) table)
              "Expected :coordinate column for multi-key coords"))))

    (testing "renders multi-metric extract as consolidated table"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} 1e6]
                                                  [{:n 1000} 1e7]]}
                                          :thread-allocation
                                          {:metric [:stats :thread-allocation :mean]
                                           :data [[{:n 100} 1024]
                                                  [{:n 1000} 2048]]}}}}]
        (view/domain-extract* :kindly {} data-map)
        (let [result (kindly/flush)
              [_ table] result
              col-keys (keys (first table))]
          (is (= 2 (count table))
              "Expected 2 rows")
          (is (some #(clojure.string/starts-with? (str %) "elapsed-time") col-keys)
              "Expected elapsed-time column")
          (is (some #(clojure.string/starts-with? (str %) "thread-allocation") col-keys)
              "Expected thread-allocation column"))))

    (testing "renders multi-impl extract with impl in column headers"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :impl-axis :impl
                                :implementations [:foo :bar]
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100 :impl :foo} 1e6]
                                                  [{:n 100 :impl :bar} 2e6]
                                                  [{:n 1000 :impl :foo} 1e7]
                                                  [{:n 1000 :impl :bar} 2e7]]}}}}]
        (view/domain-extract* :kindly {} data-map)
        (let [result (kindly/flush)
              [_ table] result
              col-keys (set (map str (keys (first table))))]
          (is (= 2 (count table))
              "Expected 2 rows (one per n value)")
          ;; Column headers include impl name with newline
          (is (some #(and (clojure.string/includes? % "foo")
                          (clojure.string/includes? % "elapsed-time"))
                    col-keys)
              "Expected foo elapsed-time column")
          (is (some #(and (clojure.string/includes? % "bar")
                          (clojure.string/includes? % "elapsed-time"))
                    col-keys)
              "Expected bar elapsed-time column"))))

    (testing "handles nil extract gracefully"
      (reset! kindly/accumulated [])
      (view/domain-extract* :kindly {} {:extract nil})
      (is (nil? (kindly/flush))))

    (testing "handles nil values in data"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} nil]
                                                  [{:n 1000} 1e7]]}}}}]
        (view/domain-extract* :kindly {} data-map)
        (let [result (kindly/flush)
              [_ table] result
              col-key (first (filter #(clojure.string/starts-with?
                                       (str %) "elapsed-time")
                                     (keys (first table))))
              ;; Find the row with n=100 (has nil value) - single-key so :n column
              row-with-nil (first (filter #(= 100 (:n %)) table))]
          (is (= 2 (count table)))
          (is (nil? (get row-with-nil col-key))
              "Row with n=100 should have nil value"))))))

(deftest domain-grouped-view-test
  ;; Tests the view/domain-grouped* multimethod for :kindly viewer.
  ;; Verifies that domain grouped data is rendered as heading and table with
  ;; axis-value and run-count columns.
  (testing "view/domain-grouped* :kindly"
    (testing "renders grouped as heading and table"
      (reset! kindly/accumulated [])
      (let [data-map {:grouped {:type :criterium/domain-grouped
                                :axis :impl
                                :data {:foo {:type :criterium/domain
                                             :runs [{:coord {:impl :foo} :data {}}]}
                                       :bar {:type :criterium/domain
                                             :runs [{:coord {:impl :bar} :data {}}
                                                    {:coord {:impl :bar :n 10} :data {}}]}}}}]
        (view/domain-grouped* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (clojure.string/includes? (first heading) "Domain Grouped"))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table))
                "Expected 2 rows for 2 axis values")
            (is (every? #(contains? % :axis-value) table)
                "Expected :axis-value column")
            (is (every? #(contains? % :run-count) table)
                "Expected :run-count column")))))

    (testing "handles nil grouped gracefully"
      (reset! kindly/accumulated [])
      (view/domain-grouped* :kindly {} {:grouped nil})
      (is (nil? (kindly/flush))))))

(deftest domain-comparison-view-test
  ;; Tests the view/domain-comparison* multimethod for :kindly viewer.
  ;; Verifies that domain comparison data is rendered as heading and comparison
  ;; table with axis values as columns.
  (testing "view/domain-comparison* :kindly"
    (testing "renders comparison as heading and table"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison {:type :criterium/domain-comparison
                                   :axis :impl
                                   :metric [:stats :elapsed-time :mean]
                                   :data {:foo [{:coord {:n 100 :impl :foo} :value 1e6}
                                                {:coord {:n 1000 :impl :foo} :value 1e7}]
                                          :bar [{:coord {:n 100 :impl :bar} :value 2e6}
                                                {:coord {:n 1000 :impl :bar} :value 2e7}]}}}]
        (view/domain-comparison* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (clojure.string/includes? (first heading) "Domain Comparison"))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table))
                "Expected 2 rows for 2 n values")
            ;; With single-key coord simplification, column header is "n" not :coordinate
            (is (every? #(or (contains? % "n") (contains? % :coordinate)) table)
                "Expected coordinate column ('n' for single-key coords)")
            (is (every? #(or (contains? % ":foo") (contains? % "foo")) table)
                "Expected axis value columns")))))

    (testing "with :implementations shows factors for non-baseline"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison {:type :criterium/domain-comparison
                                   :axis :impl
                                   :metric [:stats :elapsed-time :mean]
                                   :implementations [:foo :bar]
                                   :data {:foo [{:coord {:n 100 :impl :foo} :value 1e6}
                                                {:coord {:n 1000 :impl :foo} :value 1e7}]
                                          :bar [{:coord {:n 100 :impl :bar} :value 2e6}
                                                {:coord {:n 1000 :impl :bar} :value 2e7}]}}}]
        (view/domain-comparison* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (str/includes? (first heading) "Domain Comparison"))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table)) "Expected 2 rows")
            ;; Check that baseline impl is a column and factor impl has ×
            (let [first-row (first table)]
              (is (contains? first-row "foo") "Expected baseline impl column")
              (is (contains? first-row "bar ×") "Expected factor impl column with ×"))))))

    (testing "handles empty data gracefully"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison {:type :criterium/domain-comparison
                                   :axis :impl
                                   :metric [:stats :elapsed-time :mean]
                                   :data {}}}]
        (view/domain-comparison* :kindly {} data-map)
        (is (nil? (kindly/flush)))))

    (testing "handles nil comparison gracefully"
      (reset! kindly/accumulated [])
      (view/domain-comparison* :kindly {} {:comparison nil})
      (is (nil? (kindly/flush))))))

(deftest domain-regression-view-test
  ;; Tests the view/domain-regression* multimethod for :kindly viewer.
  ;; Verifies that domain regression data is rendered as heading, model table,
  ;; scatter/line chart, and a combined residual plot for models within tolerance.
  (testing "view/domain-regression* :kindly"
    (testing "renders regression with single model within default tolerance"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} 1e6]
                                                  [{:n 200} 2e6]
                                                  [{:n 400} 4e6]
                                                  [{:n 800} 8e6]]}}}
                      :regression {:type :criterium/domain-regression
                                   :axis :n
                                   :regressions {:elapsed-time
                                                 {:metric [:stats :elapsed-time :mean]
                                                  :models [{:id :linear
                                                            :label "O(n)"
                                                            :coefficients {:a 10000.0 :b 0.0}
                                                            :equation-str "y = 10000*n + 0"
                                                            :predict-fn (fn [x] (* 10000.0 x))
                                                            :r-squared 0.9999}
                                                           {:id :quadratic
                                                            :label "O(n²)"
                                                            :coefficients {:a 0.1 :b 100000.0}
                                                            :equation-str "y = 0.1*n² + 100000"
                                                            :predict-fn (fn [x] (+ (* 0.1 x x) 100000.0))
                                                            :r-squared 0.85}]
                                                  :best-fit :linear}}}}]
        ;; With default 1% tolerance, only linear (0.9999) is plotted
        ;; quadratic (0.85) is well below threshold (0.9999 * 0.99 = 0.9899)
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 5 (count result))
              "Expected heading, table, chart, residual heading, residual chart")
          (let [[heading table chart residual-heading residual-chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (clojure.string/includes? (first heading) "Domain Regression"))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table))
                "Expected 2 rows for 2 models")
            (is (every? #(contains? % :model) table)
                "Expected :model column")
            (is (every? #(contains? % :r-squared) table)
                "Expected :r-squared column")

            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (= 2 (count (:layer chart)))
                "Expected 2 layers: scatter and line")
            ;; Combined residual plot
            (is (= :kind/md (:kindly/kind (meta residual-heading))))
            (is (clojure.string/includes? (first residual-heading) "Residual"))
            (is (= :kind/vega-lite (:kindly/kind (meta residual-chart))))
            (is (= 3 (count (:layer residual-chart)))
                "Expected 3 layers: scatter, loess line, zero line")))))

    (testing "renders combined residual plot when multiple models within tolerance"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} 1e6]
                                                  [{:n 200} 2e6]
                                                  [{:n 400} 4e6]
                                                  [{:n 800} 8e6]]}}}
                      :regression {:type :criterium/domain-regression
                                   :axis :n
                                   :regressions {:elapsed-time
                                                 {:metric [:stats :elapsed-time :mean]
                                                  :models [{:id :linear
                                                            :label "O(n)"
                                                            :coefficients {:a 10000.0 :b 0.0}
                                                            :equation-str "y = 10000*n + 0"
                                                            :predict-fn (fn [x] (* 10000.0 x))
                                                            :r-squared 0.9999}
                                                           {:id :n-log-n
                                                            :label "O(n log n)"
                                                            :coefficients {:a 1000.0 :b 0.0}
                                                            :equation-str "y = 1000*n*log(n) + 0"
                                                            :predict-fn (fn [x] (* 1000.0 x (Math/log x)))
                                                            :r-squared 0.9995}]
                                                  :best-fit :linear}}}}]
        ;; Both models within 1% tolerance (0.9999 * 0.99 = 0.9899)
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= 5 (count result))
              "Expected heading, table, chart, residual heading, residual chart")
          (let [[_ table chart _ residual-chart] result]
            ;; Chart should use color legend for multiple models
            (is (contains? (get-in chart [:layer 1 :encoding :color]) :field))
            ;; Residual chart should also use color legend
            (is (contains? (get-in residual-chart [:layer 0 :encoding :color]) :field)
                "Residual chart should have color encoding by model")))))

    (testing "respects custom tolerance parameter"
      (reset! kindly/accumulated [])
      (let [data-map {:extract {:type :criterium/domain-extract
                                :metrics {:elapsed-time
                                          {:metric [:stats :elapsed-time :mean]
                                           :data [[{:n 100} 1e6]
                                                  [{:n 200} 2e6]
                                                  [{:n 400} 4e6]
                                                  [{:n 800} 8e6]]}}}
                      :regression {:type :criterium/domain-regression
                                   :axis :n
                                   :regressions {:elapsed-time
                                                 {:metric [:stats :elapsed-time :mean]
                                                  :models [{:id :linear
                                                            :label "O(n)"
                                                            :coefficients {:a 10000.0 :b 0.0}
                                                            :equation-str "y = 10000*n + 0"
                                                            :predict-fn (fn [x] (* 10000.0 x))
                                                            :r-squared 0.9999}
                                                           {:id :quadratic
                                                            :label "O(n²)"
                                                            :coefficients {:a 0.1 :b 100000.0}
                                                            :equation-str "y = 0.1*n² + 100000"
                                                            :predict-fn (fn [x] (+ (* 0.1 x x) 100000.0))
                                                            :r-squared 0.85}]
                                                  :best-fit :linear}}}}]
        ;; With 20% tolerance, quadratic (0.85) is within threshold
        ;; (0.9999 * 0.80 = 0.7999)
        (view/domain-regression* :kindly {:tolerance 0.20} data-map)
        (let [result (kindly/flush)]
          (is (= 5 (count result))
              "Expected heading, table, chart, residual heading, residual chart")
          ;; Verify residual chart has color encoding for multiple models
          (let [residual-chart (nth result 4)]
            (is (contains? (get-in residual-chart [:layer 0 :encoding :color]) :field))))))

    (testing "renders table only when no extract data"
      (reset! kindly/accumulated [])
      (let [data-map {:regression {:type :criterium/domain-regression
                                   :axis :n
                                   :regressions {:elapsed-time
                                                 {:metric [:stats :elapsed-time :mean]
                                                  :models [{:id :linear
                                                            :label "O(n)"
                                                            :coefficients {:a 10000.0 :b 0.0}
                                                            :equation-str "y = 10000*n + 0"
                                                            :predict-fn (fn [x] (* 10000.0 x))
                                                            :r-squared 0.9999}]
                                                  :best-fit :linear}}}}]
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= 2 (count result))
              "Expected heading and table only when no extract")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= :kind/table (:kindly/kind (meta table))))))))

    (testing "handles empty models gracefully"
      (reset! kindly/accumulated [])
      (let [data-map {:regression {:type :criterium/domain-regression
                                   :axis :n
                                   :regressions {:elapsed-time
                                                 {:metric [:stats :elapsed-time :mean]
                                                  :models []
                                                  :best-fit nil}}}}]
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= 1 (count result))
              "Expected heading only when no models"))))

    (testing "handles nil regression gracefully"
      (reset! kindly/accumulated [])
      (view/domain-regression* :kindly {} {:regression nil})
      (is (nil? (kindly/flush))))))

