(ns criterium.viewer.kindly-test
  ;; Tests for the Kindly viewer accumulator and flush mechanism.
  ;; Verifies that values accumulate correctly, flush returns a kind/fragment,
  ;; and the accumulator clears after flush.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.domain.types :as domain.types]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]))

(defn table-rows
  "Extract row-maps from a kindly table structure.
  Handles both old structure (plain vector of maps) and new structure
  ({:row-maps [...] :column-names [...]}) used when column ordering is specified."
  [table]
  (if (and (map? table) (contains? table :row-maps))
    (:row-maps table)
    table))

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

    (testing "with column-names wraps data in :row-maps structure"
      (reset! kindly/accumulated [])
      (let [table-data [{:col1 "a" :col2 1}]
            column-names [:col1 :col2]]
        (kindly/kindly-table table-data {:column-names column-names})
        (let [result (first @kindly/accumulated)]
          (is (= {:row-maps table-data :column-names column-names} result))
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
      (view/outlier-significance*
       :kindly
       {}
       (:data (test-data/outlier-significance-map)))
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
            (is (= 700 (-> chart :vconcat first :width))
                "Expected notebook-friendly width")
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
  ;; Tables include :column-names metadata for explicit column ordering.
  (testing "view/domain-extract* :kindly"
    (testing
     "renders single-impl extract as consolidated table with single-key coords"
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
          (let [[heading table-data] result
                rows (table-rows table-data)]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Domain Extract**"] heading))
            (is (= :kind/table (:kindly/kind (meta table-data))))
            ;; Verify column-names are present
            (is (contains? table-data :column-names)
                "Expected :column-names in table structure")
            (is (= "n" (first (:column-names table-data)))
                "Expected coord column first in column-names")
            (is (= 3 (count rows))
                "Expected 3 rows for 3 data points")
            ;; Single-key coords use key name as column header (string key)
            (is (every? #(contains? % "n") rows)
                "Expected \"n\" column for single-key coords")
            ;; Rows should be sorted numerically
            (is (= [100 1000 10000] (mapv #(get % "n") rows))
                "Expected rows sorted numerically by coord value")
            ;; Column header is metric-name with SI unit
            (let [col-key (first (filter #(str/starts-with?
                                           (str %) "elapsed-time")
                                         (keys (first rows))))]
              (is col-key "Expected elapsed-time column")
              (is (str/includes? (str col-key) "(")
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
              [_ table-data] result
              rows (table-rows table-data)]
          (is (every? #(contains? % "coordinate") rows)
              "Expected \"coordinate\" column for multi-key coords"))))

    (testing "renders multi-metric extract as consolidated table"
      (reset! kindly/accumulated [])
      (let [data-map {:extract
                      {:type :criterium/domain-extract
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
              [_ table-data] result
              rows (table-rows table-data)
              col-keys (keys (first rows))]
          (is (= 2 (count rows))
              "Expected 2 rows")
          (is (some
               #(str/starts-with? (str %) "elapsed-time")
               col-keys)
              "Expected elapsed-time column")
          (is (some
               #(str/starts-with? (str %) "thread-allocation")
               col-keys)
              "Expected thread-allocation column"))))

    (testing "renders multi-impl extract with impl in column headers"
      (reset! kindly/accumulated [])
      (let [data-map {:extract
                      {:type :criterium/domain-extract
                       :impl-axis :impl
                       :implementations [:foo :bar]
                       :metrics
                       {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :data [[{:n 100 :impl :foo} 1e6]
                                [{:n 100 :impl :bar} 2e6]
                                [{:n 1000 :impl :foo} 1e7]
                                [{:n 1000 :impl :bar} 2e7]]}}}}]
        (view/domain-extract* :kindly {} data-map)
        (let [result (kindly/flush)
              [_ table-data] result
              rows (table-rows table-data)
              col-keys (set (map str (keys (first rows))))]
          (is (= 2 (count rows))
              "Expected 2 rows (one per n value)")
          ;; Column headers include impl name with newline
          (is (some #(and (str/includes? % "foo")
                          (str/includes? % "elapsed-time"))
                    col-keys)
              "Expected foo elapsed-time column")
          (is (some #(and (str/includes? % "bar")
                          (str/includes? % "elapsed-time"))
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
              [_ table-data] result
              rows (table-rows table-data)
              col-key (first (filter #(str/starts-with?
                                       (str %) "elapsed-time")
                                     (keys (first rows))))
              ;; Find the row with n=100 (has nil value) - single-key uses "n"
              ;; string column
              row-with-nil (first (filter #(= 100 (get % "n")) rows))]
          (is (= 2 (count rows)))
          (is (nil? (get row-with-nil col-key))
              "Row with n=100 should have nil value"))))))

(deftest domain-grouped-view-test
  ;; Tests the view/domain-grouped* multimethod for :kindly viewer.
  ;; Verifies that domain grouped data is rendered as heading and table with
  ;; axis-value and run-count columns.
  (testing "view/domain-grouped* :kindly"
    (testing "renders grouped as heading and table"
      (reset! kindly/accumulated [])
      (let [data-map {:grouped
                      {:type :criterium/domain-grouped
                       :axis :impl
                       :data
                       {:foo {:type :criterium/domain
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
  ;; table with axis values as columns. Tables include :column-names metadata
  ;; for explicit column ordering.
  (testing "view/domain-comparison* :kindly"
    (testing "renders comparison as heading and table"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison
                      {:type :criterium/domain-comparison
                       :axis :impl
                       :metric [:stats :elapsed-time :mean]
                       :data
                       {:foo [{:coord {:n 100 :impl :foo} :value 1e6}
                              {:coord {:n 1000 :impl :foo} :value 1e7}]
                        :bar [{:coord {:n 100 :impl :bar} :value 2e6}
                              {:coord {:n 1000 :impl :bar} :value 2e7}]}}}]
        (view/domain-comparison* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table-data] result
                rows (table-rows table-data)]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (str/includes? (first heading) "Domain Comparison"))
            (is (= :kind/table (:kindly/kind (meta table-data))))
            ;; Verify column-names are present
            (is (contains? table-data :column-names)
                "Expected :column-names in table structure")
            (is (= 2 (count rows))
                "Expected 2 rows for 2 n values")
            ;; With single-key coord simplification, column header is "n"
            ;; not "coordinate"
            (is (every? #(or (contains? % "n") (contains? % "coordinate")) rows)
                "Expected coordinate column ('n' for single-key coords)")
            (is (every? #(or (contains? % ":foo") (contains? % "foo")) rows)
                "Expected axis value columns")))))

    (testing "with :implementations shows factors for non-baseline"
      (reset! kindly/accumulated [])
      (let [data-map {:comparison
                      {:type :criterium/domain-comparison
                       :axis :impl
                       :metric [:stats :elapsed-time :mean]
                       :implementations [:foo :bar]
                       :data
                       {:foo [{:coord {:n 100 :impl :foo} :value 1e6}
                              {:coord {:n 1000 :impl :foo} :value 1e7}]
                        :bar [{:coord {:n 100 :impl :bar} :value 2e6}
                              {:coord {:n 1000 :impl :bar} :value 2e7}]}}}]
        (view/domain-comparison* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          ;; Expect heading, table, and line chart (multi-point with multiple n values)
          (is (= 3 (count result)) "Expected heading, table, and chart")
          (let [[heading table-data chart] result
                rows (table-rows table-data)]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (str/includes? (first heading) "Domain Comparison"))
            (is (= :kind/table (:kindly/kind (meta table-data))))
            (is (= 2 (count rows)) "Expected 2 rows")
            ;; Check that baseline impl is a column and factor impl has ×
            (let [first-row (first rows)]
              (is (contains? first-row "foo")
                  "Expected baseline impl column")
              (is (contains? first-row "bar ×")
                  "Expected factor impl column with ×"))
            ;; Verify chart is present
            (is (= :kind/vega-lite (:kindly/kind (meta chart)))
                "Expected Vega-Lite chart")))))

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
      (let [data-map
            {:extract
             {:type :criterium/domain-extract
              :metrics {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :data [[{:n 100} 1e6]
                                [{:n 200} 2e6]
                                [{:n 400} 4e6]
                                [{:n 800} 8e6]]}}}
             :regression
             {:type :criterium/domain-regression
              :axis :n
              :regressions
              {:elapsed-time
               {:metric [:stats :elapsed-time :mean]
                :models [{:id :linear
                          :label "O(n)"
                          :coefficients {:a 10000.0 :b 0.0}
                          :equation-str "y = 10000*n + 0"
                          :predict-fn (fn [^double x]
                                        (* 10000.0 x))
                          :r-squared 0.9999}
                         {:id :quadratic
                          :label "O(n²)"
                          :coefficients {:a 0.1 :b 100000.0}
                          :equation-str "y = 0.1*n² + 100000"
                          :predict-fn (fn [^double x]
                                        (+ (* 0.1 x x) 100000.0))
                          :r-squared 0.85}]
                :best-fit :linear}}}}]
        ;; With default 1% tolerance, only linear (0.9999) is plotted
        ;; quadratic (0.85) is well below threshold (0.9999 * 0.99 = 0.9899)
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is
           (= 5 (count result))
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

    (testing
     "renders combined residual plot when multiple models within tolerance"
      (reset! kindly/accumulated [])
      (let [data-map
            {:extract
             {:type :criterium/domain-extract
              :metrics {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :data [[{:n 100} 1e6]
                                [{:n 200} 2e6]
                                [{:n 400} 4e6]
                                [{:n 800} 8e6]]}}}
             :regression
             {:type :criterium/domain-regression
              :axis :n
              :regressions
              {:elapsed-time
               {:metric [:stats :elapsed-time :mean]
                :models [{:id :linear
                          :label "O(n)"
                          :coefficients {:a 10000.0 :b 0.0}
                          :equation-str "y = 10000*n + 0"
                          :predict-fn (fn [^double x]
                                        (* 10000.0 x))
                          :r-squared 0.9999}
                         {:id :n-log-n
                          :label "O(n log n)"
                          :coefficients {:a 1000.0 :b 0.0}
                          :equation-str "y = 1000*n*log(n) + 0"
                          :predict-fn (fn [^double x]
                                        (* 1000.0 x (Math/log x)))
                          :r-squared 0.9995}]
                :best-fit :linear}}}}]
        ;; Both models within 1% tolerance (0.9999 * 0.99 = 0.9899)
        (view/domain-regression* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is
           (= 5 (count result))
           "Expected heading, table, chart, residual heading, residual chart")
          (let [[_ _table chart _ residual-chart] result]
            ;; Chart should use color legend for multiple models
            (is (contains? (get-in chart [:layer 1 :encoding :color]) :field))
            ;; Residual chart should also use color legend
            (is (contains?
                 (get-in residual-chart [:layer 0 :encoding :color])
                 :field)
                "Residual chart should have color encoding by model")))))

    (testing "respects custom tolerance parameter"
      (reset! kindly/accumulated [])
      (let [data-map
            {:extract
             {:type :criterium/domain-extract
              :metrics {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :data [[{:n 100} 1e6]
                                [{:n 200} 2e6]
                                [{:n 400} 4e6]
                                [{:n 800} 8e6]]}}}
             :regression
             {:type :criterium/domain-regression
              :axis :n
              :regressions
              {:elapsed-time
               {:metric [:stats :elapsed-time :mean]
                :models [{:id :linear
                          :label "O(n)"
                          :coefficients {:a 10000.0 :b 0.0}
                          :equation-str "y = 10000*n + 0"
                          :predict-fn (fn [^double x]
                                        (* 10000.0 x))
                          :r-squared 0.9999}
                         {:id :quadratic
                          :label "O(n²)"
                          :coefficients {:a 0.1 :b 100000.0}
                          :equation-str "y = 0.1*n² + 100000"
                          :predict-fn (fn [^double x]
                                        (+ (* 0.1 x x) 100000.0))
                          :r-squared 0.85}]
                :best-fit :linear}}}}]
        ;; With 20% tolerance, quadratic (0.85) is within threshold
        ;; (0.9999 * 0.80 = 0.7999)
        (view/domain-regression* :kindly {:tolerance 0.20} data-map)
        (let [result (kindly/flush)]
          (is
           (= 5 (count result))
           "Expected heading, table, chart, residual heading, residual chart")
          ;; Verify residual chart has color encoding for multiple models
          (let [residual-chart (nth result 4)]
            (is (contains?
                 (get-in residual-chart [:layer 0 :encoding :color])
                 :field))))))

    (testing "renders table only when no extract data"
      (reset! kindly/accumulated [])
      (let [data-map {:regression
                      {:type :criterium/domain-regression
                       :axis :n
                       :regressions
                       {:elapsed-time
                        {:metric [:stats :elapsed-time :mean]
                         :models [{:id :linear
                                   :label "O(n)"
                                   :coefficients {:a 10000.0 :b 0.0}
                                   :equation-str "y = 10000*n + 0"
                                   :predict-fn (fn [^double x]
                                                 (* 10000.0 x))
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
      (let [data-map {:regression
                      {:type :criterium/domain-regression
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

(deftest allocation-summary-view-test
  ;; Tests the view/allocation-summary* multimethod for :kindly viewer.
  ;; Verifies that allocation summary data is rendered as a heading and table
  ;; with metrics for total allocated/freed, retained, counts, and ratio (based on bytes).
  (testing "view/allocation-summary* :kindly"
    (testing "renders summary as heading and table"
      (reset! kindly/accumulated [])
      (let [data-map {:allocation-summary
                      {:type :criterium/allocation-summary
                       :total-allocated 1048576
                       :total-freed 524288
                       :num-allocations 100
                       :num-freed 50
                       :freed-ratio 0.5}}]
        (view/allocation-summary* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Allocation Summary**"] heading))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 6 (count table))
                "Expected 6 metrics")
            (is (every? #(contains? % :metric) table)
                "Expected :metric column")
            (is (every? #(contains? % :value) table)
                "Expected :value column")
            (let [metrics-by-name (into {} (map (juxt :metric :value) table))]
              (is (= "1048576 bytes" (get metrics-by-name "Total allocated"))
                  "Expected byte value with unit")
              (is (= "50.0%" (get metrics-by-name "Freed ratio"))
                  "Expected percentage string based on bytes"))))))

    (testing "handles zero allocations"
      (reset! kindly/accumulated [])
      (let [data-map {:allocation-summary
                      {:type :criterium/allocation-summary
                       :total-allocated 0
                       :total-freed 0
                       :num-allocations 0
                       :num-freed 0
                       :freed-ratio 0.0}}]
        (view/allocation-summary* :kindly {} data-map)
        (let [result (kindly/flush)
              [_ table] result
              metrics-by-name (into {} (map (juxt :metric :value) table))]
          (is (= "0.0%" (get metrics-by-name "Freed ratio"))
              "Expected 0.0% for zero allocations"))))

    (testing "handles nil summary gracefully"
      (reset! kindly/accumulated [])
      (view/allocation-summary* :kindly {} {:allocation-summary nil})
      (is (nil? (kindly/flush))))))

(deftest allocation-hotspots-view-test
  ;; Tests the view/allocation-hotspots* multimethod for :kindly viewer.
  ;; Verifies that hotspots data is rendered as a heading and table with
  ;; call-site, object-type, count, bytes, freed-count, and freed-bytes columns.
  (testing "view/allocation-hotspots* :kindly"
    (testing "renders hotspots as heading and table"
      (reset! kindly/accumulated [])
      (let [data-map {:allocation-hotspots
                      {:type :criterium/allocation-hotspots
                       :hotspots [{:call-site {:call-class "my.ns$fn"
                                               :call-method "invoke"
                                               :call-file "my_ns.clj"
                                               :call-line 42}
                                   :object-type "Ljava/lang/String;"
                                   :count 100
                                   :bytes 4096
                                   :freed-count 50
                                   :freed-bytes 2048}
                                  {:call-site {:call-class "other.ns$bar"
                                               :call-method "invoke"
                                               :call-file "other_ns.clj"
                                               :call-line 10}
                                   :object-type "[B"
                                   :count 50
                                   :bytes 2048
                                   :freed-count 25
                                   :freed-bytes 1024}]}}]
        (view/allocation-hotspots* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Allocation Hotspots**"] heading))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 2 (count table))
                "Expected 2 hotspot rows")
            (let [first-row (first table)]
              (is (str/includes? (:call-site first-row) "my.ns$fn")
                  "Expected call-site to contain class name")
              (is (str/includes? (:call-site first-row) "my_ns.clj:42")
                  "Expected call-site to contain file:line")
              (is (= "Ljava/lang/String;" (:object-type first-row))
                  "Expected object-type to be present")
              (is (= 100 (:count first-row)))
              (is (= 4096 (:bytes first-row))
                  "Expected integer byte value")
              (is (= 50 (:freed-count first-row))))))))

    (testing "handles empty hotspots gracefully"
      (reset! kindly/accumulated [])
      (let [data-map {:allocation-hotspots
                      {:type :criterium/allocation-hotspots
                       :hotspots []}}]
        (view/allocation-hotspots* :kindly {} data-map)
        (is (nil? (kindly/flush)))))

    (testing "handles nil hotspots gracefully"
      (reset! kindly/accumulated [])
      (view/allocation-hotspots* :kindly {} {:allocation-hotspots nil})
      (is (nil? (kindly/flush))))))

(deftest allocation-by-type-view-test
  ;; Tests the view/allocation-by-type* multimethod for :kindly viewer.
  ;; Verifies that by-type data is rendered as a heading and table with
  ;; type, count, bytes, freed-count, and freed-bytes columns, sorted by bytes.
  (testing "view/allocation-by-type* :kindly"
    (testing "renders by-type as heading and table sorted by bytes"
      (reset! kindly/accumulated [])
      (let [data-map {:allocation-by-type
                      {:type :criterium/allocation-by-type
                       :by-type {"[B" {:count 10 :bytes 1024
                                       :freed-count 5 :freed-bytes 512}
                                 "java.lang.String" {:count 50 :bytes 4096
                                                     :freed-count 25 :freed-bytes 2048}
                                 "[J" {:count 5 :bytes 512
                                       :freed-count 2 :freed-bytes 256}}}}]
        (view/allocation-by-type* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Allocations by Type**"] heading))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 3 (count table))
                "Expected 3 type rows")
            ;; Verify sorted by bytes descending
            (is (= "java.lang.String" (:type (first table)))
                "Expected String first (highest bytes)")
            (is (= "[B" (:type (second table)))
                "Expected byte array second")
            (is (= "[J" (:type (nth table 2)))
                "Expected long array last (lowest bytes)")
            (let [first-row (first table)]
              (is (= 50 (:count first-row)))
              (is (integer? (:bytes first-row))
                  "Expected integer byte value"))))))

    (testing "handles empty by-type gracefully"
      (reset! kindly/accumulated [])
      (let [data-map {:allocation-by-type
                      {:type :criterium/allocation-by-type
                       :by-type {}}}]
        (view/allocation-by-type* :kindly {} data-map)
        (is (nil? (kindly/flush)))))

    (testing "handles nil by-type gracefully"
      (reset! kindly/accumulated [])
      (view/allocation-by-type* :kindly {} {:allocation-by-type nil})
      (is (nil? (kindly/flush))))))

(deftest allocation-treemap-view-test
  ;; Tests the view/allocation-treemap* multimethod for :kindly viewer.
  ;; Verifies that treemap data is rendered as a heading and Vega chart.
  (testing "view/allocation-treemap* :kindly"
    (testing "renders treemap as heading and vega chart"
      (reset! kindly/accumulated [])
      (let [data-map {:allocation-treemap
                      {:type :criterium/allocation-treemap
                       :size-by :bytes
                       :root {:name "root"
                              :value 1000
                              :children [{:name "java.lang.String"
                                          :value 600
                                          :children [{:name "L42" :value 600}]}
                                         {:name "[B"
                                          :value 400
                                          :children [{:name "L10" :value 400}]}]}}}]
        (view/allocation-treemap* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and chart")
          (let [[heading vega-spec] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Allocation Treemap**"] heading))
            (is (= :kind/vega (:kindly/kind (meta vega-spec))))
            (is (str/includes? (:$schema vega-spec) "vega/v5.json"))
            (is (contains? vega-spec :data))
            (is (contains? vega-spec :marks))))))

    (testing "uses custom treemap-id"
      (reset! kindly/accumulated [])
      (let [data-map {:my-treemap
                      {:type :criterium/allocation-treemap
                       :root {:name "root"
                              :value 100
                              :children [{:name "type1" :value 100}]}}}]
        (view/allocation-treemap* :kindly {:treemap-id :my-treemap} data-map)
        (let [result (kindly/flush)
              [heading _vega-spec] result]
          (is (= ["**Allocation Treemap**"] heading)))))

    (testing "handles nil treemap data gracefully"
      (reset! kindly/accumulated [])
      (view/allocation-treemap* :kindly {} {:allocation-treemap nil})
      (is (nil? (kindly/flush))))

    (testing "handles missing root gracefully"
      (reset! kindly/accumulated [])
      (view/allocation-treemap* :kindly {}
                                {:allocation-treemap
                                 {:type :criterium/allocation-treemap}})
      (is (nil? (kindly/flush))))))

(deftest kde-view-test
  ;; Tests the view/kde* multimethod for :kindly viewer.
  ;; Verifies that KDE data is rendered as a heading and Vega-Lite chart
  ;; with density curve, confidence bands, and optional mode markers.
  (testing "view/kde* :kindly"
    (testing "renders KDE as heading and Vega-Lite chart"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0]
                              :density [0.1 0.3 0.1]
                              :lower-band [0.08 0.25 0.08]
                              :upper-band [0.12 0.35 0.12]
                              :modes [{:location 2.0
                                       :density 0.3
                                       :ci-lower 1.8
                                       :ci-upper 2.2}]
                              :n 100}}}]
        (view/kde* :kindly {} {:kde kde-data})
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and chart")
          (let [[heading chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Kernel Density Estimation**"] heading))
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))
            (is (string? (:$schema chart)) "Expected Vega-Lite schema")
            (is (contains? chart :vconcat))
            (let [first-chart (first (:vconcat chart))
                  layers (:layer first-chart)
                  ;; KDE layers are nested in a group for independent Y-scale
                  kde-group (first layers)
                  kde-layers (:layer kde-group)]
              (is (contains? first-chart :layer))
              (is (>= (count kde-layers) 2)
                  "Expected at least confidence band and density layers in nested group"))))))

    (testing "uses custom kde-id"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.3
                              :grid [1.0]
                              :density [0.5]
                              :lower-band [0.4]
                              :upper-band [0.6]
                              :modes []
                              :n 25}}}]
        (view/kde* :kindly {:kde-id :my-kde} {:my-kde kde-data})
        (let [result (kindly/flush)
              [heading _chart] result]
          (is (= ["**Kernel Density Estimation**"] heading)))))

    (testing "handles missing kde data gracefully"
      (reset! kindly/accumulated [])
      (view/kde* :kindly {} {})
      (is (nil? (kindly/flush))))))

(deftest kde-modes-rendering-test
  ;; Tests mode markers, significance indicators, and CI lines in KDE visualization.
  ;; Verifies that when modes data is provided via separate :modes key,
  ;; the chart includes point markers and rule lines for confidence intervals.
  (testing "view/kde* :kindly modes rendering"
    (testing "includes mode markers and CI lines when modes data present"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0]
                              :density [0.1 0.3 0.1]
                              :lower-band [0.08 0.25 0.08]
                              :upper-band [0.12 0.35 0.12]
                              :n 100}}}
            modes-data {:type :criterium/modes
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 2.0
                                          :density 0.3
                                          :ci-lower 1.8
                                          :ci-upper 2.2
                                          :significant? true}]
                                 :n-modes 1}}}]
        (view/kde* :kindly {} {:kde kde-data :modes modes-data})
        (let [result (kindly/flush)
              [_heading chart] result
              first-chart (first (:vconcat chart))
              layers (:layer first-chart)
              kde-group (first layers)
              kde-layers (:layer kde-group)
              point-layer (first (filter #(= "point" (get-in % [:mark :type]))
                                         kde-layers))
              rule-layer (first (filter #(= "rule" (get-in % [:mark :type]))
                                        kde-layers))]
          (is (some? point-layer) "Expected point layer for mode markers")
          (is (some? rule-layer) "Expected rule layer for CI lines")
          ;; Verify point layer has shape encoding for significance
          (is (= "significant" (get-in point-layer [:encoding :shape :field]))
              "Expected shape encoding by significance field")
          (is (= ["yes" "no"]
                 (get-in point-layer [:encoding :shape :scale :domain]))
              "Expected significance domain")
          (is (= ["circle" "triangle-up"]
                 (get-in point-layer [:encoding :shape :scale :range]))
              "Expected significance shape range")
          ;; Verify rule layer encodes CI bounds
          (is (= "ci-lower" (get-in rule-layer [:encoding :x :field]))
              "Expected x encoding from ci-lower")
          (is (= "ci-upper" (get-in rule-layer [:encoding :x2 :field]))
              "Expected x2 encoding from ci-upper")
          ;; Verify stroke dash encoding for significance
          (is (= "significant" (get-in rule-layer [:encoding :strokeDash :field]))
              "Expected strokeDash encoding by significance"))))

    (testing "distinguishes significant vs non-significant modes via shape"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0 4.0]
                              :density [0.1 0.3 0.2 0.15]
                              :lower-band [0.08 0.25 0.15 0.10]
                              :upper-band [0.12 0.35 0.25 0.20]
                              :n 100}}}
            modes-data {:type :criterium/modes
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 2.0
                                          :density 0.3
                                          :ci-lower 1.8
                                          :ci-upper 2.2
                                          :significant? true}
                                         {:location 3.5
                                          :density 0.18
                                          :ci-lower 3.2
                                          :ci-upper 3.8
                                          :significant? false}]
                                 :n-modes 2}}}]
        (view/kde* :kindly {} {:kde kde-data :modes modes-data})
        (let [result (kindly/flush)
              [_heading chart] result
              first-chart (first (:vconcat chart))
              kde-group (first (:layer first-chart))
              kde-layers (:layer kde-group)
              point-layer (first (filter #(= "point" (get-in % [:mark :type]))
                                         kde-layers))
              point-data (get-in point-layer [:data :values])]
          (is (= 2 (count point-data)) "Expected 2 mode markers")
          (is (= #{"yes" "no"} (set (map #(get % "significant") point-data)))
              "Expected both significant and non-significant modes"))))

    (testing "omits mode layers when modes data not present"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0]
                              :density [0.1 0.3 0.1]
                              :lower-band [0.08 0.25 0.08]
                              :upper-band [0.12 0.35 0.12]
                              :n 100}}}]
        (view/kde* :kindly {} {:kde kde-data})
        (let [result (kindly/flush)
              [_heading chart] result
              first-chart (first (:vconcat chart))
              kde-group (first (:layer first-chart))
              kde-layers (:layer kde-group)
              point-layers (filter #(= "point" (get-in % [:mark :type]))
                                   kde-layers)
              rule-layers (filter #(= "rule" (get-in % [:mark :type]))
                                  kde-layers)]
          (is (empty? point-layers)
              "Expected no point layers without modes data")
          (is (empty? rule-layers)
              "Expected no rule layers without modes data"))))

    (testing "omits mode layers when modes list is empty"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            kde-data {:type :criterium/kde
                      :metrics-defs metrics-defs
                      :transform {:sample-> identity :->sample identity}
                      :kdes {[:elapsed-time]
                             {:type :criterium/kde
                              :bandwidth 0.5
                              :grid [1.0 2.0 3.0]
                              :density [0.1 0.3 0.1]
                              :lower-band [0.08 0.25 0.08]
                              :upper-band [0.12 0.35 0.12]
                              :n 100}}}
            modes-data {:type :criterium/modes
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes []
                                 :n-modes 0}}}]
        (view/kde* :kindly {} {:kde kde-data :modes modes-data})
        (let [result (kindly/flush)
              [_heading chart] result
              first-chart (first (:vconcat chart))
              kde-group (first (:layer first-chart))
              kde-layers (:layer kde-group)
              point-layers (filter #(= "point" (get-in % [:mark :type]))
                                   kde-layers)]
          (is (empty? point-layers)
              "Expected no point layers when modes list empty"))))))

;;; Shape Statistics Views

(deftest shape-stats-view-test
  ;; Tests the view/shape-stats* multimethod for :kindly viewer.
  ;; Covers: skewness, kurtosis, and CV with their classifications.
  (testing "view/shape-stats* :kindly"
    (testing "renders shape statistics as heading and table"
      (reset! kindly/accumulated [])
      (let [data-map (test-data/bootstrap-stats-with-shape-map)]
        (view/shape-stats* :kindly {} data-map)
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and table")
          (let [[heading table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Shape Statistics**"] heading))
            (is (= :kind/table (:kindly/kind (meta table))))
            (is (= 1 (count table)) "Expected 1 row for 1 metric")
            (let [row (first table)]
              (is (= "Elapsed Time" (:metric row)))
              (is (str/includes? (:skewness row) "0.35"))
              (is (= "slightly-right-skewed" (:skewness-interpretation row)))
              (is (str/includes? (:kurtosis row) "2.8"))
              (is (= "normal-tails" (:kurtosis-interpretation row)))
              (is (str/includes? (:cv row) "0.04"))
              (is (= "low-variability" (:cv-interpretation row))))))))
    (testing "handles missing bootstrap data gracefully"
      (reset! kindly/accumulated [])
      (view/shape-stats* :kindly {} {})
      (is (nil? (kindly/flush))))
    (testing "uses custom bootstrap-stats-id"
      (reset! kindly/accumulated [])
      (let [data-map (test-data/bootstrap-stats-with-shape-map)
            custom-map {:my-bootstrap (:bootstrap-stats data-map)}]
        (view/shape-stats* :kindly {:bootstrap-stats-id :my-bootstrap} custom-map)
        (let [result (kindly/flush)
              [heading _table] result]
          (is (= ["**Shape Statistics**"] heading)))))))

;;; Modal Analysis Views

(deftest multimodal-warning-view-test
  ;; Tests the view/multimodal-warning* multimethod for :kindly viewer.
  ;; Verifies that multimodal warning is rendered as heading and tables
  ;; when n-modes > 1, with correct Kindly metadata.
  (testing "view/multimodal-warning* :kindly"
    (testing "outputs warning with mode count and locations when n-modes > 1"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 1e-9
                                          :density 0.3}
                                         {:location 2e-9
                                          :density 0.25}]
                                 :n-modes 2}}}]
        (view/multimodal-warning* :kindly {} {:modes modes-data})
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 4 (count result))
              "Expected heading, status table, locations heading, locations table")
          (let [[heading status-table loc-heading loc-table] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (str/includes? (first heading) "WARNING"))
            (is (str/includes? (first heading) "Multimodal"))
            (is (= :kind/table (:kindly/kind (meta status-table))))
            (is (= 1 (count status-table))
                "Expected 1 row in status table")
            (is (= "Mode count" (:metric (first status-table))))
            (is (= 2 (:value (first status-table))))
            (is (= :kind/md (:kindly/kind (meta loc-heading))))
            (is (= :kind/table (:kindly/kind (meta loc-table))))
            (is (= 2 (count loc-table))
                "Expected 2 mode location rows")
            (is (every? #(contains? % :location) loc-table))
            (is (every? #(contains? % :density) loc-table))))))

    (testing "outputs nothing when n-modes <= 1"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 1e-9 :density 0.3}]
                                 :n-modes 1}}}]
        (view/multimodal-warning* :kindly {} {:modes modes-data})
        (is (nil? (kindly/flush))
            "Expected no output when only 1 mode")))

    (testing "outputs nothing when modes-map is nil"
      (reset! kindly/accumulated [])
      (view/multimodal-warning* :kindly {} {:modes nil})
      (is (nil? (kindly/flush))))

    (testing "uses custom modes-id"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform {:sample-> identity :->sample identity}
                        :modes {[:elapsed-time]
                                {:modes [{:location 1e-9 :density 0.3}
                                         {:location 2e-9 :density 0.25}]
                                 :n-modes 2}}}]
        (view/multimodal-warning*
         :kindly
         {:modes-id :my-modes}
         {:my-modes modes-data})
        (let [result (kindly/flush)]
          (is (= 4 (count result))
              "Expected output with custom modes-id"))))))

;;; Domain Apply View Tests

(defn- make-bench-data
  "Create minimal benchmark data with stats for testing domain-apply.
  Unlike print viewer, kindly stats view also requires :max-val in stats."
  [mean-ns]
  (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
        mean-ns (double mean-ns)]
    {:samples {:type :criterium/collected-metrics-samples
               :metrics-defs metrics-defs
               :metric->values {[:elapsed-time] [mean-ns]}
               :transform collect-plan/identity-transforms
               :batch-size 1
               :num-samples 1
               :eval-count 1
               :elapsed-time mean-ns}
     :stats {:type :criterium/stats
             :metrics-defs metrics-defs
             :transform collect-plan/identity-transforms
             :batch-size 1
             :source-id :samples
             :outliers-id nil
             :stats {:elapsed-time {:mean mean-ns
                                    :variance 1.0
                                    :mean-plus-3sigma (+ mean-ns 3.0)
                                    :mean-minus-3sigma (- mean-ns 3.0)
                                    :min-val mean-ns
                                    :max-val (+ mean-ns 10.0)}}}}))

(deftest domain-apply-kindly-test
  ;; Tests the view/domain-apply* multimethod for :kindly viewer.
  ;; Verifies that each run is rendered with a heading showing the coord
  ;; and the view-spec is applied to produce output in the accumulator.
  (testing "view/domain-apply* :kindly"
    (testing "renders heading and applies view-spec to each run"
      (reset! kindly/accumulated [])
      (let [domain (-> (domain.types/domain)
                       (domain.types/add-run {:n 100} (make-bench-data 100))
                       (domain.types/add-run {:n 200} (make-bench-data 200)))]
        (view/domain-apply*
         :kindly
         {:view-spec [:stats {}]}
         {:domain domain})
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          ;; Each run produces: heading + stats heading + stats table = 3 items
          ;; 2 runs = 6 items total
          (is (= 6 (count result))
              "Expected 6 items: 2 runs × (coord heading + stats heading + stats table)")
          ;; First item should be the coord heading for first run
          (let [first-heading (first result)]
            (is (= :kind/md (:kindly/kind (meta first-heading))))
            (is (str/includes? (first first-heading) "Run:")
                "First item should be run heading")
            (is (str/includes? (first first-heading) "{:n 100}")
                "Run heading should contain coord"))
          ;; Fourth item should be the coord heading for second run
          (let [second-heading (nth result 3)]
            (is (= :kind/md (:kindly/kind (meta second-heading))))
            (is (str/includes? (first second-heading) "{:n 200}")
                "Second run heading should contain coord")))))

    (testing "handles keyword coord"
      (reset! kindly/accumulated [])
      (let [domain (domain.types/domain
                    {:coord :baseline :data (make-bench-data 50)})]
        (view/domain-apply*
         :kindly
         {:view-spec [:stats {}]}
         {:domain domain})
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (let [first-heading (first result)]
            (is (str/includes? (first first-heading) ":baseline")
                "Coord heading should contain keyword coord")))))

    (testing "uses custom domain-id"
      (reset! kindly/accumulated [])
      (let [domain (domain.types/domain
                    {:coord {:impl :foo} :data (make-bench-data 150)})]
        (view/domain-apply*
         :kindly
         {:domain-id :my-domain
          :view-spec [:stats {}]}
         {:my-domain domain})
        (let [result (kindly/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (let [first-heading (first result)]
            (is (str/includes? (first first-heading) "{:impl :foo}")
                "Should use custom domain-id")))))

    (testing "handles empty domain gracefully"
      (reset! kindly/accumulated [])
      (let [domain (domain.types/domain)]
        (view/domain-apply*
         :kindly
         {:view-spec [:stats {}]}
         {:domain domain})
        (is (nil? (kindly/flush))
            "Should produce no output for empty domain")))

    (testing "handles missing domain gracefully"
      (reset! kindly/accumulated [])
      (view/domain-apply*
       :kindly
       {:view-spec [:stats {}]}
       {})
      (is (nil? (kindly/flush))
          "Should produce no output when domain is missing"))

    (testing "warns when view-spec is missing"
      (reset! kindly/accumulated [])
      (let [domain (domain.types/domain
                    {:coord :test :data (make-bench-data 100)})
            stderr-output (java.io.StringWriter.)]
        (binding [*err* stderr-output]
          (view/domain-apply*
           :kindly
           {}
           {:domain domain}))
        (is (nil? (kindly/flush))
            "Should produce no kindly output")
        (is (str/includes? (str stderr-output)
                           "WARNING: domain-apply requires :view-spec option")
            "Should warn on stderr when view-spec is missing")))))
