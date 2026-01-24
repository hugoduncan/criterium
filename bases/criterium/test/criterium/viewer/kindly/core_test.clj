(ns criterium.viewer.kindly.core-test
  ;; Tests for core kindly viewer functions: accumulator, helper functions,
  ;; and basic view implementations (stats, extremes, quantiles, outliers,
  ;; samples, histogram, KDE, bootstrap stats).
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly.core :as core]))

(deftest kindly-add-test
  (testing "kindly-add"
    (testing "appends values to the accumulator"
      (reset! core/accumulated [])
      (core/kindly-add {:a 1})
      (core/kindly-add {:b 2})
      (is (= [{:a 1} {:b 2}] @core/accumulated)))

    (testing "returns nil"
      (reset! core/accumulated [])
      (is (nil? (core/kindly-add {:x 1}))))))

(deftest kindly-heading-test
  (testing "kindly-heading"
    (testing "adds markdown heading with :kind/md metadata"
      (reset! core/accumulated [])
      (core/kindly-heading "Test Section")
      (let [result (first @core/accumulated)]
        (is (= ["**Test Section**"] result))
        (is (= :kind/md (:kindly/kind (meta result))))))

    (testing "returns nil"
      (reset! core/accumulated [])
      (is (nil? (core/kindly-heading "Heading"))))))

(deftest kindly-table-test
  (testing "kindly-table"
    (testing "adds table data with :kind/table metadata"
      (reset! core/accumulated [])
      (let [table-data [{:col1 "a" :col2 1}]]
        (core/kindly-table table-data)
        (let [result (first @core/accumulated)]
          (is (= table-data result))
          (is (= :kind/table (:kindly/kind (meta result)))))))

    (testing "with column-names wraps data in :row-maps structure"
      (reset! core/accumulated [])
      (let [table-data [{:col1 "a" :col2 1}]
            column-names [:col1 :col2]]
        (core/kindly-table table-data {:column-names column-names})
        (let [result (first @core/accumulated)]
          (is (= {:row-maps table-data :column-names column-names} result))
          (is (= :kind/table (:kindly/kind (meta result)))))))

    (testing "returns nil"
      (reset! core/accumulated [])
      (is (nil? (core/kindly-table []))))))

(deftest kindly-vega-lite-test
  (testing "kindly-vega-lite"
    (testing "adds Vega-Lite spec with :kind/vega-lite metadata and schema"
      (reset! core/accumulated [])
      (let [spec {:data {:values []} :mark "point"}]
        (core/kindly-vega-lite spec)
        (let [result (first @core/accumulated)]
          (is (= "https://vega.github.io/schema/vega-lite/v5.json"
                 (:$schema result)))
          (is (= {:values []} (:data result)))
          (is (= "point" (:mark result)))
          (is (= :kind/vega-lite (:kindly/kind (meta result)))))))

    (testing "returns nil"
      (reset! core/accumulated [])
      (is (nil? (core/kindly-vega-lite {}))))))

(deftest kindly-vega-test
  (testing "kindly-vega"
    (testing "adds Vega spec with :kind/vega metadata and schema"
      (reset! core/accumulated [])
      (let [spec {:data {:values []} :marks []}]
        (core/kindly-vega spec)
        (let [result (first @core/accumulated)]
          (is (= "https://vega.github.io/schema/vega/v5.json"
                 (:$schema result)))
          (is (= {:values []} (:data result)))
          (is (= :kind/vega (:kindly/kind (meta result)))))))

    (testing "returns nil"
      (reset! core/accumulated [])
      (is (nil? (core/kindly-vega {}))))))

(deftest flush-test
  (testing "flush"
    (testing "returns accumulated values as kind/fragment"
      (reset! core/accumulated [])
      (core/kindly-add {:a 1})
      (core/kindly-add {:b 2})
      (let [result (core/flush)]
        (is (= [{:a 1} {:b 2}] result))
        (is (= :kind/fragment (:kindly/kind (meta result))))))

    (testing "clears accumulator after flush"
      (reset! core/accumulated [])
      (core/kindly-add {:x 1})
      (core/flush)
      (is (= [] @core/accumulated)))

    (testing "stores fragment in last-fragment atom"
      (reset! core/accumulated [])
      (reset! core/last-fragment nil)
      (core/kindly-add {:a 1})
      (core/kindly-add {:b 2})
      (core/flush)
      (is (= [{:a 1} {:b 2}] @core/last-fragment))
      (is (= :kind/fragment (:kindly/kind (meta @core/last-fragment)))))

    (testing "returns nil when accumulator is empty"
      (reset! core/accumulated [])
      (is (nil? (core/flush))))))

(deftest flush-viewer-test
  (testing "flush-viewer :kindly"
    (testing "dispatches to flush function"
      (reset! core/accumulated [])
      (core/kindly-heading "Section")
      (core/kindly-table [{:a 1}])
      (let [result (view/flush-viewer :kindly)]
        (is (= 2 (count result)))
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= [] @core/accumulated))))))

(deftest stats-view-test
  (testing "view/stats* :kindly"
    (testing "renders stats as heading and table with bench-stats-map"
      (reset! core/accumulated [])
      (view/stats* :kindly {} (:data (test-data/bench-stats-map)))
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Summary stats**"] heading))
          (is (= :kind/table (:kindly/kind (meta table)))))))))

(deftest extremes-view-test
  (testing "view/extremes* :kindly"
    (testing "renders extremes as heading and table"
      (reset! core/accumulated [])
      (view/extremes* :kindly {} (:data (test-data/bench-stats-map)))
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Extremes**"] heading))
          (is (= :kind/table (:kindly/kind (meta table)))))))))

(deftest quantiles-view-test
  (testing "view/quantiles* :kindly"
    (testing "renders quantiles as heading and table"
      (reset! core/accumulated [])
      (view/quantiles* :kindly {} (:data (test-data/quantiles-map)))
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading _table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Quantiles**"] heading)))))))

(deftest outlier-counts-view-test
  (testing "view/outlier-counts* :kindly"
    (testing "renders outliers as heading and table"
      (reset! core/accumulated [])
      (view/outlier-counts* :kindly {} (:data (test-data/outlier-count-map)))
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading _table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Outliers**"] heading)))))))

(deftest samples-view-test
  (testing "view/samples* :kindly"
    (testing "renders samples as heading and Vega-Lite chart"
      (reset! core/accumulated [])
      (view/samples* :kindly {} (:data (test-data/samples-with-2-values-map)))
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and chart")
        (let [[heading chart] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Samples**"] heading))
          (is (= :kind/vega-lite (:kindly/kind (meta chart))))
          (is (string? (:$schema chart))))))))

(deftest histogram-view-test
  (testing "view/histogram* :kindly"
    (testing "renders histogram as heading and Vega-Lite chart"
      (reset! core/accumulated [])
      (let [data-map (:data (test-data/samples-with-outliers-values-map))
            quantiles (analyse/quantiles {:quantiles [0.9 0.99]})
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
        (let [result (core/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result))
              "Expected heading and chart")
          (let [[heading chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Histogram**"] heading))
            (is (= :kind/vega-lite (:kindly/kind (meta chart))))))))))

(deftest metrics-view-test
  (testing "view/metrics* :kindly"
    (testing "renders metrics as heading and table"
      (reset! core/accumulated [])
      (view/metrics* :kindly {} (:data (test-data/samples-with-2-values-map)))
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected heading and table")
        (let [[heading _table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Metrics**"] heading)))))))

(deftest noop-views-test
  (testing "noop views"
    (testing "final-gc-warnings* produces no output"
      (reset! core/accumulated [])
      (view/final-gc-warnings* :kindly {} {})
      (is (empty? @core/accumulated)))

    (testing "os* produces no output"
      (reset! core/accumulated [])
      (view/os* :kindly {} {})
      (is (empty? @core/accumulated)))

    (testing "runtime* produces no output"
      (reset! core/accumulated [])
      (view/runtime* :kindly {} {})
      (is (empty? @core/accumulated)))))
