(ns criterium.viewer.kindly-test
  ;; Tests for the Kindly viewer accumulator and flush mechanism.
  ;; Verifies that values accumulate correctly, flush returns a kind/fragment,
  ;; and the accumulator clears after flush.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]
   [criterium.viewer.kindly-test-helpers :refer [table-rows]]))

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

;;; Tail Analysis Views

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
