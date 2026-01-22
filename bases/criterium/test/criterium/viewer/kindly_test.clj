(ns criterium.viewer.kindly-test
  ;; Tests for the Kindly viewer accumulator and flush mechanism.
  ;; Verifies that values accumulate correctly, flush returns a kind/fragment,
  ;; and the accumulator clears after flush.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
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
