(ns criterium.viewer.kindly-distribution-views-test
  ;; Tests for distribution-related Kindly views including KDE visualization,
  ;; mode detection with significance indicators, shape statistics, and
  ;; multimodal warnings.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.test-data :as test-data]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]))

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
