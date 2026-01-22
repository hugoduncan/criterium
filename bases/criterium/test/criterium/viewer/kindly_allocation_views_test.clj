(ns criterium.viewer.kindly-allocation-views-test
  ;; Tests for allocation profiling views in the Kindly viewer.
  ;; Covers: allocation-summary, allocation-hotspots, allocation-by-type, and allocation-treemap.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]))

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
