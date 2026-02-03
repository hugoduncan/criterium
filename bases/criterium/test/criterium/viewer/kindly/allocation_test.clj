(ns criterium.viewer.kindly.allocation-test
  ;; Tests for kindly allocation views: allocation-summary, allocation-hotspots,
  ;; allocation-by-type, and allocation-treemap.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.view :as view]
   [criterium.viewer.kindly.allocation]
   [criterium.viewer.kindly.core :as core]))

;;; Allocation Summary Tests

(deftest allocation-summary-kindly-test
  ;; Tests the kindly viewer output for allocation-summary results.
  ;; Verifies display of totals, counts, and freed ratio as Kindly table.
  (testing "allocation-summary*"
    (testing "renders summary as heading and table"
      (reset! core/accumulated [])
      (view/allocation-summary*
       :kindly
       {}
       {:allocation-summary
        {:type :criterium/allocation-summary
         :total-allocated 1024
         :total-freed 512
         :num-allocations 100
         :num-freed 50
         :freed-ratio 0.5}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)) "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Allocation Summary**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 6 (count table)) "Expected 6 rows")
          (is (= {:metric "Total allocated" :value "1024 bytes"} (first table)))
          (is (= {:metric "Freed ratio" :value "50.0%"} (last table))))))

    (testing "handles zero allocations"
      (reset! core/accumulated [])
      (view/allocation-summary*
       :kindly
       {}
       {:allocation-summary
        {:type :criterium/allocation-summary
         :total-allocated 0
         :total-freed 0
         :num-allocations 0
         :num-freed 0
         :freed-ratio 0.0}})
      (let [result (core/flush)]
        (is (= 2 (count result)))
        (let [[_ table] result]
          (is (= {:metric "Total allocated" :value "0 bytes"} (first table)))
          (is (= {:metric "Freed ratio" :value "0.0%"} (last table))))))

    (testing "uses custom summary-id"
      (reset! core/accumulated [])
      (view/allocation-summary*
       :kindly
       {:summary-id :my-summary}
       {:my-summary
        {:type :criterium/allocation-summary
         :total-allocated 256
         :total-freed 128
         :num-allocations 10
         :num-freed 5
         :freed-ratio 0.5}})
      (let [result (core/flush)]
        (is (= 2 (count result)))
        (let [[_ table] result]
          (is
           (= {:metric "Total allocated" :value "256 bytes"} (first table))))))

    (testing "returns nil for missing data"
      (reset! core/accumulated [])
      (view/allocation-summary* :kindly {} {})
      (is (empty? @core/accumulated)))))

;;; Allocation Hotspots Tests

(deftest allocation-hotspots-kindly-test
  ;; Tests the kindly viewer output for allocation-hotspots results.
  ;; Verifies table format with call site, object type, counts, and byte amounts.
  (testing "allocation-hotspots*"
    (testing "renders hotspots as heading and table"
      (reset! core/accumulated [])
      (view/allocation-hotspots*
       :kindly
       {}
       {:allocation-hotspots
        {:type :criterium/allocation-hotspots
         :hotspots [{:call-site {:call-class "my.ns$fn"
                                 :call-method "invoke"
                                 :call-file "my_ns.clj"
                                 :call-line 42}
                     :object-type "Ljava/lang/String;"
                     :count 100
                     :bytes 1024
                     :freed-count 50
                     :freed-bytes 512}
                    {:call-site {:call-class "other.ns$g"
                                 :call-method "apply"
                                 :call-file "other.clj"
                                 :call-line 10}
                     :object-type "Ljava/lang/Object;"
                     :count 50
                     :bytes 256
                     :freed-count 25
                     :freed-bytes 128}]}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)) "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Allocation Hotspots**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 2 (count table)))
          (is (= 100 (:count (first table))))
          (is (= 1024 (:bytes (first table))))
          (is (= "Ljava/lang/String;" (:object-type (first table)))))))

    (testing "handles empty hotspots"
      (reset! core/accumulated [])
      (view/allocation-hotspots*
       :kindly
       {}
       {:allocation-hotspots
        {:type :criterium/allocation-hotspots
         :hotspots []}})
      (is (empty? @core/accumulated)))

    (testing "handles nil object-type"
      (reset! core/accumulated [])
      (view/allocation-hotspots*
       :kindly
       {:hotspots-id :my-hotspots}
       {:my-hotspots
        {:type :criterium/allocation-hotspots
         :hotspots [{:call-site {:call-class "x.y$z"
                                 :call-method "run"
                                 :call-file "z.clj"
                                 :call-line 1}
                     :count 10
                     :bytes 100
                     :freed-count 5
                     :freed-bytes 50}]}})
      (let [result (core/flush)]
        (is (= 2 (count result)))
        (let [[_ table] result]
          (is (= "" (:object-type (first table)))))))))

;;; Allocations by Type Tests

(deftest allocation-by-type-kindly-test
  ;; Tests the kindly viewer output for allocation-by-type results.
  ;; Verifies table format sorted by bytes descending.
  (testing "allocation-by-type*"
    (testing "renders by-type as heading and table sorted by bytes"
      (reset! core/accumulated [])
      (view/allocation-by-type*
       :kindly
       {}
       {:allocation-by-type
        {:type :criterium/allocation-by-type
         :by-type {"Ljava/lang/String;" {:count 50
                                         :bytes 256
                                         :freed-count 25
                                         :freed-bytes 128}
                   "[B" {:count 100
                         :bytes 1024
                         :freed-count 50
                         :freed-bytes 512}}}})
      (let [result (core/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result)) "Expected heading and table")
        (let [[heading table] result]
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (= ["**Allocations by Type**"] heading))
          (is (= :kind/table (:kindly/kind (meta table))))
          (is (= 2 (count table)))
          ;; Sorted by bytes descending
          (is (= "[B" (:type (first table))))
          (is (= 1024 (:bytes (first table))))
          (is (= "Ljava/lang/String;" (:type (second table)))))))

    (testing "handles empty by-type"
      (reset! core/accumulated [])
      (view/allocation-by-type*
       :kindly
       {}
       {:allocation-by-type
        {:type :criterium/allocation-by-type
         :by-type {}}})
      (is (empty? @core/accumulated)))

    (testing "uses custom by-type-id"
      (reset! core/accumulated [])
      (view/allocation-by-type*
       :kindly
       {:by-type-id :my-by-type}
       {:my-by-type
        {:type :criterium/allocation-by-type
         :by-type {"Ljava/lang/Object;" {:count 10
                                         :bytes 100
                                         :freed-count 5
                                         :freed-bytes 50}}}})
      (let [result (core/flush)]
        (is (= 2 (count result)))
        (let [[_ table] result]
          (is (= "Ljava/lang/Object;" (:type (first table)))))))))

;;; Allocation Treemap Tests

(deftest allocation-treemap-kindly-test
  ;; Tests the kindly viewer output for allocation-treemap results.
  ;; Verifies that Vega treemap chart is rendered.
  (testing "allocation-treemap*"
    (testing "renders treemap as heading and Vega chart"
      (reset! core/accumulated [])
      (let [treemap-data {:type :criterium/allocation-treemap
                          :group-by :class->line->type
                          :size-by :bytes
                          :root {:name "allocations"
                                 :value 1024
                                 :children [{:name "MyClass"
                                             :value 1024
                                             :children [{:name "L42"
                                                         :value 1024
                                                         :children [{:name "String"
                                                                     :value 1024}]}]}]}}]
        (view/allocation-treemap*
         :kindly
         {}
         {:allocation-treemap treemap-data})
        (let [result (core/flush)]
          (is (= :kind/fragment (:kindly/kind (meta result))))
          (is (= 2 (count result)) "Expected heading and chart")
          (let [[heading chart] result]
            (is (= :kind/md (:kindly/kind (meta heading))))
            (is (= ["**Allocation Treemap**"] heading))
            (is (= :kind/vega (:kindly/kind (meta chart))))
            (is (string? (:$schema chart)))))))

    (testing "handles missing treemap data"
      (reset! core/accumulated [])
      (view/allocation-treemap* :kindly {} {})
      (is (empty? @core/accumulated)))

    (testing "handles nil root"
      (reset! core/accumulated [])
      (view/allocation-treemap*
       :kindly
       {}
       {:allocation-treemap {:type :criterium/allocation-treemap
                             :root nil}})
      (is (empty? @core/accumulated)))

    (testing "uses custom treemap-id"
      (reset! core/accumulated [])
      (let [treemap-data {:type :criterium/allocation-treemap
                          :group-by :type->class->line
                          :size-by :count
                          :root {:name "allocations"
                                 :value 100
                                 :children [{:name "Object" :value 100}]}}]
        (view/allocation-treemap*
         :kindly
         {:treemap-id :my-treemap}
         {:my-treemap treemap-data})
        (let [result (core/flush)]
          (is (= 2 (count result))))))))
