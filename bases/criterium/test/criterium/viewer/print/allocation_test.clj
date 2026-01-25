(ns criterium.viewer.print.allocation-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.print.allocation]))

;;; Allocation Summary Tests

(deftest allocation-summary-print-test
  ;; Tests the print viewer output for allocation-summary results.
  ;; Verifies display of totals, counts, and freed ratio (based on bytes).
  (testing "allocation-summary*"
    (testing "prints summary statistics with right-justified numbers"
      (is (= [""
              "Allocation Summary:"
              "Total allocated:         1024 bytes"
              "Total freed:          512 bytes"
              "Retained:          512 bytes"
              "Allocation count:          100"
              "Freed count:           50"
              "Freed ratio:         50.0%"]
             (trimmed-lines
              (with-out-str
                (view/allocation-summary*
                 :print
                 {}
                 {:allocation-summary
                  {:type :criterium/allocation-summary
                   :total-allocated 1024
                   :total-freed 512
                   :num-allocations 100
                   :num-freed 50
                   :freed-ratio 0.5}}))))))
    (testing "handles zero allocations"
      (is (= [""
              "Allocation Summary:"
              "Total allocated:            0 bytes"
              "Total freed:            0 bytes"
              "Retained:            0 bytes"
              "Allocation count:            0"
              "Freed count:            0"
              "Freed ratio:          0.0%"]
             (trimmed-lines
              (with-out-str
                (view/allocation-summary*
                 :print
                 {}
                 {:allocation-summary
                  {:type :criterium/allocation-summary
                   :total-allocated 0
                   :total-freed 0
                   :num-allocations 0
                   :num-freed 0
                   :freed-ratio 0.0}}))))))
    (testing "uses custom summary-id"
      (is (= [""
              "Allocation Summary:"
              "Total allocated:          256 bytes"
              "Total freed:          128 bytes"
              "Retained:          128 bytes"
              "Allocation count:           10"
              "Freed count:            5"
              "Freed ratio:         50.0%"]
             (trimmed-lines
              (with-out-str
                (view/allocation-summary*
                 :print
                 {:summary-id :my-summary}
                 {:my-summary
                  {:type :criterium/allocation-summary
                   :total-allocated 256
                   :total-freed 128
                   :num-allocations 10
                   :num-freed 5
                   :freed-ratio 0.5}}))))))))

;;; Allocation Hotspots Tests

(deftest allocation-hotspots-print-test
  ;; Tests the print viewer output for allocation-hotspots results.
  ;; Verifies table format with call site, object type, counts, and byte amounts.
  ;; Object type is truncated from the start to fit the 30-char column.
  (testing "allocation-hotspots*"
    (testing "prints hotspots table with object type"
      (is (= [""
              "Allocation Hotspots:"
              "Count        Bytes    Freed  Freed Bytes  Object Type                     Call Site"
              (apply str (repeat 110 "-"))
              "100         1024       50          512  Ljava/lang/String;              my.ns$fn.invoke (my_ns.clj:42)"
              "50          256       25          128  Ljava/lang/Object;              other.ns$g.apply (other.clj:10)"]
             (trimmed-lines
              (with-out-str
                (view/allocation-hotspots*
                 :print
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
                               :freed-bytes 128}]}}))))))
    (testing "truncates long object type from the start"
      ;; "Ljava/util/concurrent/ArrayBlockingQueue;" is 41 chars
      ;; With 30-char column, keeps last 29 chars + ellipsis
      (is (= [""
              "Allocation Hotspots:"
              "Count        Bytes    Freed  Freed Bytes  Object Type                     Call Site"
              (apply str (repeat 110 "-"))
              "10          100        5           50  …oncurrent/ArrayBlockingQueue;  x.y$z.run (z.clj:1)"]
             (trimmed-lines
              (with-out-str
                (view/allocation-hotspots*
                 :print
                 {:hotspots-id :my-hotspots}
                 {:my-hotspots
                  {:type :criterium/allocation-hotspots
                   :hotspots [{:call-site {:call-class "x.y$z"
                                           :call-method "run"
                                           :call-file "z.clj"
                                           :call-line 1}
                               :object-type "Ljava/util/concurrent/ArrayBlockingQueue;"
                               :count 10
                               :bytes 100
                               :freed-count 5
                               :freed-bytes 50}]}}))))))
    (testing "handles empty hotspots gracefully"
      (let [output (with-out-str
                     (view/allocation-hotspots*
                      :print
                      {}
                      {:allocation-hotspots
                       {:type :criterium/allocation-hotspots
                        :hotspots []}}))]
        (is (str/blank? output))))
    (testing "handles nil object-type"
      (is (= [""
              "Allocation Hotspots:"
              "Count        Bytes    Freed  Freed Bytes  Object Type                     Call Site"
              (apply str (repeat 110 "-"))
              "10          100        5           50                                  x.y$z.run (z.clj:1)"]
             (trimmed-lines
              (with-out-str
                (view/allocation-hotspots*
                 :print
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
                               :freed-bytes 50}]}}))))))))

;;; Allocations by Type Tests

(deftest allocation-by-type-print-test
  ;; Tests the print viewer output for allocation-by-type results.
  ;; Verifies table format sorted by bytes descending.
  (testing "allocation-by-type*"
    (testing "prints by-type table sorted by bytes"
      (is (= [""
              "Allocations by Type:"
              "Count        Bytes    Freed  Freed Bytes  Type"
              (apply str (repeat 80 "-"))
              "100         1024       50          512  [B"
              "50          256       25          128  Ljava/lang/String;"]
             (trimmed-lines
              (with-out-str
                (view/allocation-by-type*
                 :print
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
                                   :freed-bytes 512}}}}))))))
    (testing "handles empty by-type gracefully"
      (let [output (with-out-str
                     (view/allocation-by-type*
                      :print
                      {}
                      {:allocation-by-type
                       {:type :criterium/allocation-by-type
                        :by-type {}}}))]
        (is (str/blank? output))))
    (testing "uses custom by-type-id"
      (is (= [""
              "Allocations by Type:"
              "Count        Bytes    Freed  Freed Bytes  Type"
              (apply str (repeat 80 "-"))
              "10          100        5           50  Ljava/lang/Object;"]
             (trimmed-lines
              (with-out-str
                (view/allocation-by-type*
                 :print
                 {:by-type-id :my-by-type}
                 {:my-by-type
                  {:type :criterium/allocation-by-type
                   :by-type {"Ljava/lang/Object;" {:count 10
                                                   :bytes 100
                                                   :freed-count 5
                                                   :freed-bytes 50}}}}))))))))

;;; Allocation Treemap Tests

(deftest allocation-treemap-print-test
  ;; Tests the print viewer output for allocation-treemap results.
  ;; Verifies that ASCII treemap is rendered with proper structure.
  (testing "allocation-treemap*"
    (testing "prints ASCII tree structure with header"
      (let [treemap-data {:type :criterium/allocation-treemap
                          :group-by :class→line→type
                          :size-by :bytes
                          :root {:name "allocations"
                                 :value 1024
                                 :children [{:name "MyClass"
                                             :value 1024
                                             :children [{:name "L42"
                                                         :value 1024
                                                         :children [{:name "String"
                                                                     :value 1024}]}]}]}}
            output (with-out-str
                     (view/allocation-treemap*
                      :print
                      {}
                      {:allocation-treemap treemap-data}))
            lines (str/split-lines output)]
        (is (str/includes? (first (drop-while str/blank? lines))
                           "Allocation Treemap"))
        (is (str/includes? output "class→line→type"))
        (is (str/includes? output "bytes"))
        (is (str/includes? output "allocations/"))
        (is (str/includes? output "MyClass/"))
        (is (str/includes? output "L42/"))
        (is (str/includes? output "String"))))

    (testing "handles missing treemap data"
      (let [output (with-out-str
                     (view/allocation-treemap*
                      :print
                      {}
                      {}))]
        (is (str/blank? output))))

    (testing "handles nil root"
      (let [output (with-out-str
                     (view/allocation-treemap*
                      :print
                      {}
                      {:allocation-treemap {:type :criterium/allocation-treemap
                                            :root nil}}))]
        (is (str/blank? output))))

    (testing "uses custom treemap-id"
      (let [treemap-data {:type :criterium/allocation-treemap
                          :group-by :type→class→line
                          :size-by :count
                          :root {:name "allocations"
                                 :value 100
                                 :children [{:name "Object" :value 100}]}}
            output (with-out-str
                     (view/allocation-treemap*
                      :print
                      {:treemap-id :my-treemap}
                      {:my-treemap treemap-data}))]
        (is (str/includes? output "type→class→line"))
        (is (str/includes? output "count"))
        (is (str/includes? output "Object"))))))
