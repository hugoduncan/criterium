(ns criterium.viewer.portal.allocation-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.view :as view]
   [criterium.viewer.portal.allocation]
   [criterium.viewer.portal.core :as portal.core]))

;;; Allocation Summary Tests

(deftest allocation-summary-test
  ;; Tests the portal viewer output for allocation-summary results.
  ;; Verifies table structure with metrics, retained calculation, and freed ratio.
  (testing "allocation-summary*"
    (testing "produces table with summary statistics"
      (let [tapped (atom [])
            data-map {:allocation-summary
                      {:type :criterium/allocation-summary
                       :total-allocated 1024
                       :total-freed 512
                       :num-allocations 100
                       :num-freed 50
                       :freed-ratio 0.5}}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/allocation-summary* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (= 6 (count rows)))
          (is (= {:metric "Total allocated" :value 1024} (nth rows 0)))
          (is (= {:metric "Total freed" :value 512} (nth rows 1)))
          (is (= {:metric "Retained" :value 512} (nth rows 2)))
          (is (= {:metric "Allocation count" :value 100} (nth rows 3)))
          (is (= {:metric "Freed count" :value 50} (nth rows 4)))
          (is (= {:metric "Freed ratio" :value "50.0%"} (nth rows 5))))))

    (testing "uses custom summary-id"
      (let [tapped (atom [])
            data-map {:my-summary
                      {:type :criterium/allocation-summary
                       :total-allocated 256
                       :total-freed 128
                       :num-allocations 10
                       :num-freed 5
                       :freed-ratio 0.5}}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/allocation-summary* :portal {:summary-id :my-summary} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (= {:metric "Retained" :value 128} (nth rows 2))))))

    (testing "returns nil when summary not found"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (is (nil? (view/allocation-summary* :portal {} {}))))
        (is (empty? @tapped))))))

;;; Allocation Hotspots Tests

(deftest allocation-hotspots-test
  ;; Tests the portal viewer output for allocation-hotspots results.
  ;; Verifies table format with call site, object type, counts, and byte amounts.
  (testing "allocation-hotspots*"
    (testing "produces table with hotspot data"
      (let [tapped (atom [])
            data-map {:allocation-hotspots
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
                                   :freed-bytes 128}]}}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/allocation-hotspots* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (= 2 (count rows)))
          (is (= "Ljava/lang/String;" (:object-type (first rows))))
          (is (= 100 (:count (first rows))))
          (is (= 1024 (:bytes (first rows)))))))

    (testing "handles nil object-type"
      (let [tapped (atom [])
            data-map {:allocation-hotspots
                      {:type :criterium/allocation-hotspots
                       :hotspots [{:call-site {:call-class "x.y$z"
                                               :call-method "run"
                                               :call-file "z.clj"
                                               :call-line 1}
                                   :count 10
                                   :bytes 100
                                   :freed-count 5
                                   :freed-bytes 50}]}}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/allocation-hotspots* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (= "" (:object-type (first rows)))))))

    (testing "returns nil for empty hotspots"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/allocation-hotspots*
           :portal {}
           {:allocation-hotspots {:type :criterium/allocation-hotspots
                                  :hotspots []}}))
        (is (empty? @tapped))))))

;;; Allocations by Type Tests

(deftest allocation-by-type-test
  ;; Tests the portal viewer output for allocation-by-type results.
  ;; Verifies table format sorted by bytes descending.
  (testing "allocation-by-type*"
    (testing "produces table sorted by bytes"
      (let [tapped (atom [])
            data-map {:allocation-by-type
                      {:type :criterium/allocation-by-type
                       :by-type {"Ljava/lang/String;" {:count 50
                                                       :bytes 256
                                                       :freed-count 25
                                                       :freed-bytes 128}
                                 "[B" {:count 100
                                       :bytes 1024
                                       :freed-count 50
                                       :freed-bytes 512}}}}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/allocation-by-type* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (= 2 (count rows)))
          ;; Sorted by bytes descending: [B (1024) before String (256)
          (is (= "[B" (:type (first rows))))
          (is (= 1024 (:bytes (first rows))))
          (is (= "Ljava/lang/String;" (:type (second rows)))))))

    (testing "uses custom by-type-id"
      (let [tapped (atom [])
            data-map {:my-by-type
                      {:type :criterium/allocation-by-type
                       :by-type {"Ljava/lang/Object;" {:count 10
                                                       :bytes 100
                                                       :freed-count 5
                                                       :freed-bytes 50}}}}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/allocation-by-type* :portal {:by-type-id :my-by-type} data-map))
        (is (= 1 (count @tapped)))
        (let [rows (first @tapped)]
          (is (= "Ljava/lang/Object;" (:type (first rows)))))))

    (testing "returns nil for empty by-type"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-table #(swap! tapped conj %)]
          (view/allocation-by-type*
           :portal {}
           {:allocation-by-type {:type :criterium/allocation-by-type
                                 :by-type {}}}))
        (is (empty? @tapped))))))

;;; Allocation Treemap Tests

(deftest allocation-treemap-test
  ;; Tests the portal viewer output for allocation-treemap results.
  ;; Verifies that Vega spec is produced for treemap visualization.
  (testing "allocation-treemap*"
    (testing "produces Vega spec for treemap"
      (let [tapped (atom [])
            treemap-data {:type :criterium/allocation-treemap
                          :group-by :class->line->type
                          :size-by :bytes
                          :root {:name "allocations"
                                 :value 1024
                                 :children [{:name "MyClass"
                                             :value 1024
                                             :children [{:name "L42"
                                                         :value 1024}]}]}}
            data-map {:allocation-treemap treemap-data}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega #(swap! tapped conj %)]
          (view/allocation-treemap* :portal {} data-map))
        (is (= 1 (count @tapped)))
        (let [spec (first @tapped)]
          (is (map? spec))
          (is (contains? spec :data)))))

    (testing "uses custom treemap-id"
      (let [tapped (atom [])
            treemap-data {:type :criterium/allocation-treemap
                          :group-by :type->class->line
                          :size-by :count
                          :root {:name "allocations"
                                 :value 100}}
            data-map {:my-treemap treemap-data}]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega #(swap! tapped conj %)]
          (view/allocation-treemap* :portal {:treemap-id :my-treemap} data-map))
        (is (= 1 (count @tapped)))))

    (testing "returns nil when treemap not found"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega #(swap! tapped conj %)]
          (is (nil? (view/allocation-treemap* :portal {} {}))))
        (is (empty? @tapped))))

    (testing "returns nil for nil root"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading (fn [_])
                      portal.core/portal-vega #(swap! tapped conj %)]
          (view/allocation-treemap*
           :portal {}
           {:allocation-treemap {:type :criterium/allocation-treemap
                                 :root nil}}))
        (is (empty? @tapped))))))
