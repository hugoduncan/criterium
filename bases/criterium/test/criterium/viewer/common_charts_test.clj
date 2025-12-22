(ns criterium.viewer.common-charts-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common-charts :as charts]))

;; Tests for treemap-vega-spec function.
;; Verifies Vega spec generation for treemap visualizations of allocation data.

(def sample-treemap
  "Sample allocation treemap for testing."
  {:type :criterium/allocation-treemap
   :group-by :class→line→type
   :size-by :bytes
   :root {:name "allocations"
          :value 1000
          :children [{:name "MyClass"
                      :value 800
                      :children [{:name "L42"
                                  :value 500}
                                 {:name "L50"
                                  :value 300}]}
                     {:name "OtherClass"
                      :value 200}]}})

(deftest treemap-vega-spec-test
  (testing "treemap-vega-spec"
    (testing "produces valid Vega spec structure"
      (let [spec (charts/treemap-vega-spec sample-treemap {})]
        (is (map? spec))
        (is (contains? spec :$schema))
        (is (contains? spec :width))
        (is (contains? spec :height))
        (is (contains? spec :data))
        (is (contains? spec :scales))
        (is (contains? spec :marks))
        (is (vector? (:data spec)))
        (is (vector? (:scales spec)))
        (is (vector? (:marks spec)))))

    (testing "includes correct $schema"
      (let [spec (charts/treemap-vega-spec sample-treemap {})]
        (is (= "https://vega.github.io/schema/vega/v5.json" (:$schema spec)))))

    (testing "data values match input hierarchy"
      (let [spec (charts/treemap-vega-spec sample-treemap {})
            tree-data (first (:data spec))
            values (:values tree-data)]
        (is (= "tree" (:name tree-data)))
        (is (= 5 (count values)) "Expected 5 nodes in flattened tree")
        (is (= "root" (:id (first values))))
        (is (nil? (:parent (first values))))
        (is (= "allocations" (:name (first values))))
        (is (= 1000 (:value (first values))))
        ;; Check a child node
        (let [myclass-node (second values)]
          (is (= "allocations/MyClass" (:id myclass-node)))
          (is (= "root" (:parent myclass-node)))
          (is (= "MyClass" (:name myclass-node)))
          (is (= 800 (:value myclass-node))))))

    (testing "respects width/height options"
      (let [spec (charts/treemap-vega-spec sample-treemap {:width 500 :height 300})]
        (is (= 500 (:width spec)))
        (is (= 300 (:height spec)))))

    (testing "uses default width/height when not specified"
      (let [spec (charts/treemap-vega-spec sample-treemap {})]
        (is (= 700 (:width spec)))
        (is (= 400 (:height spec)))))

    (testing "respects color-scheme option"
      (let [spec (charts/treemap-vega-spec sample-treemap {:color-scheme "category20"})
            color-scale (first (:scales spec))]
        (is (= {:scheme "category20"} (:range color-scale)))))

    (testing "with empty children produces minimal spec"
      (let [empty-treemap {:type :criterium/allocation-treemap
                           :root {:name "allocations"
                                  :value 0}}
            spec (charts/treemap-vega-spec empty-treemap {})]
        (is (map? spec))
        (is (= "https://vega.github.io/schema/vega/v5.json" (:$schema spec)))
        (let [values (-> spec :data first :values)]
          (is (= 1 (count values)))
          (is (= "root" (:id (first values)))))))

    (testing "with nil root produces empty data"
      (let [nil-treemap {:type :criterium/allocation-treemap
                         :root nil}
            spec (charts/treemap-vega-spec nil-treemap {})]
        (is (map? spec))
        (is (empty? (-> spec :data first :values)))))

    (testing "includes stratify and treemap transforms"
      (let [spec (charts/treemap-vega-spec sample-treemap {})
            transforms (-> spec :data first :transform)]
        (is (= 2 (count transforms)))
        (is (= "stratify" (:type (first transforms))))
        (is (= "treemap" (:type (second transforms))))
        (is (= "squarify" (:method (second transforms))))))

    (testing "includes three mark types"
      (let [spec (charts/treemap-vega-spec sample-treemap {})
            marks (:marks spec)]
        (is (= 3 (count marks)))
        (is (= "rect" (:type (first marks))))
        (is (= "rect" (:type (second marks))))
        (is (= "text" (:type (nth marks 2))))))))
