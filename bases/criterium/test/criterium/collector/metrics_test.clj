(ns criterium.collector.metrics-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.metric :as metric]))

(deftest filter-metric-values-test
  (testing "filter-metric-values"
    (let [values [{:dimension :time :value 1}
                  {:dimension :memory :value 2}
                  {:dimension :time :value 3}]]
      (testing "filters by predicate"
        (is (= [{:dimension :time :value 1}
                {:dimension :time :value 3}]
               (metric/filter-metric-values
                #(= :time (:dimension %))
                values))))
      (testing "returns empty vector when no matches"
        (is (= []
               (metric/filter-metric-values
                #(= :not-found (:dimension %))
                values))))

      (testing "returns empty vector for empty input"
        (is (= []
               (metric/filter-metric-values
                #(= :time (:dimension %))
                [])))))))

(deftest filter-metrics-test
  (testing "filter-metrics"
    (let [metric-map {:type   :quantitative
                      :values [{:dimension :time :value 1}
                               {:dimension :memory :value 2}]
                      :groups {"group1"
                               {:values [{:dimension :time :value 3}
                                         {:dimension :count :value 4}]}
                               "group2"
                               {:values [{:dimension :memory :value 5}]}}}]

      (testing "filters values at top level"
        (let [result (metric/filter-metrics
                      metric-map
                      #(= :time (:dimension %)))]
          (is (= [{:dimension :time :value 1}]
                 (:values result)))
          (is (contains? result :type))))

      (testing "filters nested groups"
        (let [result (metric/filter-metrics
                      metric-map
                      #(= :time (:dimension %)))]
          (is (= {"group1"
                  {:values [{:dimension :time :value 3}]}}
                 (:groups result)))))

      (testing "removes empty groups"
        (let [result (metric/filter-metrics
                      metric-map
                      #(= :count (:dimension %)))]
          (is (= {"group1"
                  {:values [{:dimension :count :value 4}]}}
                 (:groups result)))
          (is (empty? (:values result))))))))

(deftest dimension-pred-test
  (testing "dimension-pred"
    (let [pred (metric/dimension-pred :time)]
      (testing "matches correct dimension"
        (is (pred {:dimension :time :value 1})))

      (testing "does not match different dimension"
        (is (not (pred {:dimension :memory :value 1}))))

      (testing "does not match missing dimension"
        (is (not (pred {:value 1})))))))

(deftest type-pred-test
  (testing "type-pred"
    (let [pred (metric/type-pred :event)]
      (testing "matches correct type"
        (is (pred {:type :event :value 1})))

      (testing "does not match different type"
        (is (not (pred {:type :quantitative :value 1}))))

      (testing "does not match missing type"
        (is (not (pred {:value 1})))))))

(deftest gc-integration-test
  (testing "garbage collector metric filtering"
    (let [gc-metric (:garbage-collector (metrics/metrics))]
      (testing "filters time dimension across groups"
        (let [result (metric/filter-metrics
                      gc-metric
                      (metric/dimension-pred :time))]
          (is (every? (comp #{:time} :dimension)
                      (mapcat
                       (comp :values second)
                       (:groups result))))
          (is (= (count (:groups gc-metric))
                 (count (:groups result))))))

      (testing "filters count dimension across groups"
        (let [result (metric/filter-metrics
                      gc-metric
                      (metric/dimension-pred :count))]
          (is (every? (comp #{:count} :dimension)
                      (mapcat
                       (comp :values second)
                       (:groups result))))
          (is (= (count (:groups gc-metric))
                 (count (:groups result))))))

      (testing "filters by event type"
        (let [result (metric/filter-metrics
                      gc-metric
                      (metric/type-pred :event))]
          (is (= gc-metric result))))

      (testing "removes all groups for non-matching type"
        (let [result (metric/filter-metrics
                      gc-metric
                      (metric/type-pred :quantitative))]
          (is (empty? (:groups result))))))))
