(ns criterium.viewer.kindly.autocorrelation-test
  ;; Tests multimethod registration, data processing, and output formatting
  ;; for autocorrelation analysis views in kindly viewer.
  ;;
  ;; Autocorrelation views display ACF plots, classification assessments,
  ;; and effective sample size statistics.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.view :as view]
   [criterium.viewer.kindly.autocorrelation]
   [criterium.viewer.kindly.core :as kindly.core]))

(deftest autocorrelation-kindly-test
  ;; Tests autocorrelation* multimethod (no-op for kindly, uses acf-plot instead)
  (testing "autocorrelation*"
    (testing "returns nil for kindly viewer"
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.12 2 0.08}
                         :lag-1 {:value 0.12 :severity :minor}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 157
                                                 :ratio 0.785}}}}}]
        (is (nil? (view/autocorrelation* :kindly {} data-map)))))))

(deftest acf-plot-kindly-test
  ;; Tests acf-plot* multimethod for rendering ACF charts in Kindly
  (testing "acf-plot*"
    (testing "produces vega-lite spec when data exists"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.12 2 0.08 3 0.05}
                         :lag-1 {:value 0.12 :severity :minor}
                         :effective-sample-size {:n-original 200
                                                 :n-effective 157
                                                 :ratio 0.785}}}}}]
        (view/acf-plot* :kindly {} data-map)
        (let [acc @kindly.core/accumulated]
          (is (= 2 (count acc)))
          (is (= :kind/md (:kindly/kind (meta (first acc)))))
          (is (re-find #"Autocorrelation.*Elapsed Time" (first (first acc))))
          (is (= :kind/vega-lite (:kindly/kind (meta (second acc)))))
          (let [spec (second acc)]
            (is (contains? spec :layer))
            (is (contains? spec :title))))))

    (testing "uses custom autocorrelation-id"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:my-acf
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.08}
                         :lag-1 {:value 0.08 :severity :none}
                         :effective-sample-size {:n-original 100
                                                 :n-effective 95
                                                 :ratio 0.95}}}}}]
        (view/acf-plot* :kindly {:autocorrelation-id :my-acf} data-map)
        (is (= 2 (count @kindly.core/accumulated)))))

    (testing "handles missing data gracefully"
      (reset! kindly.core/accumulated [])
      (view/acf-plot* :kindly {} {})
      (is (empty? @kindly.core/accumulated)))))

(deftest autocorrelation-classification-kindly-test
  ;; Tests autocorrelation-classification* multimethod for displaying
  ;; sample independence assessment tables in Kindly
  (testing "autocorrelation-classification*"
    (testing "produces table for warning classification"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation-classification
                      {:type :criterium/autocorrelation-classification
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation
                       :classification-data
                       {[:elapsed-time]
                        {:ljung-box {:q-statistic 35.0 :df 20 :p-value 0.02}
                         :classification :warning
                         :pattern :transient-effects
                         :detected-period nil}}}
                      :autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.25}
                         :lag-1 {:value 0.25 :severity :moderate}
                         :anomalous-lags [1]
                         :lag-severities {1 :moderate}}}}}]
        (view/autocorrelation-classification* :kindly {} data-map)
        (let [acc @kindly.core/accumulated]
          (is (= 2 (count acc)))
          (is (= :kind/md (:kindly/kind (meta (first acc)))))
          (is (re-find #"Sample Independence Classification" (first (first acc))))
          (is (= :kind/table (:kindly/kind (meta (second acc)))))
          (let [table (second acc)]
            (is (some #(= "Assessment" (:metric %)) table))
            (is (some #(= "Warning" (:value %)) table))
            (is (some #(= "Pattern" (:metric %)) table))))))

    (testing "does not produce output for :pass classification"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation-classification
                      {:type :criterium/autocorrelation-classification
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation
                       :classification-data
                       {[:elapsed-time]
                        {:ljung-box {:q-statistic 12.0 :df 20 :p-value 0.85}
                         :classification :pass
                         :pattern :clean
                         :detected-period nil}}}
                      :autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.05}
                         :lag-1 {:value 0.05 :severity :none}}}}}]
        (view/autocorrelation-classification* :kindly {} data-map)
        (is (empty? @kindly.core/accumulated))))

    (testing "handles missing data gracefully"
      (reset! kindly.core/accumulated [])
      (view/autocorrelation-classification* :kindly {} {})
      (is (empty? @kindly.core/accumulated)))))

(deftest effective-sample-size-kindly-test
  ;; Tests effective-sample-size* multimethod for displaying ESS statistics
  (testing "effective-sample-size*"
    (testing "produces table when ci-inflation-factor > 1"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:effective-sample-size
                      {:type :criterium/effective-sample-size
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation
                       :effective-sample-size-data
                       {[:elapsed-time]
                        {:effective-sample-size {:n-original 200
                                                 :n-effective 120
                                                 :ratio 0.60}
                         :ci-inflation-factor 1.67}}}
                      :autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :autocorrelation
                       {[:elapsed-time]
                        {:lag-1 {:value 0.25 :severity :moderate}}}}}]
        (view/effective-sample-size* :kindly {} data-map)
        (let [acc @kindly.core/accumulated]
          (is (= 2 (count acc)))
          (is (= :kind/md (:kindly/kind (meta (first acc)))))
          (is (re-find #"Effective Sample Size" (first (first acc))))
          (is (= :kind/table (:kindly/kind (meta (second acc)))))
          (let [table (second acc)]
            (is (some #(= "Effective sample size" (:metric %)) table))
            (is (some #(= "CI inflation factor" (:metric %)) table))))))

    (testing "does not produce output when ci-inflation-factor = 1"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:effective-sample-size
                      {:type :criterium/effective-sample-size
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation
                       :effective-sample-size-data
                       {[:elapsed-time]
                        {:effective-sample-size {:n-original 200
                                                 :n-effective 200
                                                 :ratio 1.0}
                         :ci-inflation-factor 1.0}}}
                      :autocorrelation
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :autocorrelation
                       {[:elapsed-time]
                        {:lag-1 {:value 0.0 :severity :none}}}}}]
        (view/effective-sample-size* :kindly {} data-map)
        (is (empty? @kindly.core/accumulated))))

    (testing "handles missing data gracefully"
      (reset! kindly.core/accumulated [])
      (view/effective-sample-size* :kindly {} {})
      (is (empty? @kindly.core/accumulated)))))
