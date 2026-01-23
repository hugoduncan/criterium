(ns criterium.viewer.kindly-autocorrelation-views-test
  ;; Tests autocorrelation views in the Kindly viewer.
  ;; Covers: autocorrelation-classification with anomalous lags display.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.view :as view]
   [criterium.viewer.kindly :as kindly]))

(deftest autocorrelation-classification-anomalous-lags-test
  ;; Tests that autocorrelation-classification* displays anomalous lags when present.
  ;; Verifies the table includes anomalous lags row with severity for each lag.
  (testing "view/autocorrelation-classification* :kindly"
    (testing "displays anomalous lags when present"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation-classification
                      {:type :criterium/autocorrelation-classification
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation-raw
                       :classification-data
                       {[:elapsed-time]
                        {:ljung-box {:q-statistic 25.0 :df 20 :p-value 0.20}
                         :pattern :drift
                         :classification :warning
                         :detected-period nil}}}
                      :autocorrelation-raw
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.15 2 0.12 12 0.25}
                         :lag-1 {:value 0.15 :severity :minor}
                         :lag-severities {1 :minor 2 :minor 12 :moderate}
                         :anomalous-lags [1 2 12]
                         :effective-sample-size {:n-original 200
                                                 :n-effective 160
                                                 :ratio 0.80}}}}}]
        (view/autocorrelation-classification*
         :kindly
         {:classification-id :autocorrelation-classification
          :autocorrelation-id :autocorrelation-raw}
         data-map)
        (let [result (kindly/flush)
              tables (filter #(= :kind/table (:kindly/kind (meta %))) result)]
          (is (seq tables) "Should have at least one table")
          (let [table-data (first tables)
                metrics (map :metric table-data)
                values (map :value table-data)]
            (is (some #(str/includes? % "Anomalous lags") metrics)
                "Should include anomalous lags in table metrics")
            (is (some #(and (string? %)
                            (str/includes? % "1 (minor)")
                            (str/includes? % "12 (moderate)"))
                      values)
                "Should format anomalous lags with severities")))))

    (testing "does not display anomalous lags when empty"
      (reset! kindly/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            data-map {:autocorrelation-classification
                      {:type :criterium/autocorrelation-classification
                       :metrics-defs metrics-defs
                       :source-id :autocorrelation-raw
                       :classification-data
                       {[:elapsed-time]
                        {:ljung-box {:q-statistic 25.0 :df 20 :p-value 0.20}
                         :pattern :drift
                         :classification :warning
                         :detected-period nil}}}
                      :autocorrelation-raw
                      {:type :criterium/autocorrelation
                       :metrics-defs metrics-defs
                       :source-id :samples
                       :autocorrelation
                       {[:elapsed-time]
                        {:acf {1 0.05 2 0.03}
                         :lag-1 {:value 0.05 :severity :none}
                         :lag-severities {1 :none 2 :none}
                         :anomalous-lags []
                         :effective-sample-size {:n-original 200
                                                 :n-effective 195
                                                 :ratio 0.975}}}}}]
        (view/autocorrelation-classification*
         :kindly
         {:classification-id :autocorrelation-classification
          :autocorrelation-id :autocorrelation-raw}
         data-map)
        (let [result (kindly/flush)
              tables (filter #(= :kind/table (:kindly/kind (meta %))) result)]
          (when (seq tables)
            (let [table-data (first tables)
                  metrics (map :metric table-data)]
              (is (not (some #(str/includes? (str %) "Anomalous lags") metrics))
                  "Should not include anomalous lags when empty"))))))))
