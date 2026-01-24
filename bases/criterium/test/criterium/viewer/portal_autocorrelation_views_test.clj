(ns criterium.viewer.portal-autocorrelation-views-test
  ;; Tests autocorrelation views in the Portal viewer.
  ;; Covers: autocorrelation-classification with anomalous lags display.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.view :as view]
   [criterium.viewer.portal :as portal])
  (:import
   [java.util Queue]))

(defn- with-tap-out*
  "Implementation for with-tap-out macro.
   Waits for expected-count taps to be received."
  [expected-count body-fn]
  (let [v (volatile! [])
        f (fn [x]
            (when-not (= ::portal/_ x)
              (vswap! v conj x)))]
    (try
      (add-tap f)
      (body-fn)
      ;; Wait for tap queue to drain
      (loop []
        (when-not (.isEmpty ^Queue @#'clojure.core/tapq)
          (recur)))
      ;; Wait for expected number of values
      (loop [attempts (long 0)]
        (when (and (< (count @v) (long expected-count))
                   (< attempts 10000))
          (recur (inc attempts))))
      (portal/flush)
      @v
      (finally
        (remove-tap f)))))

(defmacro ^:private with-tap-out-n
  "Capture tapped values during body execution, waiting for n values."
  [n & body]
  `(with-tap-out* ~n (fn [] ~@body)))

(deftest autocorrelation-classification-anomalous-lags-test
  ;; Tests that autocorrelation-classification* displays anomalous lags when present.
  ;; Verifies the output includes anomalous lags with severity for each lag.
  (testing "view/autocorrelation-classification* :portal"
    (testing "displays anomalous lags when present"
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
                                                 :ratio 0.80}}}}}
            tapped (with-tap-out-n 2
                     (view/autocorrelation-classification*
                      :portal
                      {:classification-id :autocorrelation-classification
                       :autocorrelation-id :autocorrelation-raw}
                      data-map))
            ;; Find the table (vector of maps with :metric and :value keys)
            table-data (some #(when (and (vector? %)
                                         (every? map? %)
                                         (every? :metric %))
                                %)
                             tapped)]
        (is table-data "Should have table with :metric keys")
        (let [metrics (map :metric table-data)
              values (map :value table-data)]
          (is (some #(and (string? %) (str/includes? % "Anomalous lags")) metrics)
              "Should include anomalous lags in table metrics")
          (is (some #(and (string? %)
                          (str/includes? % "1 (minor)")
                          (str/includes? % "12 (moderate)"))
                    values)
              "Should format anomalous lags with severities"))))

    (testing "does not display anomalous lags when empty"
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
                                                 :ratio 0.975}}}}}
            tapped (with-tap-out-n 2
                     (view/autocorrelation-classification*
                      :portal
                      {:classification-id :autocorrelation-classification
                       :autocorrelation-id :autocorrelation-raw}
                      data-map))
            table-data (some #(when (and (vector? %)
                                         (every? map? %)
                                         (every? :metric %))
                                %)
                             tapped)]
        (when table-data
          (let [metrics (map :metric table-data)]
            (is (not (some #(and (string? %) (str/includes? % "Anomalous lags")) metrics))
                "Should not include anomalous lags when empty")))))))
