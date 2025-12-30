(ns criterium.analyse-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.benchmark :as benchmark]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.test-utils :refer [approx=]]
   [criterium.util.helpers :as util])
  (:import
   [java.lang Math]))

(deftest outlier-significance-impl--test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier significance"
    (let [batch-size 67108864]
      (is (= 0.9960022873987793
             (analyse/outlier-significance*
              (/ 1.395522860870968 batch-size)
              (/ (* 0.0013859776344426547 0.0013859776344426547)
                 batch-size)
              batch-size))))))

(defn metrics-samples
  [data ^long batch-size]
  (let [n (count (first (vals data)))]
    {:type :criterium/metrics-samples
     :metric->values data
     :transform (if (= batch-size 1)
                  collect-plan/identity-transforms
                  (#'collect-plan/batch-transforms batch-size))
     :num-samples n
     :batch-size batch-size
     :eval-count (* n batch-size)
     :metrics-defs (select-keys
                    (metrics/metrics)
                    (mapv first (keys data)))
     :source-id nil
     :expr-value (ffirst (vals data))}))

(defn transformed-metric-values
  [data-map id p]
  (let [m (-> data-map id)
        transforms (util/get-transforms data-map id)]
    (mapv
     #(util/transform-sample-> % transforms)
     (get (:metric->values m) p))))

(defn transformed-values
  [data-map id vs]
  (let [transforms (util/get-transforms data-map id)]
    (prn :transforms transforms)
    (mapv
     #(util/transform-sample-> % transforms)
     vs)))

(deftest transform-log-test
  (testing "transform-log"
    (let [raw-data [(Math/exp 1) (Math/exp 2) (Math/exp 3)]
          samples (metrics-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          result ((analyse/transform-log) data-map)]
      (testing "puts the log transformed metrics into the result-path"
        (is (= [1.0 2.0 3.0]
               (-> result
                   :log-samples
                   :metric->values
                   (get [:elapsed-time])))))
      (testing "doesnot change original samples"
        (is (= samples (:samples result))))
      (testing "adds transfprms for the values"
        (is (approx=
             (mapv (fn [^double v] (/ v 10.0)) raw-data)
             (transformed-metric-values result :log-samples [:elapsed-time]))))
      (testing "doesn't transform event-metrics "
        (is (not (contains? (:log-samples result) [:compilation])))))))

(deftest quantiles-test
  (testing "quantiles"
    (let [raw-data [10 20 30]
          samples (metrics-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          result ((analyse/quantiles {:quantiles [0.025 0.975]})
                  data-map)]
      (testing "puts the quantiles into the result-path"
        (let [qs [0.25 0.5 0.75 0.025 0.975]
              vs (-> result :quantiles util/quantiles :elapsed-time)]
          (is (approx=
               [15 20 25 10.5 29.5]
               (mapv vs qs)))
          (is (approx=
               [1.5 2.0 2.5 1.05 2.95]
               (transformed-values result :quantiles (mapv vs qs))))))
      (testing "doesn't transform event-metrics "
        (is (every?
             #(not (contains? % :compilation))
             (->> result :quantiles)))
        (is (= [:elapsed-time]
               (->> result :quantiles util/quantiles keys)))))))

(deftest outliers-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outliers"
    (let [raw-data [9 10 9 10 9 10 10000]
          samples (metrics-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          quantiles (analyse/quantiles {:quantiles []})
          outliers (analyse/outliers)]
      (is (= {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 1}
             (-> data-map
                 quantiles
                 outliers
                 :outliers
                 util/outliers
                 :elapsed-time
                 :outlier-counts))))))

(deftest outlier-counts-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier counts"
    (let [data-map
          {:samples
           {:type :criterium/collected-metrics-samples
            :metric->values {[:elapsed-time] [1 1 1 1000]}
            :transform collect-plan/identity-transforms
            :batch-size 1
            :eval-count 4
            :metrics-defs (select-keys
                           (metrics/metrics)
                           [:elapsed-time])}}
          analyse (benchmark/->analyse
                   [[:quantiles {:quantiles [0.025 0.975]}]
                    :outliers])]
      (is (= {:low-severe 0, :low-mild 0, :high-mild 0, :high-severe 1}
             (-> (analyse data-map)
                 :outliers
                 util/outliers
                 :elapsed-time
                 :outlier-counts))))))

;; Tests adjusted boxplot outlier detection using medcouple.
;; Verifies that medcouple is computed and stored, and that the adjusted
;; thresholds handle skewed data appropriately.

(deftest adjusted-outliers-test
  (testing "adjusted outlier detection"
    (testing "computes and stores medcouple"
      (let [raw-data [1 2 3 4 5 6 7 8 9]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] [0 0 0]}
                     1)
            data-map {:samples samples}
            quantiles (analyse/quantiles {:quantiles []})
            outliers (analyse/outliers)
            result (-> data-map quantiles outliers)]
        (is (number? (-> result :outliers util/outliers :elapsed-time :medcouple))
            "medcouple should be present in outliers output")))

    (testing "returns positive medcouple for right-skewed data"
      (let [;; Right-skewed: most values low, few high
            raw-data [1 2 2 3 3 3 4 4 5 10 15 20]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] (repeat 12 0)}
                     1)
            data-map {:samples samples}
            quantiles (analyse/quantiles {:quantiles []})
            outliers (analyse/outliers)
            result (-> data-map quantiles outliers)
            mc (-> result :outliers util/outliers :elapsed-time :medcouple)]
        (is (pos? mc) "medcouple should be positive for right-skewed data")))

    (testing "returns negative medcouple for left-skewed data"
      (let [;; Left-skewed: most values high, few low
            raw-data [-20 -15 -10 -5 -4 -4 -3 -3 -3 -2 -2 -1]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] (repeat 12 0)}
                     1)
            data-map {:samples samples}
            quantiles (analyse/quantiles {:quantiles []})
            outliers (analyse/outliers)
            result (-> data-map quantiles outliers)
            mc (-> result :outliers util/outliers :elapsed-time :medcouple)]
        (is (neg? mc) "medcouple should be negative for left-skewed data")))

    (testing "returns near-zero medcouple for symmetric data"
      (let [raw-data [1 2 3 4 5 6 7 8 9]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] (repeat 9 0)}
                     1)
            data-map {:samples samples}
            quantiles (analyse/quantiles {:quantiles []})
            outliers (analyse/outliers)
            result (-> data-map quantiles outliers)
            ^double mc (-> result :outliers util/outliers :elapsed-time :medcouple)]
        (is (< (Math/abs mc) 0.1)
            "medcouple should be near zero for symmetric data")))

    (testing "uses asymmetric thresholds for skewed data"
      (let [;; Right-skewed data
            raw-data [1 2 2 3 3 3 4 4 5 10 15 20]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] (repeat 12 0)}
                     1)
            data-map {:samples samples}
            quantiles (analyse/quantiles {:quantiles []})
            outliers (analyse/outliers)
            result (-> data-map quantiles outliers)
            [low-severe low-mild high-mild high-severe]
            (-> result :outliers util/outliers :elapsed-time :thresholds)
            ^double q1 (-> result :quantiles util/quantiles :elapsed-time (get 0.25))
            ^double q3 (-> result :quantiles util/quantiles :elapsed-time (get 0.75))
            iqr (- q3 q1)
            ;; Standard thresholds would be symmetric
            std-low-mild (- q1 (* 1.5 iqr))
            std-high-mild (+ q3 (* 1.5 iqr))]
        ;; For right-skewed data, upper fence should be wider than standard
        (is (> high-mild std-high-mild)
            "upper fence should be wider for right-skewed data")
        ;; For right-skewed data, lower fence should be narrower than standard
        (is (> low-mild std-low-mild)
            "lower fence should be narrower for right-skewed data")))))

(deftest outlier-method-test
  ;; Tests the :outlier-method option for outlier detection.
  ;; Verifies that :standard uses symmetric thresholds and :adjusted uses
  ;; medcouple-adjusted thresholds.
  (testing ":outlier-method option"
    (testing "with :standard uses symmetric thresholds"
      (let [;; Right-skewed data
            raw-data [1 2 2 3 3 3 4 4 5 10 15 20]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] (repeat 12 0)}
                     1)
            data-map {:samples samples}
            quantiles (analyse/quantiles {:quantiles []})
            outliers-standard (analyse/outliers {:outlier-method :standard})
            result (-> data-map quantiles outliers-standard)
            outlier-data (-> result :outliers util/outliers :elapsed-time)
            [low-severe low-mild high-mild high-severe] (:thresholds outlier-data)
            ^double q1 (-> result :quantiles util/quantiles :elapsed-time (get 0.25))
            ^double q3 (-> result :quantiles util/quantiles :elapsed-time (get 0.75))
            iqr (- q3 q1)]
        ;; With :standard, thresholds should be symmetric
        (is (approx= (- q1 (* 1.5 iqr)) low-mild)
            "low-mild should be standard Q1 - 1.5*IQR")
        (is (approx= (+ q3 (* 1.5 iqr)) high-mild)
            "high-mild should be standard Q3 + 1.5*IQR")
        (is (approx= (- q1 (* 3.0 iqr)) low-severe)
            "low-severe should be standard Q1 - 3*IQR")
        (is (approx= (+ q3 (* 3.0 iqr)) high-severe)
            "high-severe should be standard Q3 + 3*IQR")
        (is (nil? (:medcouple outlier-data))
            "medcouple should be nil for :standard method")))

    (testing "with :adjusted uses asymmetric thresholds"
      (let [;; Right-skewed data
            raw-data [1 2 2 3 3 3 4 4 5 10 15 20]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] (repeat 12 0)}
                     1)
            data-map {:samples samples}
            quantiles (analyse/quantiles {:quantiles []})
            outliers-adjusted (analyse/outliers {:outlier-method :adjusted})
            result (-> data-map quantiles outliers-adjusted)
            outlier-data (-> result :outliers util/outliers :elapsed-time)
            [_low-severe low-mild high-mild _high-severe] (:thresholds outlier-data)
            ^double q1 (-> result :quantiles util/quantiles :elapsed-time (get 0.25))
            ^double q3 (-> result :quantiles util/quantiles :elapsed-time (get 0.75))
            iqr (- q3 q1)
            std-low-mild (- q1 (* 1.5 iqr))
            std-high-mild (+ q3 (* 1.5 iqr))]
        ;; With :adjusted on right-skewed data, upper fence should be wider
        (is (> high-mild std-high-mild)
            "upper fence should be wider than standard for right-skewed")
        (is (> low-mild std-low-mild)
            "lower fence should be narrower than standard for right-skewed")
        (is (number? (:medcouple outlier-data))
            "medcouple should be present for :adjusted method")))

    (testing "default method is :adjusted"
      (let [raw-data [1 2 2 3 3 3 4 4 5 10 15 20]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] (repeat 12 0)}
                     1)
            data-map {:samples samples}
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            outliers-default (analyse/outliers {})
            outliers-adjusted (analyse/outliers {:outlier-method :adjusted})
            result-default (-> with-quantiles outliers-default)
            result-adjusted (-> with-quantiles outliers-adjusted)]
        (is (= (-> result-default :outliers util/outliers :elapsed-time :thresholds)
               (-> result-adjusted :outliers util/outliers :elapsed-time :thresholds))
            "default should produce same thresholds as :adjusted")))

    (testing ":auto is equivalent to :adjusted for metrics-samples"
      (let [raw-data [1 2 2 3 3 3 4 4 5 10 15 20]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] (repeat 12 0)}
                     1)
            data-map {:samples samples}
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            outliers-auto (analyse/outliers {:outlier-method :auto})
            outliers-adjusted (analyse/outliers {:outlier-method :adjusted})
            result-auto (-> with-quantiles outliers-auto)
            result-adjusted (-> with-quantiles outliers-adjusted)]
        (is (= (-> result-auto :outliers util/outliers :elapsed-time :thresholds)
               (-> result-adjusted :outliers util/outliers :elapsed-time :thresholds))
            ":auto should produce same thresholds as :adjusted for metrics-samples")))))

(deftest stats-test
  (testing "stats"
    (let [raw-data [1 2 3]
          samples (metrics-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          result ((analyse/stats) data-map)]
      (testing "puts the stats into the result-path"
        (is (= {:min-val 1.0,
                :max-val 3.0,
                :mean 2.0,
                :mean-plus-3sigma 5.0,
                :variance 1.0,
                :mean-minus-3sigma -1.0
                :n 3}
               (->> result :stats util/stats :elapsed-time))))
      (testing "doesn't transform event-metrics "
        (is (every?
             #(not (contains? % :compilation))
             (->> result :stats)))
        (is (= [:elapsed-time]
               (->> result :stats util/stats keys))))))
  (testing "stats variance"
    (let [raw-data [1 1 1 5 5 5 9 9 9]
          samples (metrics-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   1)
          data-map {:samples samples}
          result ((analyse/stats) data-map)]
      (testing "calculates sample variance"
        (is (= 12.0 (:variance (->> result :stats util/stats :elapsed-time))))))
    (let [raw-data [1 1 1 5 5 5 9 9 9]
          samples (metrics-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   10)
          data-map {:samples samples}
          result ((analyse/stats) data-map)]
      (testing "scales with batch size"
        (let [v (:variance (->> result :stats util/stats :elapsed-time))]
          (is (= 12.0 v))
          (is (= 1.20 (util/transform-sample->
                       v
                       (util/get-transforms result :stats))))))))
  (testing "excludes outliers"
    (let [raw-data [9 10 9 10 9 10 10000]
          samples (metrics-samples
                   {[:elapsed-time] raw-data
                    [:compilation :time-ms] [0 0 0]}
                   1)
          data-map {:samples samples}
          quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
          outliers (analyse/outliers)
          stats (analyse/stats)
          result (-> data-map
                     quantiles
                     outliers
                     stats)
          smap (->> result :stats util/stats :elapsed-time)
          smap' (util/transform-vals->
                 (->> result :stats util/stats :elapsed-time)
                 (util/get-transforms result :stats))]
      (testing "calculates sample variance"
        (is (approx= 9.5 (:mean smap)))
        (is (approx= 0.3 (:variance smap)))
        (is (approx= 9 (:min-val smap)))
        (is (approx= 10 (:max-val smap)))
        (is (approx= 11.14316767 (:mean-plus-3sigma smap)))
        (is (approx= 7.8568323274845016 (:mean-minus-3sigma smap)))
        (is (= 6 (:n smap)))

        (is (approx= 9.5 (:mean smap')))
        (is (approx= 0.3 (:variance smap')))
        (is (approx= 9 (:min-val smap')))
        (is (approx= 10 (:max-val smap')))
        (is (approx= 11.14316767 (:mean-plus-3sigma smap')))
        (is (approx= 7.8568323274845016 (:mean-minus-3sigma smap')))))

    (testing "scales with batch size"
      (let [raw-data [9 10 9 10 9 10 10000]
            samples (metrics-samples
                     {[:elapsed-time] raw-data
                      [:compilation :time-ms] [0 0 0]}
                     2)
            quantiles (analyse/quantiles {:quantiles [0.9 0.99 0.99]})
            data-map {:samples samples}
            outliers (analyse/outliers)
            stats (analyse/stats)
            result (-> data-map quantiles outliers stats)
            smap (-> result :stats util/stats :elapsed-time)
            smap' (util/transform-vals->
                   (-> result :stats util/stats :elapsed-time)
                   (util/get-transforms result :stats))]
        (is (approx= 9.5 (:mean smap)))
        (is (approx= 0.3 (:variance smap)))
        (is (approx= 9 (:min-val smap)))
        (is (approx= 10 (:max-val smap)))
        (is (approx= 11.14316767 (:mean-plus-3sigma smap)))
        (is (approx= 7.8568323274845016 (:mean-minus-3sigma smap)))

        (is (approx= (/ 9.5 2.0) (:mean smap')))
        (is (approx= (/ 0.3 2.0) (:variance smap')))
        (is (approx= 4.5 (:min-val smap')))
        (is (approx= 5 (:max-val smap')))
        (is (approx= (/ 11.14316767 2) (:mean-plus-3sigma smap')))
        (is (approx= (/ 7.8568323274845016 2) (:mean-minus-3sigma smap')))))))

(deftest event-stats-test
  (testing "event-stats"
    (let [data-map
          {:samples
           {:type :criterium/metrics-samples
            :metrics-defs
            (-> (select-keys
                 (metrics/metrics)
                 [:class-loader :compilation
                  :elapsed-time :garbage-collector])
                (assoc-in
                 [:garbage-collector]
                 {:type :event
                  :groups
                  {:total
                   {:summary
                    (str "%32s: ran %s times"
                         " for a total of %s in %s samples")
                    :values
                    [{:path [:garbage-collector :total :count]
                      :scale 1
                      :dimension :count
                      :label "GC total count"
                      :type :event}
                     {:path [:garbage-collector :total :time-ms]
                      :scale 1e-3
                      :dimension :time
                      :label "GC total time"
                      :type :event}]
                    :label "Garbage Collector"}}}))
            :metric->values {[:elapsed-time] [1 2 3]
                             [:compilation :time-ms] [3 5 0]
                             [:garbage-collector :total :time-ms] [1 1 1]
                             [:garbage-collector :total :count] [2 1 1]
                             [:class-loader :loaded-count] [2 2 0]
                             [:class-loader :unloaded-count] [0 0 0]}
            :batch-size 1
            :eval-count 3}}
          result ((analyse/event-stats) data-map)]
      (testing "puts the event-stats into the output-path"
        (is (= {[:class-loader :loaded-count] 4,
                [:class-loader :unloaded-count] 0,
                [:class-loader :sample-count] 2,
                [:compilation :time-ms] 8,
                [:compilation :sample-count] 2,
                [:garbage-collector :total :count] 4,
                [:garbage-collector :total :time-ms] 3,
                [:garbage-collector :total :sample-count] 3}
               #_{:compilation {:time-ms 8 :sample-count 2}
                  :garbage-collector {:total
                                      {:time-ms 3 :count 4 :sample-count 3}}
                  :class-loader {:sample-count 2
                                 :loaded-count 4 :unloaded-count 0}}
               (->> result :event-stats util/event-stats)))))))

(deftest outlier-effect-test
  (is (= :unaffected (analyse/outlier-effect 0.009)))
  (is (= :slight (analyse/outlier-effect 0.09)))
  (is (= :moderate (analyse/outlier-effect 0.49)))
  (is (= :severe (analyse/outlier-effect 0.51))))

(deftest outlier-significance-test
  ;; http://www.ellipticgroup.com/misc/article_supplement.pdf, p22
  (testing "Outlier counts"
    (let [data-map
          {:samples
           {:type :criterium/collected-metrics-samples
            :metric->values {[:elapsed-time] [1 1 1 1000]}
            :transform collect-plan/identity-transforms
            :batch-size 1
            :eval-count 4
            :metrics-defs (select-keys
                           (metrics/metrics)
                           [:elapsed-time])}}
          analyse (benchmark/->analyse
                   [[:quantiles {:quantiles [0.025 0.975]}]
                    :outliers
                    :stats
                    :outlier-significance])]

      (is (= {:significance 0
              :effect :unaffected}
             (-> (analyse data-map)
                 :outlier-significance
                 util/outlier-significance
                 :elapsed-time))))))

(deftest kde-test
  ;; Tests the analyse/kde function for correct structure,
  ;; outlier filtering, and graceful handling of missing data.
  ;; Note: Mode detection is now a separate analysis step via analyse/modes.
  (testing "kde"
    (testing "returns correct structure"
      (let [;; Bimodal: clusters around 100 and 200
            raw-data (concat
                      (mapv #(+ 98.0 (* 4.0 (double %))) (range 30))
                      (mapv #(+ 198.0 (* 4.0 (double %))) (range 30)))
            samples (metrics-samples
                     {[:elapsed-time] (vec raw-data)}
                     1)
            data-map {:samples samples}
            ;; Need to add log-samples since that's the default source
            with-log ((analyse/transform-log {:id :log-samples
                                              :samples-id :samples})
                      data-map)
            result ((analyse/kde {:n-bootstrap 10 :n-points 64}) with-log)]
        (is (contains? result :kde) "result should have :kde key")
        (let [kde-data (:kde result)]
          (is (= :criterium/kde (:type kde-data)))
          (is (map? (:kdes kde-data)))
          (is (contains? (:kdes kde-data) [:elapsed-time]))
          (let [elapsed-kde (get-in kde-data [:kdes [:elapsed-time]])]
            (is (number? (:bandwidth elapsed-kde)))
            (is (vector? (:grid elapsed-kde)))
            (is (vector? (:density elapsed-kde)))
            ;; Modes are now computed separately via analyse/modes
            (is (nil? (:modes elapsed-kde))
                "modes should not be in KDE output (now separate)")))))

    (testing "uses custom samples-id"
      (let [raw-data (mapv #(+ 10.0 (* 0.5 (double %))) (range 50))
            samples (metrics-samples
                     {[:elapsed-time] raw-data}
                     1)
            data-map {:my-samples samples}
            result ((analyse/kde {:samples-id :my-samples
                                  :n-bootstrap 10
                                  :n-points 32})
                    data-map)]
        (is (contains? result :kde))))

    (testing "returns data-map unchanged when samples unavailable"
      (let [data-map {:other-data 123}
            result ((analyse/kde) data-map)]
        (is (= data-map result))
        (is (not (contains? result :kde)))))

    (testing "uses custom output id"
      (let [raw-data (mapv #(+ 10.0 (* 0.5 (double %))) (range 50))
            samples (metrics-samples
                     {[:elapsed-time] raw-data}
                     1)
            data-map {:log-samples samples}
            result ((analyse/kde {:id :my-kde
                                  :n-bootstrap 10
                                  :n-points 32})
                    data-map)]
        (is (contains? result :my-kde))
        (is (not (contains? result :kde)))))

    (testing "excludes outliers from KDE computation"
      (let [;; Normal samples around 100, with one extreme outlier at end
            raw-data (conj (vec (mapv #(+ 100.0 (* 0.5 (double %))) (range 49)))
                           10000.0)
            samples (metrics-samples
                     {[:elapsed-time] raw-data}
                     1)
            data-map {:samples samples}
            ;; Use the analysis functions to generate proper outliers
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            with-outliers ((analyse/outliers) with-quantiles)
            ;; KDE uses log-samples by default, but we test with raw samples
            result ((analyse/kde {:samples-id :samples
                                  :n-bootstrap 10
                                  :n-points 32})
                    with-outliers)
            kde-data (:kde result)
            elapsed-kde (get-in kde-data [:kdes [:elapsed-time]])
            grid (:grid elapsed-kde)
            grid-max (apply max grid)]
        ;; If outlier was included, grid would extend to ~10000
        ;; With outlier excluded, grid max should be near 124 (100 + 0.5*48)
        (is (< grid-max 200) "grid should not extend to outlier value")
        (is (= :outliers (:outliers-id kde-data))
            "should record outliers-id in output")))))

(deftest modes-test
  ;; Tests analyse/modes function for multimodality testing.
  ;; Verifies ACR (default) and Silverman methods work correctly
  ;; and produce expected output structure.
  (testing "modes"
    (testing "uses ACR test by default"
      (let [raw-data (mapv #(+ 100.0 (* 0.5 (double %))) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            with-log ((analyse/transform-log {:id :log-samples
                                              :samples-id :samples})
                      data-map)
            with-kde ((analyse/kde {:n-bootstrap 10 :n-points 64}) with-log)
            result ((analyse/modes {:n-bootstrap 10}) with-kde)]
        (is (contains? result :modes))
        (let [modes-data (:modes result)
              elapsed-modes (get-in modes-data [:modes [:elapsed-time]])]
          (is (= :acr (get-in elapsed-modes [:test-results :method]))
              "should use ACR method by default")
          (is (contains? (:test-results elapsed-modes) :excess-mass)
              "ACR results should include excess-mass"))))

    (testing "supports Silverman method via :method option"
      (let [raw-data (mapv #(+ 100.0 (* 0.5 (double %))) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            with-log ((analyse/transform-log {:id :log-samples
                                              :samples-id :samples})
                      data-map)
            with-kde ((analyse/kde {:n-bootstrap 10 :n-points 64}) with-log)
            result ((analyse/modes {:n-bootstrap 10 :method :silverman}) with-kde)]
        (is (contains? result :modes))
        (let [modes-data (:modes result)
              elapsed-modes (get-in modes-data [:modes [:elapsed-time]])]
          (is (= :silverman (get-in elapsed-modes [:test-results :method]))
              "should use Silverman method when specified")
          (is (nil? (get-in elapsed-modes [:test-results :excess-mass]))
              "Silverman results should not include excess-mass"))))

    (testing "returns correct output structure"
      (let [raw-data (mapv #(+ 100.0 (* 0.5 (double %))) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            with-log ((analyse/transform-log {:id :log-samples
                                              :samples-id :samples})
                      data-map)
            with-kde ((analyse/kde {:n-bootstrap 10 :n-points 64}) with-log)
            result ((analyse/modes {:n-bootstrap 10}) with-kde)
            modes-data (:modes result)
            elapsed-modes (get-in modes-data [:modes [:elapsed-time]])]
        (is (= :criterium/modes (:type modes-data)))
        (is (vector? (:modes elapsed-modes)))
        (is (number? (:n-modes elapsed-modes)))
        (is (map? (:test-results elapsed-modes)))
        (is (contains? (:test-results elapsed-modes) :k-tested))
        (is (contains? (:test-results elapsed-modes) :p-values))
        (is (contains? (:test-results elapsed-modes) :critical-bandwidths))))

    (testing "with :mode-method :critical"
      (testing "includes antimodes and mode-bandwidth"
        (let [raw-data (mapv #(+ 100.0 (* 0.5 (double %))) (range 50))
              samples (metrics-samples {[:elapsed-time] raw-data} 1)
              data-map {:samples samples}
              with-log ((analyse/transform-log {:id :log-samples
                                                :samples-id :samples})
                        data-map)
              with-kde ((analyse/kde {:n-bootstrap 10 :n-points 64}) with-log)
              result ((analyse/modes {:n-bootstrap 10 :mode-method :critical})
                      with-kde)
              modes-data (:modes result)
              elapsed-modes (get-in modes-data [:modes [:elapsed-time]])]
          (is (= :critical (:mode-method elapsed-modes))
              "should record mode-method in output")
          (is (number? (:mode-bandwidth elapsed-modes))
              "should include mode-bandwidth")
          (is (vector? (:antimodes elapsed-modes))
              "should include antimodes vector")))

      (testing "for multimodal data produces antimodes"
        (let [;; Create well-separated bimodal data (log-transform will be applied)
              ;; Use exponential values so log-transform creates clearly separated modes
              raw-data (concat
                        (mapv #(* (Math/exp 1.0) (+ 1.0 (* 0.01 (double %)))) (range 30))
                        (mapv #(* (Math/exp 5.0) (+ 1.0 (* 0.01 (double %)))) (range 30)))
              samples (metrics-samples {[:elapsed-time] (vec raw-data)} 1)
              data-map {:samples samples}
              with-log ((analyse/transform-log {:id :log-samples
                                                :samples-id :samples})
                        data-map)
              with-kde ((analyse/kde {:n-bootstrap 10 :n-points 64}) with-log)
              result ((analyse/modes {:n-bootstrap 10 :mode-method :critical})
                      with-kde)
              elapsed-modes (get-in result [:modes :modes [:elapsed-time]])]
          ;; Antimodes vector should exist for :critical method
          (is (vector? (:antimodes elapsed-modes))
              "antimodes should be a vector")
          ;; When we have multiple modes, there should be antimodes between them
          (when (> (long (:n-modes elapsed-modes)) 1)
            (is (pos? (count (:antimodes elapsed-modes)))
                "multiple modes should have at least one antimode"))))

      (testing "with :isj (default) does not include antimodes"
        (let [raw-data (mapv #(+ 100.0 (* 0.5 (double %))) (range 50))
              samples (metrics-samples {[:elapsed-time] raw-data} 1)
              data-map {:samples samples}
              with-log ((analyse/transform-log {:id :log-samples
                                                :samples-id :samples})
                        data-map)
              with-kde ((analyse/kde {:n-bootstrap 10 :n-points 64}) with-log)
              result ((analyse/modes {:n-bootstrap 10 :mode-method :isj})
                      with-kde)
              elapsed-modes (get-in result [:modes :modes [:elapsed-time]])]
          (is (nil? (:mode-method elapsed-modes))
              "should not include mode-method for :isj")
          (is (nil? (:antimodes elapsed-modes))
              "should not include antimodes for :isj"))))))
