(ns criterium.analyse-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]
   [criterium.analyse.methods]
   [criterium.array :as arr]
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

(defn- path->typed-array
  "Convert a vector to the appropriate typed array for a metric path."
  [path values]
  (let [metric-key (first path)]
    ;; Quantitative metrics: elapsed-time
    ;; Event metrics: compilation, garbage-collector, class-loader
    (if (= :elapsed-time metric-key)
      (arr/->double-array (double-array values))
      ;; All other metrics (event types) use long arrays
      (arr/->long-array (long-array values)))))

(defn metrics-samples
  [data ^long batch-size]
  (let [n (count (first (vals data)))
        typed-data (into {}
                         (map (fn [[path values]]
                                [path (path->typed-array path values)]))
                         data)]
    {:type :criterium/metrics-samples
     :metric->values typed-data
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
        transforms (util/get-transforms data-map id)
        values (arr/fold (get (:metric->values m) p) conj [])]
    (mapv
     #(util/transform-sample-> % transforms)
     values)))

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
        (is (arr/array=
             (-> result
                 :log-samples
                 :metric->values
                 (get [:elapsed-time]))
             [1.0 2.0 3.0])))
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
            :metric->values {[:elapsed-time] (arr/->double-array (double-array [1 1 1 1000]))}
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
            [_low-severe low-mild high-mild _high-severe]
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
                :median 2.0,
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
            :metric->values {[:elapsed-time] (arr/->double-array (double-array [1 2 3]))
                             [:compilation :time-ms] (arr/->long-array (long-array [3 5 0]))
                             [:garbage-collector :total :time-ms] (arr/->long-array (long-array [1 1 1]))
                             [:garbage-collector :total :count] (arr/->long-array (long-array [2 1 1]))
                             [:class-loader :loaded-count] (arr/->long-array (long-array [2 2 0]))
                             [:class-loader :unloaded-count] (arr/->long-array (long-array [0 0 0]))}
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
            :metric->values {[:elapsed-time] (arr/->double-array (double-array [1 1 1 1000]))}
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

(deftest histogram-test
  (testing "histogram"
    (testing "with default (Freedman-Diaconis) method"
      (let [raw-data (mapv #(+ 100.0 (* 1.0 (double %))) (range 100))
            samples (metrics-samples
                     {[:elapsed-time] raw-data}
                     1)
            data-map {:samples samples}
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            with-outliers ((analyse/outliers) with-quantiles)
            result ((analyse/histogram) with-outliers)]
        (is (contains? result :histograms))
        (let [hist-data (:histograms result)
              elapsed-hist (get-in hist-data [:histograms [:elapsed-time]])]
          (is (= :criterium/histogram-fixed-width (:type elapsed-hist)))
          (is (vector? (:counts elapsed-hist)))
          (is (vector? (:centers elapsed-hist)))
          (is (number? (:width elapsed-hist)))
          (is (not (contains? elapsed-hist :optimal-bins))
              "Freedman-Diaconis should not include optimal-bins"))))

    (testing "with :method :knuth"
      (let [raw-data (mapv #(+ 100.0 (* 1.0 (double %))) (range 100))
            samples (metrics-samples
                     {[:elapsed-time] raw-data}
                     1)
            data-map {:samples samples}
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            with-outliers ((analyse/outliers) with-quantiles)
            result ((analyse/histogram {:method :knuth}) with-outliers)]
        (is (contains? result :histograms))
        (let [hist-data (:histograms result)
              elapsed-hist (get-in hist-data [:histograms [:elapsed-time]])]
          (is (= :criterium/histogram-knuth (:type elapsed-hist)))
          (is (vector? (:counts elapsed-hist)))
          (is (vector? (:centers elapsed-hist)))
          (is (number? (:width elapsed-hist)))
          (is (pos-int? (:optimal-bins elapsed-hist))
              "Knuth method should include optimal-bins")
          (is (number? (:log-posterior elapsed-hist))
              "Knuth method should include log-posterior"))))

    (testing "with :method :knuth and :max-bins"
      (let [raw-data (mapv #(+ 100.0 (* 1.0 (double %))) (range 100))
            samples (metrics-samples
                     {[:elapsed-time] raw-data}
                     1)
            data-map {:samples samples}
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            with-outliers ((analyse/outliers) with-quantiles)
            result ((analyse/histogram {:method :knuth :max-bins 10}) with-outliers)
            hist-data (:histograms result)
            elapsed-hist (get-in hist-data [:histograms [:elapsed-time]])]
        (is (<= (long (:optimal-bins elapsed-hist)) 10)
            "optimal-bins should respect max-bins limit")))))

;;; Tests for KDE-based stats computation
;; Validates that defmethod methods/stats :criterium/kde produces correct
;; statistics derived from density integration.

(deftest kde-stats-test
  (testing "methods/stats :criterium/kde"
    (testing "returns stats with correct structure"
      (let [;; Create a simple KDE data structure
            kde-data {:type :criterium/kde
                      :kdes {[:elapsed-time]
                             {:grid [1.0 2.0 3.0 4.0 5.0]
                              :density [0.1 0.2 0.4 0.2 0.1]
                              :bandwidth 0.5
                              :n 100}}
                      :transform {:sample-> identity :->sample identity}}
            metric-configs [{:path [:elapsed-time]}]
            result (criterium.analyse.methods/stats kde-data nil metric-configs {})]
        (is (= :criterium/stats (:type result))
            "result type should be :criterium/stats")
        (is (map? (:stats result))
            "result should contain :stats map")
        (is (contains? (:stats result) :elapsed-time)
            "stats should contain elapsed-time metric")
        (let [s (get-in result [:stats :elapsed-time])]
          (is (number? (:mean s)) "should have :mean")
          (is (number? (:variance s)) "should have :variance")
          (is (number? (:min-val s)) "should have :min-val")
          (is (number? (:max-val s)) "should have :max-val")
          (is (number? (:mean-plus-3sigma s)) "should have :mean-plus-3sigma")
          (is (number? (:mean-minus-3sigma s)) "should have :mean-minus-3sigma")
          (is (= 100 (:n s)) "should have :n from KDE metadata"))))

    (testing "computes correct mean for symmetric density"
      ;; Symmetric density centered at 3.0 should have mean ≈ 3.0
      ;; Using a uniform density simplifies verification
      (let [grid [1.0 2.0 3.0 4.0 5.0]
            ;; Uniform density: f(x) = 0.25 for x in [1,5]
            ;; ∫f(x)dx from 1 to 5 = 0.25 * 4 = 1.0 (normalized)
            ;; Mean = ∫x*f(x)dx = 0.25 * ∫x dx from 1 to 5
            ;;      = 0.25 * [x²/2] from 1 to 5 = 0.25 * (12.5 - 0.5) = 3.0
            density [0.25 0.25 0.25 0.25 0.25]
            kde-data {:type :criterium/kde
                      :kdes {[:elapsed-time]
                             {:grid grid
                              :density density
                              :bandwidth 0.5
                              :n 50}}
                      :transform {:sample-> identity :->sample identity}}
            metric-configs [{:path [:elapsed-time]}]
            result (criterium.analyse.methods/stats kde-data nil metric-configs {})
            mean (get-in result [:stats :elapsed-time :mean])]
        (is (approx= 3.0 mean 0.01)
            (str "mean of uniform density should be at center, got: " mean))))

    (testing "computes correct min/max from grid bounds"
      (let [grid [10.0 20.0 30.0 40.0 50.0]
            kde-data {:type :criterium/kde
                      :kdes {[:elapsed-time]
                             {:grid grid
                              :density [0.2 0.2 0.2 0.2 0.2]
                              :bandwidth 1.0
                              :n 25}}
                      :transform {:sample-> identity :->sample identity}}
            metric-configs [{:path [:elapsed-time]}]
            result (criterium.analyse.methods/stats kde-data nil metric-configs {})
            stats (get-in result [:stats :elapsed-time])]
        (is (= 10.0 (:min-val stats))
            "min-val should be first grid point")
        (is (= 50.0 (:max-val stats))
            "max-val should be last grid point")))

    (testing "mean-plus/minus-3sigma derived from variance"
      (let [kde-data {:type :criterium/kde
                      :kdes {[:elapsed-time]
                             {:grid [0.0 1.0 2.0 3.0 4.0]
                              :density [0.1 0.2 0.4 0.2 0.1]
                              :bandwidth 0.3
                              :n 30}}
                      :transform {:sample-> identity :->sample identity}}
            metric-configs [{:path [:elapsed-time]}]
            result (criterium.analyse.methods/stats kde-data nil metric-configs {})
            stats (get-in result [:stats :elapsed-time])
            ^double mean (:mean stats)
            ^double variance (:variance stats)
            expected-3sigma (* 3.0 (Math/sqrt variance))]
        (is (approx= (+ mean expected-3sigma) (:mean-plus-3sigma stats))
            "mean-plus-3sigma should be mean + 3*stddev")
        (is (approx= (- mean expected-3sigma) (:mean-minus-3sigma stats))
            "mean-minus-3sigma should be mean - 3*stddev")))

    (testing "preserves transform from kde-map"
      (let [custom-transform {:sample-> #(* ^double % 2.0) :->sample #(/ ^double % 2.0)}
            kde-data {:type :criterium/kde
                      :kdes {[:elapsed-time]
                             {:grid [1.0 2.0 3.0]
                              :density [0.25 0.5 0.25]
                              :bandwidth 0.2
                              :n 10}}
                      :transform custom-transform}
            metric-configs [{:path [:elapsed-time]}]
            result (criterium.analyse.methods/stats kde-data nil metric-configs {})]
        (is (= custom-transform (:transform result))
            "transform should be preserved from kde-map")))

    (testing "handles multiple metrics"
      (let [kde-data {:type :criterium/kde
                      :kdes {[:elapsed-time]
                             {:grid [1.0 2.0 3.0]
                              :density [0.25 0.5 0.25]
                              :bandwidth 0.3
                              :n 100}
                             [:memory :used]
                             {:grid [100.0 200.0 300.0]
                              :density [0.25 0.5 0.25]
                              :bandwidth 10.0
                              :n 100}}
                      :transform {:sample-> identity :->sample identity}}
            metric-configs [{:path [:elapsed-time]}
                            {:path [:memory :used]}]
            result (criterium.analyse.methods/stats kde-data nil metric-configs {})]
        (is (contains? (:stats result) :elapsed-time)
            "should have elapsed-time stats")
        (is (contains? (get-in result [:stats :memory]) :used)
            "should have memory/used stats")))

    (testing "skips metrics not in kde-map"
      (let [kde-data {:type :criterium/kde
                      :kdes {[:elapsed-time]
                             {:grid [1.0 2.0 3.0]
                              :density [0.25 0.5 0.25]
                              :bandwidth 0.3
                              :n 100}}
                      :transform {:sample-> identity :->sample identity}}
            ;; Request stats for a metric that doesn't exist in kdes
            metric-configs [{:path [:elapsed-time]}
                            {:path [:nonexistent :metric]}]
            result (criterium.analyse.methods/stats kde-data nil metric-configs {})]
        (is (contains? (:stats result) :elapsed-time)
            "should have elapsed-time stats")
        (is (not (contains? (:stats result) :nonexistent))
            "should not have nonexistent metric")))))

;;; Tests for kde-stats analysis function
;; Validates the full analysis pipeline from samples through KDE to stats.

(deftest kde-stats-analysis-test
  ;; Tests the analyse/kde-stats function which provides a high-level
  ;; interface for computing stats from KDE density estimates.
  (testing "kde-stats"
    (testing "computes stats from KDE data"
      (let [raw-data (mapv #(+ 100.0 (* 0.5 (double %))) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            with-log ((analyse/transform-log {:id :log-samples
                                              :samples-id :samples})
                      data-map)
            with-kde ((analyse/kde {:n-bootstrap 10 :n-points 64}) with-log)
            result ((analyse/kde-stats) with-kde)]
        (is (contains? result :kde-stats) "result should have :kde-stats key")
        (let [stats-data (:kde-stats result)]
          (is (= :criterium/stats (:type stats-data)))
          (is (= :kde (:source-id stats-data)))
          (let [s (-> stats-data util/stats :elapsed-time)]
            (is (number? (:mean s)) "should have :mean")
            (is (number? (:variance s)) "should have :variance")
            (is (number? (:min-val s)) "should have :min-val")
            (is (number? (:max-val s)) "should have :max-val")))))

    (testing "returns data-map unchanged when KDE unavailable"
      (let [data-map {:other-data 123}
            result ((analyse/kde-stats) data-map)]
        (is (= data-map result))
        (is (not (contains? result :kde-stats)))))

    (testing "uses custom kde-id"
      (let [raw-data (mapv #(+ 100.0 (* 0.5 (double %))) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            with-log ((analyse/transform-log {:id :log-samples
                                              :samples-id :samples})
                      data-map)
            with-kde ((analyse/kde {:id :my-kde
                                    :n-bootstrap 10
                                    :n-points 32})
                      with-log)
            result ((analyse/kde-stats {:kde-id :my-kde}) with-kde)]
        (is (contains? result :kde-stats))))

    (testing "uses custom output id"
      (let [raw-data (mapv #(+ 100.0 (* 0.5 (double %))) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            with-log ((analyse/transform-log {:id :log-samples
                                              :samples-id :samples})
                      data-map)
            with-kde ((analyse/kde {:n-bootstrap 10 :n-points 32}) with-log)
            result ((analyse/kde-stats {:id :my-kde-stats}) with-kde)]
        (is (contains? result :my-kde-stats))
        (is (not (contains? result :kde-stats)))))))

;;; Distribution Fitting Tests
;; Tests the analyse/distribution-fit function for fitting parametric
;; distributions using MLE with model selection via AIC/BIC.

(deftest distribution-fit-test
  ;; Tests distribution fitting analysis including MLE parameter estimation,
  ;; model selection, goodness-of-fit tests, and bootstrap CIs.
  (testing "distribution-fit"
    (testing "returns correct structure"
      (let [;; Generate gamma-distributed samples (shape=2, scale=50)
            ;; Using inverse CDF method would be complex, so use samples
            ;; that roughly follow a right-skewed distribution
            raw-data (mapv #(+ 50.0 (* 20.0 (Math/pow (/ (double %) 100.0) 0.5)))
                           (range 1 101))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit {:n-bootstrap 20}) data-map)]
        (is (contains? result :distribution-fit)
            "result should have :distribution-fit key")
        (let [fit-data (:distribution-fit result)]
          (is (= :criterium/distribution-fit (:type fit-data)))
          (is (= :samples (:source-id fit-data)))
          (is (map? (:fits fit-data)))
          (is (contains? (:fits fit-data) [:elapsed-time]))
          (let [elapsed-fit (get-in fit-data [:fits [:elapsed-time]])]
            (is (= 100 (:n elapsed-fit))
                "should record sample size")
            (is (nil? (:warning elapsed-fit))
                "no warning for n >= 30")
            (is (keyword? (:best-model elapsed-fit))
                "should identify best model")
            (is (map? (:distributions elapsed-fit))
                "should have distributions map")
            (is (map? (:parameter-cis elapsed-fit))
                "should have parameter CIs for best model")))))

    (testing "warns for small sample size"
      (let [raw-data (mapv #(+ 50.0 (* 10.0 (double %))) (range 1 21))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit {:n-bootstrap 10}) data-map)]
        (is (= :small-sample
               (get-in result [:distribution-fit :fits [:elapsed-time] :warning]))
            "should warn when n < 30")))

    (testing "fits specified distributions only"
      (let [raw-data (mapv #(+ 50.0 (* 10.0 (double %))) (range 1 51))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit
                     {:distributions [:gamma :lognormal]
                      :n-bootstrap 10})
                    data-map)
            fit-data (get-in result [:distribution-fit :fits [:elapsed-time]])]
        (is (= #{:gamma :lognormal}
               (set (keys (:distributions fit-data))))
            "should only fit requested distributions")))

    (testing "computes AIC/BIC for fitted distributions"
      (let [raw-data (mapv #(+ 50.0 (* 10.0 (double %))) (range 1 51))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit {:n-bootstrap 10}) data-map)
            fit-data (get-in result [:distribution-fit :fits [:elapsed-time]])
            gamma-fit (get-in fit-data [:distributions :gamma])]
        (when-not (:error gamma-fit)
          (is (number? (:aic gamma-fit)) "should have AIC")
          (is (number? (:bic gamma-fit)) "should have BIC")
          (is (number? (:aicc gamma-fit)) "should have AICc")
          (is (number? (:delta-aic gamma-fit)) "should have delta-AIC"))))

    (testing "computes goodness-of-fit tests"
      (let [raw-data (mapv #(+ 50.0 (* 10.0 (double %))) (range 1 51))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit {:n-bootstrap 10}) data-map)
            fit-data (get-in result [:distribution-fit :fits [:elapsed-time]])
            gamma-fit (get-in fit-data [:distributions :gamma])]
        (when-not (:error gamma-fit)
          (is (map? (:ks-test gamma-fit)) "should have K-S test")
          (is (number? (get-in gamma-fit [:ks-test :statistic])))
          (is (number? (get-in gamma-fit [:ks-test :p-value])))
          (is (map? (:cvm-test gamma-fit)) "should have CvM test")
          (is (number? (get-in gamma-fit [:cvm-test :statistic])))
          (is (number? (get-in gamma-fit [:cvm-test :p-value]))))))

    (testing "best model has delta-aic of 0"
      (let [raw-data (mapv #(+ 50.0 (* 10.0 (double %))) (range 1 51))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit {:n-bootstrap 10}) data-map)
            fit-data (get-in result [:distribution-fit :fits [:elapsed-time]])
            best-model (:best-model fit-data)
            best-fit (get-in fit-data [:distributions best-model])]
        (when best-model
          (is (approx= 0.0 (:delta-aic best-fit))
              "best model should have delta-AIC of 0"))))

    (testing "parameter CIs computed for best model only"
      (let [raw-data (mapv #(+ 50.0 (* 10.0 (double %))) (range 1 51))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit {:n-bootstrap 20}) data-map)
            fit-data (get-in result [:distribution-fit :fits [:elapsed-time]])
            best-model (:best-model fit-data)
            cis (:parameter-cis fit-data)]
        (when best-model
          (is (= 1 (count cis))
              "should only have CIs for best model")
          (is (contains? cis best-model)
              "CIs should be keyed by best model")
          (let [best-cis (get cis best-model)]
            (is (map? best-cis) "CIs should be a map")
            (doseq [[_param ci] best-cis]
              (is (number? (:point-estimate ci))
                  "CI should have point estimate")
              (is (number? (:ci-lower ci))
                  "CI should have lower bound")
              (is (number? (:ci-upper ci))
                  "CI should have upper bound"))))))

    (testing "uses moment-match prefilter"
      ;; Using data that should fail prefilter for some distributions
      ;; (e.g., data with negative values would fail all, but we use
      ;; positive data that might fail weibull CV constraint)
      (let [raw-data (mapv #(+ 1.0 (* 0.01 (double %))) (range 1 51))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit {:n-bootstrap 10}) data-map)
            fit-data (get-in result [:distribution-fit :fits [:elapsed-time]])
            distributions (:distributions fit-data)]
        ;; At least some distributions should be fitted
        (is (some #(and (not (:skipped (second %)))
                        (not (:error (second %))))
                  distributions)
            "at least some distributions should be fitted successfully")))

    (testing "filters outliers when outliers-id provided"
      (let [;; Normal samples around 100, with extreme outlier
            raw-data (conj (vec (mapv #(+ 100.0 (* 0.5 (double %))) (range 49)))
                           10000.0)
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            with-outliers ((analyse/outliers) with-quantiles)
            ;; Fit without outlier filtering
            result-no-filter ((analyse/distribution-fit
                               {:outliers-id nil :n-bootstrap 10})
                              with-outliers)
            ;; Fit with outlier filtering
            result-with-filter ((analyse/distribution-fit
                                 {:n-bootstrap 10})
                                with-outliers)
            n-no-filter (get-in result-no-filter
                                [:distribution-fit :fits [:elapsed-time] :n])
            n-with-filter (get-in result-with-filter
                                  [:distribution-fit :fits [:elapsed-time] :n])]
        (is (= 50 n-no-filter)
            "without filtering should use all samples")
        (is (< n-with-filter 50)
            "with filtering should exclude outliers")))

    (testing "returns data-map unchanged when samples unavailable"
      (let [data-map {:other-data 123}
            result ((analyse/distribution-fit) data-map)]
        (is (= data-map result))
        (is (not (contains? result :distribution-fit)))))

    (testing "uses custom output id"
      (let [raw-data (mapv #(+ 50.0 (* 10.0 (double %))) (range 1 51))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/distribution-fit {:id :my-fit :n-bootstrap 10})
                    data-map)]
        (is (contains? result :my-fit))
        (is (not (contains? result :distribution-fit)))))

    (testing "uses custom samples-id"
      (let [raw-data (mapv #(+ 50.0 (* 10.0 (double %))) (range 1 51))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:my-samples samples}
            result ((analyse/distribution-fit {:samples-id :my-samples
                                               :n-bootstrap 10})
                    data-map)]
        (is (contains? result :distribution-fit))))))

;;; Tail Analysis Tests
;; Tests the analyse/tail-analysis function for extreme value analysis including
;; Hill estimator, GPD fitting, mean residual life, and tail ratios.

(deftest tail-analysis-test
  ;; Tests tail analysis function which computes statistics for understanding
  ;; distribution tails - useful for latency analysis where p99/p999 matter.
  ;; Key distinction: tail analysis uses raw samples WITHOUT outlier filtering
  ;; because the "outliers" ARE the tail we want to analyze.
  (testing "tail-analysis"
    (testing "returns correct structure"
      (let [;; Create data with a heavy tail (some large values)
            ;; Need enough samples so 10% (threshold) gives >10 exceedances
            raw-data (concat
                      (mapv #(+ 10.0 (* 2.0 (double %))) (range 180))
                      ;; Add some tail values (20 values to ensure >10 exceedances)
                      [400.0 450.0 500.0 550.0 600.0 650.0 700.0 750.0
                       800.0 850.0 900.0 950.0 1000.0 1100.0 1200.0
                       1300.0 1400.0 1500.0 1600.0 1800.0])
            samples (metrics-samples {[:elapsed-time] (vec raw-data)} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis) data-map)]
        (is (contains? result :tail-analysis)
            "result should have :tail-analysis key")
        (let [tail-data (:tail-analysis result)]
          (is (= :criterium/tail-analysis (:type tail-data)))
          (is (= :samples (:source-id tail-data)))
          (is (map? (:tail-analysis tail-data)))
          (is (contains? (:tail-analysis tail-data) [:elapsed-time]))
          (let [elapsed-tail (get-in tail-data [:tail-analysis [:elapsed-time]])]
            (is (= 200 (:n elapsed-tail)) "should record sample size")
            (is (number? (:threshold elapsed-tail)) "should have threshold")
            (is (= 0.9 (:threshold-quantile elapsed-tail))
                "default threshold quantile should be 0.9")
            (is (map? (:tail-ratios elapsed-tail)) "should have tail-ratios")
            (is (map? (:hill elapsed-tail)) "should have Hill estimator results")
            (is (map? (:gpd elapsed-tail)) "should have GPD fit")
            (is (map? (:mrl elapsed-tail)) "should have MRL results")
            (is (map? (:high-quantiles elapsed-tail))
                "should have high quantile estimates")
            (is (map? (:empirical-quantiles elapsed-tail))
                "should have empirical quantiles")))))

    (testing "tail-ratios structure"
      (let [;; Need enough samples for meaningful percentile ratios
            raw-data (concat
                      (mapv #(+ 10.0 (* 1.0 (double %))) (range 180))
                      ;; Heavy tail values
                      [200.0 250.0 300.0 350.0 400.0 450.0 500.0 600.0
                       700.0 800.0 900.0 1000.0 1100.0 1200.0 1300.0
                       1400.0 1500.0 1600.0 1800.0 2000.0])
            samples (metrics-samples {[:elapsed-time] (vec raw-data)} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis) data-map)
            tail-ratios (get-in result [:tail-analysis :tail-analysis
                                        [:elapsed-time] :tail-ratios])]
        (is (number? (:p99-p95 tail-ratios)) "should have p99/p95 ratio")
        (is (number? (:p999-p99 tail-ratios)) "should have p999/p99 ratio")
        (is (number? (:p999-p95 tail-ratios)) "should have p999/p95 ratio")
        ;; For heavy-tailed data, ratios should be > 1
        (is (> (double (:p99-p95 tail-ratios)) 1.0)
            "p99/p95 ratio should be > 1 for heavy-tailed data")))

    (testing "Hill estimator structure"
      (let [;; Need enough samples for meaningful Hill estimator
            raw-data (concat
                      (mapv #(+ 10.0 (* 1.0 (double %))) (range 180))
                      [200.0 300.0 400.0 500.0 600.0 700.0 800.0 900.0
                       1000.0 1100.0 1200.0 1300.0 1400.0 1500.0 1600.0
                       1700.0 1800.0 1900.0 2000.0 2200.0])
            samples (metrics-samples {[:elapsed-time] (vec raw-data)} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis) data-map)
            hill (get-in result [:tail-analysis :tail-analysis
                                 [:elapsed-time] :hill])]
        (is (vector? (:k-range hill)) "should have k-range vector")
        (is (vector? (:estimates hill)) "should have estimates vector")
        (is (vector? (:tail-indices hill)) "should have tail-indices vector")
        (is (number? (:stable-estimate hill)) "should have stable-estimate")
        ;; k-range and estimates should have same length
        (is (= (count (:k-range hill)) (count (:estimates hill)))
            "k-range and estimates should match in length")))

    (testing "GPD fit structure"
      (let [;; Need 200 samples with 90th percentile threshold to get 20 exceedances
            raw-data (concat
                      (mapv #(+ 10.0 (* 1.0 (double %))) (range 180))
                      ;; 20 tail values for GPD fitting (need >10 exceedances)
                      [200.0 250.0 300.0 350.0 400.0 450.0 500.0 550.0
                       600.0 650.0 700.0 750.0 800.0 900.0 1000.0
                       1100.0 1200.0 1300.0 1400.0 1500.0])
            samples (metrics-samples {[:elapsed-time] (vec raw-data)} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis) data-map)
            gpd (get-in result [:tail-analysis :tail-analysis
                                [:elapsed-time] :gpd])]
        (is (number? (:threshold gpd)) "should have threshold")
        (is (number? (:xi gpd)) "should have xi (shape parameter)")
        (is (number? (:sigma gpd)) "should have sigma (scale parameter)")
        (is (number? (:log-likelihood gpd)) "should have log-likelihood")
        (is (pos-int? (:exceedances-count gpd))
            "should have positive exceedances count")))

    (testing "MRL structure"
      (let [raw-data (mapv #(+ 10.0 (* 5.0 (double %))) (range 100))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis) data-map)
            mrl (get-in result [:tail-analysis :tail-analysis
                                [:elapsed-time] :mrl])]
        (is (vector? (:thresholds mrl)) "should have thresholds vector")
        (is (vector? (:values mrl)) "should have values (MRL) vector")
        (is (vector? (:n-exceed mrl)) "should have n-exceed vector")
        (is (= (count (:thresholds mrl)) (count (:values mrl)))
            "thresholds and values should match in length")))

    (testing "high quantiles via GPD extrapolation"
      (let [;; Need 200 samples with 90th percentile threshold to get 20 exceedances
            raw-data (concat
                      (mapv #(+ 10.0 (* 1.0 (double %))) (range 180))
                      [200.0 250.0 300.0 350.0 400.0 450.0 500.0 550.0
                       600.0 650.0 700.0 750.0 800.0 900.0 1000.0
                       1100.0 1200.0 1300.0 1400.0 1500.0])
            samples (metrics-samples {[:elapsed-time] (vec raw-data)} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis) data-map)
            high-qs (get-in result [:tail-analysis :tail-analysis
                                    [:elapsed-time] :high-quantiles])]
        ;; Default quantiles are 0.99, 0.999, 0.9999
        (is (number? (get high-qs 0.99)) "should have 0.99 quantile")
        (is (number? (get high-qs 0.999)) "should have 0.999 quantile")
        (is (number? (get high-qs 0.9999)) "should have 0.9999 quantile")
        ;; Higher quantiles should be larger
        (is (< (double (get high-qs 0.99)) (double (get high-qs 0.999)))
            "0.999 quantile should be larger than 0.99")))

    (testing "uses raw samples without outlier filtering"
      ;; The key design decision: tail analysis should NOT filter outliers
      ;; because the outliers ARE the tail we want to analyze
      (let [;; Create data with obvious outliers
            raw-data (conj (vec (mapv #(+ 100.0 (* 0.5 (double %))) (range 199)))
                           ;; This would be an outlier normally
                           10000.0)
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            ;; Add outliers to data-map (which would normally cause filtering)
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            with-outliers ((analyse/outliers) with-quantiles)
            ;; tail-analysis should ignore outliers
            result ((analyse/tail-analysis) with-outliers)
            tail-data (get-in result [:tail-analysis :tail-analysis [:elapsed-time]])]
        ;; All 200 samples should be used (including the "outlier")
        (is (= 200 (:n tail-data))
            "tail analysis should use all samples including outliers")))

    (testing "custom threshold-quantile"
      (let [raw-data (mapv #(+ 10.0 (* 5.0 (double %))) (range 100))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis {:threshold-quantile 0.95}) data-map)
            tail-data (get-in result [:tail-analysis :tail-analysis [:elapsed-time]])]
        (is (= 0.95 (:threshold-quantile tail-data))
            "should use specified threshold quantile")))

    (testing "custom explicit threshold"
      (let [raw-data (mapv #(+ 10.0 (* 5.0 (double %))) (range 100))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis {:threshold 200.0}) data-map)
            tail-data (get-in result [:tail-analysis :tail-analysis [:elapsed-time]])]
        (is (= 200.0 (:threshold tail-data))
            "should use explicit threshold")))

    (testing "custom high quantiles"
      (let [;; Need 200 samples with 90th percentile threshold to get 20 exceedances
            raw-data (concat
                      (mapv #(+ 10.0 (* 1.0 (double %))) (range 180))
                      [200.0 250.0 300.0 350.0 400.0 450.0 500.0 550.0
                       600.0 650.0 700.0 750.0 800.0 900.0 1000.0
                       1100.0 1200.0 1300.0 1400.0 1500.0])
            samples (metrics-samples {[:elapsed-time] (vec raw-data)} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis {:high-quantiles [0.95 0.99 0.995]})
                    data-map)
            high-qs (get-in result [:tail-analysis :tail-analysis
                                    [:elapsed-time] :high-quantiles])]
        (is (number? (get high-qs 0.95)) "should have custom 0.95 quantile")
        (is (number? (get high-qs 0.99)) "should have custom 0.99 quantile")
        (is (number? (get high-qs 0.995)) "should have custom 0.995 quantile")
        (is (nil? (get high-qs 0.9999)) "should not have default 0.9999")))

    (testing "returns data-map unchanged when samples unavailable"
      (let [data-map {:other-data 123}
            result ((analyse/tail-analysis) data-map)]
        (is (= data-map result))
        (is (not (contains? result :tail-analysis)))))

    (testing "returns data-map unchanged for insufficient samples"
      (let [;; Less than 30 samples
            raw-data (mapv #(+ 10.0 (double %)) (range 20))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis) data-map)]
        ;; Should return without tail analysis due to insufficient samples
        (is (not (contains? (:tail-analysis result) [:elapsed-time]))
            "should not compute tail analysis for < 30 samples")))

    (testing "uses custom output id"
      (let [raw-data (mapv #(+ 10.0 (* 5.0 (double %))) (range 100))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/tail-analysis {:id :my-tail}) data-map)]
        (is (contains? result :my-tail))
        (is (not (contains? result :tail-analysis)))))

    (testing "uses custom samples-id"
      (let [raw-data (mapv #(+ 10.0 (* 5.0 (double %))) (range 100))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:my-samples samples}
            result ((analyse/tail-analysis {:samples-id :my-samples}) data-map)]
        (is (contains? result :tail-analysis))))))

;;; Autocorrelation Analysis Tests
;; Tests the analyse/autocorrelation function for detecting sample non-independence.
;; Validates output structure, severity classification, pattern detection,
;; and proper handling of edge cases.

(deftest autocorrelation-test
  (testing "autocorrelation"
    (testing "returns correct structure"
      (let [;; Generate independent samples (low autocorrelation expected)
            raw-data (mapv #(+ 100.0 (* (Math/random) 10.0) (double %)) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/autocorrelation) data-map)]
        (is (contains? result :autocorrelation)
            "result should have :autocorrelation key")
        (let [autocorr-data (:autocorrelation result)]
          (is (= :criterium/autocorrelation (:type autocorr-data)))
          (is (= :samples (:source-id autocorr-data)))
          (is (map? (util/autocorrelation autocorr-data)))
          (is (contains? (util/autocorrelation autocorr-data) [:elapsed-time]))
          (let [elapsed-autocorr (get-in autocorr-data
                                         [:autocorrelation [:elapsed-time]])]
            (is (map? (:acf elapsed-autocorr)) "should have :acf map")
            (is (map? (:lag-1 elapsed-autocorr)) "should have :lag-1 map")
            (is (number? (:value (:lag-1 elapsed-autocorr))))
            (is (keyword? (:severity (:lag-1 elapsed-autocorr))))
            (is (map? (:effective-sample-size elapsed-autocorr)))
            (is (number? (:n-original (:effective-sample-size elapsed-autocorr))))
            (is (number? (:n-effective (:effective-sample-size elapsed-autocorr))))
            (is (number? (:ratio (:effective-sample-size elapsed-autocorr))))
            (is (number? (:ci-inflation-factor elapsed-autocorr)))
            (is (map? (:ljung-box elapsed-autocorr)))
            (is (number? (:q-statistic (:ljung-box elapsed-autocorr))))
            (is (number? (:df (:ljung-box elapsed-autocorr))))
            (is (number? (:p-value (:ljung-box elapsed-autocorr))))
            (is (keyword? (:pattern elapsed-autocorr)))
            (is (keyword? (:classification elapsed-autocorr)))))))

    (testing "detects high autocorrelation in correlated samples"
      (let [;; Generate highly autocorrelated samples (each sample depends on previous)
            n 50
            raw-data (loop [i 1
                            prev 100.0
                            result [prev]]
                       (if (>= i n)
                         result
                         (let [next-val (+ (* 0.9 prev) (* 0.1 (+ 100.0 (Math/random))))]
                           (recur (inc i) next-val (conj result next-val)))))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/autocorrelation) data-map)
            elapsed-autocorr (get-in result [:autocorrelation
                                             :autocorrelation
                                             [:elapsed-time]])]
        ;; With 0.9 correlation, lag-1 should be high
        (is (> (double (:value (:lag-1 elapsed-autocorr))) 0.5)
            "lag-1 autocorrelation should be high for correlated samples")
        ;; Effective sample size should be reduced
        (is (< (double (:ratio (:effective-sample-size elapsed-autocorr))) 0.5)
            "effective sample ratio should be low for correlated samples")
        ;; CI inflation factor should be elevated
        (is (> (double (:ci-inflation-factor elapsed-autocorr)) 1.5)
            "CI inflation should be elevated for correlated samples")
        ;; Classification should not be :pass
        (is (not= :pass (:classification elapsed-autocorr))
            "classification should not be :pass for highly correlated samples")))

    (testing "returns data-map unchanged for insufficient samples"
      (let [;; Less than 20 samples
            raw-data (mapv #(+ 10.0 (double %)) (range 15))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/autocorrelation) data-map)]
        ;; Should not have autocorrelation analysis due to insufficient samples
        (is (not (contains? (:autocorrelation result) [:elapsed-time]))
            "should not compute autocorrelation for < 20 samples")))

    (testing "returns data-map unchanged when samples unavailable"
      (let [data-map {:other-data 123}
            result ((analyse/autocorrelation) data-map)]
        (is (= data-map result))
        (is (not (contains? result :autocorrelation)))))

    (testing "uses custom output id"
      (let [raw-data (mapv #(+ 100.0 (double %)) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            result ((analyse/autocorrelation {:id :my-autocorr}) data-map)]
        (is (contains? result :my-autocorr))
        (is (not (contains? result :autocorrelation)))))

    (testing "uses custom samples-id"
      (let [raw-data (mapv #(+ 100.0 (double %)) (range 50))
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:my-samples samples}
            result ((analyse/autocorrelation {:samples-id :my-samples}) data-map)]
        (is (contains? result :autocorrelation))))

    (testing "does not filter outliers"
      ;; Autocorrelation should use raw samples including outliers
      (let [raw-data (conj (vec (mapv #(+ 100.0 (double %)) (range 49)))
                           10000.0) ; outlier
            samples (metrics-samples {[:elapsed-time] raw-data} 1)
            data-map {:samples samples}
            ;; Add outliers to data-map
            with-quantiles ((analyse/quantiles {:quantiles []}) data-map)
            with-outliers ((analyse/outliers) with-quantiles)
            ;; Autocorrelation should still use all 50 samples
            result ((analyse/autocorrelation) with-outliers)
            elapsed-autocorr (get-in result [:autocorrelation
                                             :autocorrelation
                                             [:elapsed-time]])]
        (is (= 50 (:n-original (:effective-sample-size elapsed-autocorr)))
            "autocorrelation should use all samples including outliers")))))
