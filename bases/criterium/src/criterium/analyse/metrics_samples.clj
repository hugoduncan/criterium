(ns criterium.analyse.metrics-samples
  (:require
   [criterium.analyse.methods :as methods]
   [criterium.array :as arr]
   criterium.array-core.interface
   [criterium.collect-plan :as collect-plan]
   [criterium.random.interface :as random]
   [criterium.stats.interface :as si]
   [criterium.util.helpers :as util]
   [criterium.util.histogram :as histogram]
   [criterium.util.invariant :refer [have]]
   [criterium.util.kde :as kde]
   [criterium.util.sampled-stats :as sampled-stats]
   [criterium.util.stats :as stats]))

(def ^:private metrics-samples-keys
  "Keys for :criterium/metrics-samples type, used for select-keys."
  #{:type
    :transform
    :source-id
    :metric->values
    :num-samples
    :batch-size
    :metrics-defs
    :expr-value})

(derive :criterium/collected-metrics-samples :criterium/metrics-samples)

(defmethod methods/transform :criterium/metrics-samples
  [metrics-samples metric-configs f inv-f _options]
  (let [metric->values (util/metric->values metrics-samples)
        metric->values' (reduce
                         (fn x-path [result path]
                           (assoc
                            result
                            path
                            (arr/dmap (metric->values path) f)))
                         {}
                         (mapv :path metric-configs))]
    (->
     (select-keys
      metrics-samples
      (disj metrics-samples-keys :metrics-defs))
     (merge
      {:metric->values metric->values'
       :transform {:sample-> inv-f :->sample f}}))))

(defmethod methods/quantiles :criterium/metrics-samples
  [metrics-samples metric-configs options]
  (let [quantiles (sampled-stats/quantiles
                   (util/metric->values metrics-samples)
                   metric-configs
                   options)]
    {:type :criterium/quantiles
     :quantiles quantiles
     :transform collect-plan/identity-transforms}))

(defn outlier-count
  [low-severe low-mild high-mild high-severe]
  {:low-severe low-severe
   :low-mild low-mild
   :high-mild high-mild
   :high-severe high-severe})

(defn classifier
  [[^double low-severe ^double low-mild ^double high-mild ^double high-severe]]
  (fn [^double x i]
    (when-not (<= low-mild x high-mild)
      [i (cond
           (<= x low-severe) :low-severe
           (< low-severe x low-mild) :low-mild
           (> high-severe x high-mild) :high-mild
           (>= x high-severe) :high-severe)])))

(defn samples-outliers
  "Compute outliers for each metric.
  Returns a map with thresholds, outliers, outlier-counts, and (for adjusted
  method) medcouple for each metric path.

  Options:
    :outlier-method - :adjusted (default), :standard, or :auto
                      :adjusted uses the adjusted boxplot method accounting for
                      skewness via medcouple
                      :standard uses symmetric 1.5×IQR whiskers
                      :auto is equivalent to :adjusted for metrics-samples"
  [metric-configs all-quantiles samples options]
  (let [outlier-method (get options :outlier-method)
        use-adjusted? (not= outlier-method :standard)]
    (reduce
     (fn sample-m [result metric-config]
       (let [path (:path metric-config)
             quantiles (have map? (get-in all-quantiles path)
                             {:all-quantiles all-quantiles})
             q1 (get quantiles 0.25)
             q3 (get quantiles 0.75)
             sample-values (get samples path)
             sorted-samples (arr/sorted sample-values)
             mc (when use-adjusted?
                  (stats/medcouple sorted-samples))
             thresholds (if use-adjusted?
                          (stats/adjusted-boxplot-outlier-thresholds q1 q3 mc)
                          (stats/boxplot-outlier-thresholds q1 q3))
             classifier (classifier thresholds)
             outliers (when (apply not= thresholds)
                        (arr/indexed-dfold
                         sample-values
                         (fn [m ^long i ^double v]
                           (if-let [[idx class] (classifier v i)]
                             (assoc m idx class)
                             m))
                         {}))
             outlier-counts (reduce-kv
                             (fn [counts _i v]
                               (update counts v inc))
                             (outlier-count 0 0 0 0)
                             outliers)]
         (update-in result path
                    assoc
                    :thresholds thresholds
                    :outliers outliers
                    :outlier-counts outlier-counts
                    :medcouple mc)))
     {}
     metric-configs)))

(defmethod methods/outliers :criterium/metrics-samples
  [metrics-samples all-quantiles metric-configs options]
  (let [outliers (samples-outliers
                  metric-configs
                  (util/quantiles all-quantiles)
                  (util/metric->values metrics-samples)
                  options)]
    {:type :criterium/outliers
     :outliers outliers
     :num-samples (:num-samples metrics-samples)
     :transform collect-plan/identity-transforms}))

(defmethod methods/stats :criterium/metrics-samples
  [metrics-samples outliers metric-configs options]
  (let [metric->values (util/metric->values metrics-samples)
        stats (sampled-stats/sample-stats
               metric->values
               (when outliers (util/outliers outliers))
               metric-configs
               options)]
    {:type :criterium/stats
     :stats stats
     :transform collect-plan/identity-transforms}))

;;; KDE-based statistics

(defn- trapezoidal-integrate
  "Numerical integration using the trapezoidal rule.
  f-values is a sequence of function values at evenly spaced grid points.
  grid is the x-coordinates of those points.
  Returns the integral estimate."
  ^double [grid f-values]
  (let [n (count grid)]
    (if (< n 2)
      0.0
      (loop [i 1
             sum 0.0]
        (if (< i n)
          (let [x0 (double (nth grid (dec i)))
                x1 (double (nth grid i))
                f0 (double (nth f-values (dec i)))
                f1 (double (nth f-values i))
                dx (- x1 x0)]
            (recur (inc i) (+ sum (* 0.5 dx (+ f0 f1)))))
          sum)))))

(defn- kde-mean
  "Compute density-weighted mean: ∫ x·f(x) dx.
  Assumes density is normalized (integrates to 1)."
  ^double [grid density]
  (let [xf (mapv * grid density)]
    (trapezoidal-integrate grid xf)))

(defn- kde-variance
  "Compute density-weighted variance: ∫ (x-μ)²·f(x) dx.
  Requires mean to be pre-computed."
  ^double [grid density ^double mean]
  (let [sq-dev (mapv (fn [^double x ^double f]
                       (let [d (- x mean)]
                         (* d d f)))
                     grid density)]
    (trapezoidal-integrate grid sq-dev)))

(defn- kde-stats-for-metric
  "Compute standard statistics from a KDE density estimate for a single metric.
  Returns the same structure as sample-based stats."
  [kde-data]
  (let [{:keys [grid density n]} kde-data
        mean (kde-mean grid density)
        variance (kde-variance grid density mean)
        min-val (double (first grid))
        max-val (double (last grid))
        three-sigma (* 3.0 (Math/sqrt variance))]
    {:mean mean
     :variance variance
     :min-val min-val
     :max-val max-val
     :mean-plus-3sigma (+ mean three-sigma)
     :mean-minus-3sigma (- mean three-sigma)
     :n n}))

(defmethod methods/stats :criterium/kde
  [kde-map _outliers metric-configs _options]
  (let [kdes (:kdes kde-map)
        stats (reduce
               (fn [result metric-config]
                 (let [p (:path metric-config)
                       kde-data (get kdes p)]
                   (if kde-data
                     (assoc-in result p (kde-stats-for-metric kde-data))
                     result)))
               {}
               metric-configs)]
    {:type :criterium/stats
     :stats stats
     :transform (:transform kde-map)}))

(defmethod methods/event-stats :criterium/metrics-samples
  [metrics-samples metrics-defs _options]
  (let [metric->values (util/metric->values metrics-samples)
        event-stats (sampled-stats/event-stats
                     metrics-defs
                     metric->values)]
    {:type :criterium/event-stats
     :event-stats event-stats
     :batch-size (:batch-size metrics-samples)
     :transform collect-plan/identity-transforms}))

(defn- remove-outliers
  "Removes samples at outlier indices. Returns a DoubleArray."
  [samples outliers]
  (arr/filter-indices samples (set (keys outliers))))

(defn histogram
  [metric->values quantiles outliers metric-config options]
  (try
    (let [p (:path metric-config)
          iqr (when-let [qs (get-in quantiles p)]
                (- (double (get qs 0.75)) (double (get qs 0.25))))
          samples (metric->values p)
          outliers (get-in outliers p)
          samples (if-let [ols (:outliers outliers)]
                    (remove-outliers samples ols)
                    samples)
          ;; Build histogram options from analysis options
          hist-opts (cond-> {}
                      (:method options)   (assoc :method (:method options))
                      (:max-bins options) (assoc :max-bins (:max-bins options))
                      ;; For freedman-diaconis, pass IQR if available
                      (and (not= :knuth (:method options)) iqr)
                      (assoc :iqr iqr))]
      ;; Convert typed array to vector for histogram computation
      (histogram/histogram (arr/to-double-vec samples) hist-opts))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (when-not (#{:histogram/no-values :histogram/same-values}
                   (:error data))
          (throw e))))))

(defmethod methods/histogram :criterium/metrics-samples
  [metrics-samples quantiles outliers metric-configs options]
  (let [histograms (->> metric-configs
                        (mapv
                         (juxt :path
                               #(histogram
                                 (util/metric->values metrics-samples)
                                 (util/quantiles quantiles)
                                 (util/outliers outliers)
                                 %
                                 options)))
                        (filterv (comp some? second))
                        (into {}))]
    {:type :criterium/histogram
     :histograms histograms
     :transform collect-plan/identity-transforms}))

(defn kde-for-metric
  "Compute KDE for a single metric's samples, filtering outliers if present."
  [metric->values outliers metric-config options]
  (try
    (let [p (:path metric-config)
          samples-arr (metric->values p)
          outliers (get-in outliers p)
          samples-arr (if-let [ols (:outliers outliers)]
                        (remove-outliers samples-arr ols)
                        samples-arr)
          ;; Convert to vector for KDE computation
          samples (arr/to-double-vec samples-arr)]
      (when (seq samples)
        (kde/kde samples options)))
    (catch clojure.lang.ExceptionInfo e
      (let [data (ex-data e)]
        (when-not (#{:kde/no-data :kde/constant-data} (:error data))
          (throw e))))))

(defmethod methods/kde :criterium/metrics-samples
  [metrics-samples outliers metric-configs options]
  (let [metric->values (util/metric->values metrics-samples)
        outliers (when outliers (util/outliers outliers))
        kdes (->> metric-configs
                  (mapv
                   (juxt :path
                         #(kde-for-metric metric->values outliers % options)))
                  (filterv (comp some? second))
                  (into {}))]
    (when (seq kdes)
      {:type :criterium/kde
       :kdes kdes
       :transform collect-plan/identity-transforms})))

(defn modes-for-metric
  "Compute modes with statistical validation for a single metric.

  Takes KDE output and raw samples, runs multimodality test for k=1 up to max-modes,
  and computes confidence intervals for detected modes.

  Options:
  - :method - test method, :acr (default) or :silverman
  - :mode-method - mode finding method:
    - :isj (default) - find modes from KDE density at ISJ bandwidth
    - :critical - find modes at critical bandwidth for validated k modes
  - :max-modes, :n-bootstrap, :alpha, :n-points - as usual"
  [kde-data samples-arr outliers metric-config options]
  (try
    (let [{:keys [grid density bandwidth]} kde-data
          {:keys [max-modes n-bootstrap alpha n-points method mode-method]
           :or {max-modes 5 n-bootstrap 200 alpha 0.05 n-points 512
                method :acr mode-method :isj}} options
          max-modes (long max-modes)
          alpha (double alpha)
          test-fn (case method
                    :acr kde/acr-test
                    :silverman kde/silverman-test)
          p (:path metric-config)
          ;; Filter outliers from samples
          outliers-data (get-in outliers p)
          samples-arr (if-let [ols (:outliers outliers-data)]
                        (remove-outliers samples-arr ols)
                        samples-arr)
          ;; Convert to vector for KDE functions
          samples (arr/to-double-vec samples-arr)
          ;; Find modes from existing KDE density (for initial mode count)
          grid-arr (double-array grid)
          density-arr (double-array density)
          all-modes (kde/find-modes grid-arr density-arr)
          n-all-modes (long (count all-modes))
          ;; Run multimodality test for each k from 1 to max-modes
          test-results
          (into {}
                (for [k (range 1 (inc (min max-modes n-all-modes)))]
                  [k (test-fn samples k
                              {:n-bootstrap n-bootstrap
                               :n-points n-points
                               :alpha alpha})]))
          ;; Determine validated number of modes
          ;; Find smallest k where p-value >= alpha (fail to reject H0: <= k modes)
          validated-k (or (some (fn [k]
                                  (when (>= (double (get-in test-results [k :p-value])) alpha)
                                    k))
                                (range 1 (inc (min max-modes n-all-modes))))
                          n-all-modes)
          ;; Get mode locations based on mode-method
          {:keys [modes-with-ci mode-bandwidth antimodes]}
          (case mode-method
            :critical
            ;; Use critical bandwidth to find modes
            (let [locate-result (kde/locate-modes samples validated-k
                                                  {:n-points n-points})
                  h-crit (:critical-bandwidth locate-result)
                  ;; Build mode CIs at critical bandwidth
                  sample-min (double (reduce min samples))
                  sample-max (double (reduce max samples))
                  sample-range (- sample-max sample-min)
                  grid-step (/ sample-range (double (dec (long n-points))))
                  crit-grid (double-array (range sample-min
                                                 (+ sample-max 0.1)
                                                 grid-step))
                  modes-ci (kde/mode-confidence-intervals
                            samples h-crit crit-grid validated-k
                            {:n-bootstrap n-bootstrap
                             :alpha alpha})]
              {:modes-with-ci modes-ci
               :mode-bandwidth h-crit
               :antimodes (:antimodes locate-result)})

            ;; :isj - use existing KDE density from ISJ bandwidth
            {:modes-with-ci (kde/mode-confidence-intervals
                             samples bandwidth grid-arr validated-k
                             {:n-bootstrap n-bootstrap
                              :alpha alpha})
             :mode-bandwidth bandwidth
             :antimodes nil})
          ;; Mark significance based on test results
          ;; For ACR, all validated modes are significant by construction
          ;; (validated-k is the number of modes supported by the test)
          modes-with-significance
          (if (= method :acr)
            (mapv #(assoc % :significant? true) modes-with-ci)
            ;; For Silverman, use per-k p-value check
            (vec (map-indexed
                  (fn [i mode]
                    (let [k (inc (long i))
                          test-result (get test-results k)
                          significant? (and test-result
                                            (< (double (:p-value test-result)) alpha))]
                      (assoc mode :significant? significant?)))
                  modes-with-ci)))]
      (cond-> {:modes modes-with-significance
               :n-modes validated-k
               :test-results {:method method
                              :k-tested (vec (keys test-results))
                              :p-values (into {} (map (fn [[k v]] [k (:p-value v)])
                                                      test-results))
                              :critical-bandwidths (into {} (map (fn [[k v]]
                                                                   [k (:critical-bandwidth v)])
                                                                 test-results))
                              :excess-mass (when (= method :acr)
                                             (into {} (map (fn [[k v]] [k (:excess-mass v)])
                                                           test-results)))}}
        ;; Include mode-method and mode-bandwidth when using critical
        (= mode-method :critical) (assoc :mode-method :critical
                                         :mode-bandwidth mode-bandwidth
                                         :antimodes antimodes)))
    (catch Exception e
      (when-not (#{:kde/no-data :kde/constant-data}
                 (:error (ex-data e)))
        (throw e)))))

(defmethod methods/modes :criterium/kde
  [kde-map samples outliers metric-configs options]
  (let [kdes (:kdes kde-map)
        metric->values (util/metric->values samples)
        outliers (when outliers (util/outliers outliers))
        modes-results
        (->> metric-configs
             (mapv
              (fn [metric-config]
                (let [p (:path metric-config)
                      kde-data (get kdes p)
                      sample-values (metric->values p)]
                  (when (and kde-data sample-values)
                    [p (modes-for-metric kde-data sample-values outliers
                                         metric-config options)]))))
             (filterv (comp some? second))
             (into {}))]
    (when (seq modes-results)
      {:type :criterium/modes
       :modes modes-results
       :transform (:transform kde-map)})))

;;; Distribution Fitting

(def ^:private all-distributions
  "All distributions supported for fitting."
  #{:gamma :lognormal :inverse-gaussian :weibull})

(def ^:private distribution-num-params
  "Number of parameters for each distribution (for AIC/BIC)."
  {:gamma 2
   :lognormal 2
   :inverse-gaussian 2
   :weibull 2})

(defn- fit-distribution
  "Fit a single distribution to samples using MLE.
  Returns {:params {...} :log-likelihood ... :error ...} or nil on failure."
  [dist samples]
  (try
    (case dist
      :gamma (si/gamma-mle samples)
      :lognormal (si/lognormal-mle samples)
      :inverse-gaussian (si/inverse-gaussian-mle samples)
      :weibull (si/weibull-mle samples))
    (catch Exception e
      {:error (.getMessage e)})))

(defn- make-cdf-fn
  "Create a CDF function for the given distribution and parameters."
  [dist params]
  (case dist
    :gamma (si/gamma-cdf (:shape params) (:scale params))
    :lognormal (si/lognormal-cdf (:mu params) (:sigma params))
    :inverse-gaussian (si/inverse-gaussian-cdf (:mu params) (:lambda params))
    :weibull (si/weibull-cdf (:shape params) (:scale params))))

(defn- compute-gof-tests
  "Compute goodness-of-fit tests (K-S and CvM) for fitted distribution."
  [samples cdf-fn]
  {:ks-test (si/ks-test samples cdf-fn)
   :cvm-test (si/cvm-test samples cdf-fn)})

(defn- compute-information-criteria
  "Compute AIC, BIC, and AICc for a fitted model."
  [dist n log-likelihood]
  (let [k (long (get distribution-num-params dist 2))
        n (long n)]
    {:aic (si/aic k log-likelihood)
     :bic (si/bic k n log-likelihood)
     :aicc (when (> n (inc k))
             (si/aicc k n log-likelihood))}))

(defn- bootstrap-parameter-ci
  "Bootstrap confidence intervals for distribution parameters.
  Returns map of parameter name to {:point-estimate :ci-lower :ci-upper}."
  [dist samples {:keys [n-bootstrap alpha]
                 :or {n-bootstrap 200 alpha 0.05}}]
  (let [n (count samples)
        n-bootstrap (long n-bootstrap)
        alpha (double alpha)
        bootstrap-size (max 50 (long (* n 0.8)))
        quantiles [(/ alpha 2.0) (- 1.0 (/ alpha 2.0))]
        ;; Bootstrap the MLE fitting
        fit-fn (fn [s] (fit-distribution dist s))
        rng-factory random/well-rng-1024a
        ;; Run bootstrap
        bootstrap-fits
        (loop [i (long 0)
               results []]
          (if (>= i n-bootstrap)
            results
            (let [rng (rng-factory)
                  indices (si/sample-uniform bootstrap-size n rng)
                  boot-samples (mapv #(nth samples (int %)) indices)
                  fit (fit-fn boot-samples)]
              (recur (inc i)
                     (if (:error fit)
                       results
                       (conj results (:params fit)))))))
        ;; Extract parameter estimates
        param-keys (case dist
                     :gamma [:shape :scale]
                     :lognormal [:mu :sigma]
                     :inverse-gaussian [:mu :lambda]
                     :weibull [:shape :scale])
        ;; Compute CIs for each parameter
        original-fit (fit-fn samples)]
    (when-not (:error original-fit)
      (into {}
            (for [param-key param-keys]
              (let [values (mapv #(get % param-key) bootstrap-fits)
                    sorted-vals (sort values)
                    n-boot (count sorted-vals)]
                (when (>= n-boot 10)
                  [param-key
                   {:point-estimate (get-in original-fit [:params param-key])
                    :ci-lower (nth sorted-vals (long (* n-boot (double (first quantiles)))))
                    :ci-upper (nth sorted-vals (min (dec n-boot)
                                                    (long (* n-boot (double (second quantiles))))))}])))))))

(defn- fit-distributions-for-metric
  "Fit all applicable distributions to samples for a single metric.
  Returns fit results including best model selection.
  Accepts both vectors and typed arrays."
  [samples options]
  (let [{:keys [distributions n-bootstrap alpha]
         :or {n-bootstrap 200 alpha 0.05}} options
        ;; Support both typed arrays and vectors
        n (if (instance? criterium.array_core.interface.ITypedArray samples)
            (arr/length samples)
            (count samples))
        ;; Compute sample statistics for moment-match prefilter
        ;; si/mean and si/variance work with typed arrays directly
        mean-val (si/mean samples)
        var-val (si/variance samples)
        ;; Convert to vector for MLE functions (they use sequence operations)
        samples-vec (if (instance? criterium.array_core.interface.ITypedArray samples)
                      (arr/to-double-vec samples)
                      samples)
        ;; Determine which distributions to fit
        requested-dists (if distributions
                          (set distributions)
                          all-distributions)
        ;; Use moment-match prefilter to screen distributions
        prefilter-results (si/moment-match-prefilter mean-val var-val requested-dists)
        suitable-dists (si/suitable-distributions mean-val var-val requested-dists)
        ;; Fit each distribution (using vector for MLE/GOF functions)
        fit-results
        (into {}
              (for [dist requested-dists]
                (if (contains? suitable-dists dist)
                  (let [fit (fit-distribution dist samples-vec)]
                    (if (:error fit)
                      [dist {:error (:error fit)}]
                      (let [{:keys [params log-likelihood]} fit
                            cdf-fn (make-cdf-fn dist params)
                            gof (compute-gof-tests samples-vec cdf-fn)
                            ic (compute-information-criteria dist n log-likelihood)]
                        [dist (merge {:params params
                                      :log-likelihood log-likelihood}
                                     ic
                                     gof)])))
                  ;; Distribution failed moment-match prefilter
                  [dist {:skipped :moment-match-failed
                         :prefilter-result (get prefilter-results dist)}])))
        ;; Find best model by AIC (lowest AIC wins)
        valid-fits (filter (fn [[_ v]] (and (:aic v) (not (:error v)) (not (:skipped v))))
                           fit-results)
        best-model (when (seq valid-fits)
                     (first (apply min-key (fn [[_ v]] (:aic v)) valid-fits)))
        best-aic (when best-model (get-in fit-results [best-model :aic]))
        ;; Add delta-AIC to each fit
        fit-results-with-delta
        (into {}
              (for [[dist result] fit-results]
                [dist (if (and (:aic result) best-aic)
                        (assoc result :delta-aic (- (double (:aic result))
                                                    (double best-aic)))
                        result)]))
        ;; Bootstrap parameter CIs for best model only (using vector)
        parameter-cis (when best-model
                        {best-model (bootstrap-parameter-ci
                                     best-model samples-vec
                                     {:n-bootstrap n-bootstrap :alpha alpha})})]
    {:n n
     :warning (when (< n 30) :small-sample)
     :distributions fit-results-with-delta
     :best-model best-model
     :parameter-cis parameter-cis}))

(defn distribution-fit-for-metric
  "Compute distribution fitting for a single metric's samples.

  Returns fit results including :sample-range [min max] of the filtered samples
  used for fitting, enabling viewers to display consistent axis ranges."
  [metric->values outliers metric-config options]
  (try
    (let [p (:path metric-config)
          samples-arr (metric->values p)
          outliers-data (get-in outliers p)
          samples-arr (if-let [ols (:outliers outliers-data)]
                        (remove-outliers samples-arr ols)
                        samples-arr)
          n (arr/length samples-arr)]
      (when (> n 2)
        ;; Use stats functions that accept typed arrays for min/max
        (let [sample-min (si/min samples-arr)
              sample-max (si/max samples-arr)]
          (assoc (fit-distributions-for-metric samples-arr options)
                 :sample-range [sample-min sample-max]))))
    (catch Exception e
      {:error (.getMessage e)})))

(defmethod methods/distribution-fit :criterium/metrics-samples
  [metrics-samples outliers metric-configs options]
  (let [metric->values (util/metric->values metrics-samples)
        outliers (when outliers (util/outliers outliers))
        fit-results
        (->> metric-configs
             (mapv
              (fn [metric-config]
                (let [p (:path metric-config)]
                  [p (distribution-fit-for-metric
                      metric->values outliers metric-config options)])))
             (filterv (comp some? second))
             (into {}))]
    (when (seq fit-results)
      {:type :criterium/distribution-fit
       :fits fit-results
       :transform collect-plan/identity-transforms})))
