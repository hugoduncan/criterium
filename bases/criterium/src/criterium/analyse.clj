(ns criterium.analyse
  (:require
   [criterium.metric :as metric]
   [criterium.util.debug :as debug]
   [criterium.util.helpers :as util]
   [criterium.util.sampled-stats :as sampled-stats]
   [criterium.util.stats :as stats]))

(defn exp [v]
  (Math/exp v))

(defn log [v]
  (Math/log v))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn transform-log
  "Log transformation.
  Return a function to calculate the natural log of quantitative, :time
  dimension, samples."
  ([] (transform-log {}))
  ([{:keys [id samples-id metric-ids]}]
   (fn transform-log [sampled]
     (let [samples-id        (or samples-id :samples)
           id                (or id (keyword (str "log-" (name samples-id))))
           metric-configs    (metric/metric-configs-of-type
                              (:metrics-configs sampled)
                              :quantitative metric-ids)
           transform-samples (fn x-sample [samples]
                               (reduce
                                (fn x-path [result path]
                                  (assoc result
                                         path (mapv log (get samples path))))
                                {}
                                (->> metric-configs
                                     (filterv #(#{:time} (:dimension %)))
                                     (mapv :path))))]
       (->  sampled
            (assoc id (with-meta
                        (transform-samples (samples-id sampled))
                        {:source-id samples-id
                         :type      :criterium/samples
                         :transform {:sample-> exp :->sample log}})))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn quantiles
  "Calculate sample quantiles.
  Return a function to calculate the quantiles of quantitative samples."
  ([] (quantiles {}))
  ([{:keys [id samples-id metric-ids] :as analysis}]
   (fn quantiles [sampled]
     (let [samples-id     (or samples-id :samples)
           id             (or id :quantiles)
           metric-configs (metric/metric-configs-of-type
                           (:metrics-configs sampled)
                           :quantitative metric-ids)
           samples        (get sampled samples-id)
           transforms     (util/get-transforms sampled samples-id)
           res            (sampled-stats/quantiles
                           samples
                           metric-configs
                           transforms
                           analysis)]
       (assoc sampled id
              (with-meta res
                {:source-id samples-id
                 :type      :criterium/stats
                 :transform {:sample-> identity :->sample identity}}))))))

(defn outlier-count
  [low-severe low-mild high-mild high-severe]
  {:low-severe  low-severe
   :low-mild    low-mild
   :high-mild   high-mild
   :high-severe high-severe})

(defn classifier
  [[^double low-severe ^double low-mild ^double high-mild ^double high-severe]]
  (fn [^double x i]
    (when-not (<= low-mild x high-mild)
      [i (cond
           (<= x low-severe)           :low-severe
           (< low-severe x low-mild)   :low-mild
           (> high-severe x high-mild) :high-mild
           (>= x high-severe)          :high-severe)])))

(defn samples-outliers [metric-configs all-quantiles samples transforms]
  (reduce
   (fn sample-m [result metric-config]
     (let [path           (:path metric-config)
           quantiles      (get-in all-quantiles path)
           _              (assert (map? quantiles) quantiles)
           thresholds     (stats/boxplot-outlier-thresholds
                           (get quantiles 0.25)
                           (get quantiles 0.75))
           thresholds     (mapv
                           #(util/transform->sample % transforms)
                           thresholds)
           classifier     (classifier thresholds)
           outliers       (when (apply not= thresholds)
                            (into {}
                                  (mapv classifier
                                        (get samples path)
                                        (range))))
           outlier-counts (reduce-kv
                           (fn [counts _i v]
                             (update counts v inc))
                           (outlier-count 0 0 0 0)
                           outliers)]
       (update-in result path
                  assoc
                  :thresholds thresholds
                  :outliers outliers
                  :outlier-counts outlier-counts)))
   {}
   metric-configs))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn outliers
  "Calculate outiers.
  Return a function to calculate the outliers of quantitative samples."
  ([] (outliers {}))
  ([{:keys [id samples-id quantiles-id metric-ids]}]
   (fn [sampled]
     (let [id             (or id :outliers)
           quantiles-id   (or quantiles-id :quantiles)
           samples-id     (or samples-id :samples)
           metric-configs (metric/metric-configs-of-type
                           (:metrics-configs sampled)
                           :quantitative metric-ids)
           all-quantiles  (get sampled quantiles-id)
           samples        (get sampled samples-id)
           transforms     (util/get-transforms sampled samples-id)]
       (when-not all-quantiles
         (throw (ex-info
                 "outlier analysis requires quantiles analysis"
                 {:quantiles-id  quantiles-id
                  :available-ids (keys sampled)})))
       (let [result (samples-outliers
                     metric-configs all-quantiles samples transforms)]
         (assoc sampled id
                (with-meta
                  result
                  {:type      :criterium/outliers
                   :source-id samples-id})))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn stats
  "Calculate descriptive statistics.
  Return a function to calculate the summary statistics of quantitative
  samples."
  ([] (stats {}))
  ([{:keys [id samples-id outliers-id metric-ids]
     :as   analysis}]
   (let [samples-id  (or samples-id :samples)
         id          (or id :stats)
         outliers-id (or outliers-id :outliers)]
     (fn [sampled]
       (debug/dtap> {:stats id})
       (let [metric-configs (metric/metric-configs-of-type
                             (:metrics-configs sampled)
                             :quantitative
                             metric-ids)
             outliers       (when outliers-id
                              (get sampled outliers-id))
             res            (sampled-stats/sample-stats
                             sampled
                             samples-id
                             outliers
                             metric-configs
                             analysis)]
         (assoc sampled id
                (with-meta res
                  {:source-id samples-id
                   :type      :criterium/stats
                   :transform {:sample-> identity :->sample identity}})))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn event-stats
  "Calculate event statistics.
  Return a function to calculate the event statistics of event samples."
  ([] (event-stats {}))
  ([{:keys [id samples-id metric-ids] :as _analysis}]
   (let [id         (or id :event-stats)
         samples-id (or samples-id :samples)]
     (fn [sampled]
       (debug/dtap> {:event-stats id})
       (let [metrics-configs (metric/metrics-of-type
                              (:metrics-configs sampled)
                              :event metric-ids)
             samples         (sampled samples-id)
             res             (sampled-stats/event-stats
                              metrics-configs
                              samples)]
         (assoc sampled id (with-meta res {:type :criterium/event-stats})))))))

(defn- min-f
  ^double [f ^double q ^double r]
  (min ^double (f q) ^double (f r)))

(defn outlier-significance*
  "Find the significance of outliers given mean and variance estimates.
  Based on how well a gaussian can describe the sample stats.
  See http://www.ellipticgroup.com/misc/article_supplement.pdf, p17."
  [^double mean ^double variance ^long batch-size]
  {:pre [(number? mean) (number? variance) (nat-int? batch-size)]}
  (if (or (zero? variance) (< batch-size 16))
    0
    (let [variance-block (* batch-size variance)
          std-dev-block  (Math/sqrt variance-block)
          mean-g-min     (/ mean 2)
          sigma-g        (min (/ mean-g-min 4)
                              (/ std-dev-block (Math/sqrt batch-size)))
          variance-g     (* sigma-g sigma-g)
          batch-size-sqr (util/sqr batch-size)
          c-max-f        (fn ^long [^double t-min]    ; Eq 38
                           (let [j0-sqr (util/sqr (- mean t-min))
                                 k0     (- (* batch-size-sqr j0-sqr))
                                 k1     (+ variance-block
                                           (- (* batch-size variance-g))
                                           (* batch-size j0-sqr))
                                 det    (- (* k1 k1)
                                           (* 4 variance-g k0))]
                             (long (Math/floor (/ (* -2 k0)
                                                  (+ k1 (Math/sqrt det)))))))
          var-out        (fn ^double [^long c]        ; Eq 45
                           (let [nmc (- batch-size c)]
                             (* (/ nmc (double batch-size))
                                (- variance-block (* nmc variance-g)))))
          c-max          (min-f c-max-f 0.0 mean-g-min)]
      (/ (min-f var-out 1.0 c-max) variance-block))))

(defn outlier-effect
  "Return a keyword describing the effect of outliers on a point estimate."
  [^double significance]
  (cond
    (< significance 0.01) :unaffected
    (< significance 0.1)  :slight
    (< significance 0.5)  :moderate
    :else                 :severe))

(defn- samples-outlier-significance [batch-size outliers stats metric-configs]
  (reduce
   (fn sample-m [result metric]
     (let [path           (:path metric)
           outlier-data   (get-in outliers path)
           stat           (get-in stats path)
           _              (assert (map? outlier-data) outlier-data)
           outlier-counts (:outlier-counts outlier-data)
           significance   (when (some pos? (vals outlier-counts))
                            (outlier-significance*
                             (:mean stat)
                             (:variance stat)
                             batch-size))]
       (update-in result path
                  assoc
                  :significance significance
                  :effect (outlier-effect significance))))
   {}
   metric-configs))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn outlier-significance
  "Calculate outlier significance.
  Return a function to calculate the significance of outliers."
  ([] (outlier-significance {}))
  ([{:keys [id outliers-id stats-id metric-ids] :as _analysis}]
   (fn [sampled]
     (let [id             (or id :outlier-significance)
           outliers-id    (or outliers-id :outliers)
           stats-id       (or stats-id :stats)
           outliers       (get sampled outliers-id)
           stats          (get sampled stats-id)
           metric-configs (metric/metric-configs-of-type
                           (:metrics-configs sampled)
                           :quantitative metric-ids)]
       (when-not outliers
         (throw (ex-info
                 "outlier significance requires outlier analysis"
                 {:outliers-id   outliers-id
                  :available-ids (keys sampled)})))
       (let [result (samples-outlier-significance
                     (:batch-size sampled) outliers stats metric-configs)]
         (assoc sampled id
                (with-meta
                  result
                  {:type :criterium/outlier-significance})))))))
