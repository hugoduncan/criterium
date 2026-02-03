(ns criterium.viewer.kindly.core
  "Kindly viewer core functions for basic metrics display.

  Provides Kindly-annotated output for:
  - metrics, stats, extremes
  - bootstrap statistics
  - samples, histograms, KDE
  - outlier counts and significance
  - event stats (class loader, JIT, GC)
  - quantiles, sample percentiles, sample diffs
  - collect plan, OS, and runtime info

  Uses an accumulator pattern where view functions append Kindly-annotated
  values to an atom. The `flush-viewer` multimethod returns a `kind/fragment`
  combining all accumulated values.

  No runtime dependency on scicloj/kindly - produces plain maps with
  appropriate `:kindly/kind` metadata."
  (:refer-clojure :exclude [flush])
  (:require
   [clojure.string :as str]
   [criterium.array :as arr]
   [criterium.jvm :as jvm]
   [criterium.metric :as metric]
   [criterium.util.format :as format]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have]]
   [criterium.view :as view]
   [criterium.viewer.common-charts.distribution :as charts.distribution]
   [criterium.viewer.common-charts.samples :as charts.samples]
   [criterium.viewer.common.bootstrap :as bootstrap]
   [criterium.viewer.common.core :as core]))

(set! *unchecked-math* false)

;;; Accumulator

(defonce ^{:doc "Accumulator for Kindly-annotated values."}
  accumulated
  (atom []))

(defonce
  ^{:doc "Last flushed Kindly fragment for retrieval after bench completes."}
  last-fragment
  (atom nil))

;;; Chart Dimensions

(def chart-width
  "Width for Kindly vega-lite charts, sized for notebook display."
  700)

(def chart-height
  "Height for Kindly vega-lite charts, sized for notebook display."
  350)

;;; Helper Functions

(defn kindly-add
  "Add a value to the accumulator."
  [value]
  (swap! accumulated conj value)
  nil)

(defn kindly-heading
  "Add a markdown heading to the accumulator."
  [s]
  (kindly-add
   (with-meta
     [(str "**" s "**")]
     {:kindly/kind :kind/md})))

(defn kindly-table
  "Add a table to the accumulator.
  Optionally accepts :column-names in opts for explicit column ordering."
  ([data] (kindly-table data nil))
  ([data {:keys [column-names]}]
   (kindly-add
    (with-meta
      (if column-names
        {:row-maps data :column-names column-names}
        data)
      {:kindly/kind :kind/table}))))

(defn kindly-vega-lite
  "Add a Vega-Lite chart to the accumulator."
  [spec]
  (kindly-add
   (with-meta
     (assoc spec :$schema "https://vega.github.io/schema/vega-lite/v5.json")
     {:kindly/kind :kind/vega-lite})))

(defn kindly-vega
  "Add a full Vega chart to the accumulator."
  [spec]
  (kindly-add
   (with-meta
     (assoc spec :$schema "https://vega.github.io/schema/vega/v5.json")
     {:kindly/kind :kind/vega})))

(defn flush
  "Return accumulated values as a kind/fragment and clear the accumulator.
  Also stores the fragment in `last-fragment` for retrieval after bench
  completes."
  []
  (let [[values _] (swap-vals! accumulated (constantly []))]
    (when (seq values)
      (let [fragment (with-meta values {:kindly/kind :kind/fragment})]
        (reset! last-fragment fragment)
        fragment))))

;;; Flush Viewer

(defmethod view/flush-viewer :kindly [_]
  (flush))

;;; Stats

(defmethod view/stats* :kindly
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (kindly-heading "Summary stats")
          (kindly-table
           (core/stats-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

;;; Extremes

(defmethod view/extremes* :kindly
  [_ {:keys [stats-id metric-ids]} data-map]
  (let [stats-id (or stats-id :stats)
        stats-map (data-map stats-id)]
    (when stats-map
      (let [metrics-defs (-> (:metrics-defs stats-map)
                             (metric/select-metrics metric-ids))
            metric-configs (metric/all-metric-configs metrics-defs)
            transforms (util/get-transforms data-map stats-id)]
        (when (seq metric-configs)
          (kindly-heading "Extremes")
          (kindly-table
           (core/extremes-map
            (util/stats stats-map)
            metric-configs
            transforms)))))))

;;; Quantiles

(defmethod view/quantiles* :kindly
  [_ {:keys [quantiles-id]} data-map]
  (let [quantiles-id (or quantiles-id :quantiles)
        quantiles-map (data-map quantiles-id)
        metrics-defs (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quantiles-id)]
    (kindly-heading "Quantiles")
    (kindly-table
     (core/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

;;; Outlier Counts

(defmethod view/outlier-counts* :kindly
  [_ {:keys [outliers-id] :as _view} data-map]
  (let [outliers-id (or outliers-id :outliers)
        outliers-map (data-map outliers-id)
        metrics-defs (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (kindly-heading "Outliers")
    (kindly-table
     (core/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

;;; Collect Plan

(defmethod view/collect-plan* :kindly
  [_ _view data-map]
  (kindly-heading "Collect plan")
  (kindly-table
   (core/collect-plan-data data-map)))

;;; Samples

(defmethod view/samples* :kindly
  [_ view data-map]
  (kindly-heading "Samples")
  (kindly-vega-lite
   (charts.samples/samples-vega-spec data-map view {:width chart-width
                                                    :height chart-height})))

;;; Histogram

(defmethod view/histogram* :kindly
  [_ view data-map]
  (kindly-heading "Histogram")
  (kindly-vega-lite
   (charts.samples/histogram-vega-spec data-map view {:width chart-width
                                                      :height chart-height})))

;;; KDE

(defmethod view/kde* :kindly
  [_ view data-map]
  (let [kde-id  (or (:kde-id view) :kde)
        kde-map (get data-map kde-id)]
    (when kde-map
      (kindly-heading "Kernel Density Estimation")
      (kindly-vega-lite
       (charts.distribution/kde-vega-spec
        data-map view
        {:width  chart-width
         :height chart-height})))))

;;; Sample Percentiles

(defmethod view/sample-percentiles* :kindly
  [_ view data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metrics-defs (-> (:metrics-defs quant-samples)
                         (metric/filter-metrics
                          (metric/type-pred :quantitative)))
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms (util/get-transforms data-map quant-samples-id)]
    (kindly-heading "Percentiles")
    (kindly-vega-lite
     {:data {:values []}
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:width chart-width
         :height chart-height
         :layer
         (vec
          (into
           [(charts.samples/metric-percentile-layer
             (util/metric->values quant-samples)
             transforms
             (first metric-configs))]))}])})))

;;; Metrics

(defmethod view/metrics* :kindly
  [_ {:keys [samples-id]} data-map]
  (let [samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-defs (:metrics-defs metrics-samples)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (kindly-heading "Metrics")
    (kindly-table
     (core/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

;;; Event Stats

(defmethod view/event-stats* :kindly
  [_ {:keys [event-stats-id]} data-map]
  (let [event-stats-id (or event-stats-id :event-stats)
        event-stats-map (data-map event-stats-id)]
    (when event-stats-map
      (let [metrics-defs (have (:metrics-defs event-stats-map))
            stats (core/event-stats
                   metrics-defs
                   (util/event-stats event-stats-map))]
        (when (seq stats)
          (kindly-heading "Event stats")
          (kindly-table stats))))))

;;; Outlier Significance

(defmethod view/outlier-significance* :kindly
  [_ {:keys [outlier-significance-id] :as _view} data-map]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig-map (data-map outlier-sig-id)
        outlier-sig (util/outlier-significance outlier-sig-map)
        metrics-defs (:metrics-defs outlier-sig-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        significances (vec
                       (for [m metric-configs
                             :let [data (get-in outlier-sig (:path m))]
                             :when (:significance data)]
                         data))]
    (when (seq significances)
      (kindly-heading "Outlier Significance")
      (kindly-table significances))))

;;; Sample Diffs

(defmethod view/sample-diffs* :kindly
  [_ {:keys [] :as view} data-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples (data-map quant-samples-id)
        metric-configs (:metric-configs quant-samples)]
    (kindly-heading "Sample diffs")
    (kindly-vega-lite
     {:data {:values []}
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:width chart-width
         :height chart-height
         :layer
         (vec
          (into
           [(charts.samples/metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))

;;; Bootstrap Stats

(defmethod view/bootstrap-stats* :kindly
  [_ {:keys [bootstrap-stats-id]} data-map]
  (let [bootstrap-stats-id (or bootstrap-stats-id :bootstrap-stats)
        bootstrap-map (data-map bootstrap-stats-id)]
    (when bootstrap-map
      (let [metrics-defs (:metrics-defs bootstrap-map)
            metric-configs (metric/all-metric-configs metrics-defs)
            bootstrap (util/bootstrap bootstrap-map)
            transforms (util/get-transforms data-map bootstrap-stats-id)]
        (when (seq metric-configs)
          (kindly-heading "Bootstrap Statistics")
          (kindly-table
           (vec
            (for [m metric-configs
                  :let [stat (get-in bootstrap (:path m))]
                  :when stat]
              (bootstrap/bootstrap-stat-row m stat transforms)))
           {:column-names [:metric :median :median-ci-lower :median-ci-upper
                           :mean :mean-ci-lower :mean-ci-upper
                           :p10 :p90]}))))))

;;; Final GC Warnings

(defmethod view/final-gc-warnings* :kindly
  [_ {:keys [final-gc-id samples-id warn-threshold]} data-map]
  {:pre [(number? warn-threshold)]}
  (let [final-gc-id (or final-gc-id :final-gc)
        samples-id (or samples-id :samples)
        metrics-samples (data-map samples-id)
        metrics-deps (:metrics-deps metrics-samples)
        gc-metric-configs (metric/all-metric-configs
                           (select-keys
                            metrics-deps
                            [:elapsed-time :garbage-collector]))
        metric (first gc-metric-configs)
        gc-time-metrics (->> (next gc-metric-configs)
                             (filterv #(= :time (:dimension %))))
        metric->values (util/metric->values metrics-samples)
        total (* (:scale metric)
                 (arr/sum (metric->values [:elapsed-time])))
        gc-samples (-> data-map final-gc-id util/metric->values)
        total-gc (reduce
                  +
                  (mapv
                   (fn [m]
                     (* (:scale m) (arr/sum (gc-samples (:path m)))))
                   gc-time-metrics))
        frac (/ total-gc total)]
    (when (and total-gc (> frac warn-threshold))
      (kindly-heading "Final GC Warning")
      (kindly-table
       [{:warning (format
                   "Final GC ran for %s, %.1f%% of total sampling time (%s)"
                   (format/format-value :time total-gc)
                   (* frac 100)
                   (format/format-value :time total))}]))))

;;; OS Info

(defmethod view/os* :kindly
  [_ _ _sampled]
  (let [os (jvm/os-details)]
    (kindly-heading "Operating System")
    (kindly-table
     [{:property "Name" :value (:name os)}
      {:property "Version" :value (:version os)}
      {:property "Architecture" :value (:arch os)}
      {:property "Processors" :value (:available-processors os)}])))

;;; Runtime Info

(defmethod view/runtime* :kindly
  [_ _ _sampled]
  (let [runtime (jvm/runtime-details)]
    (kindly-heading "Runtime")
    (kindly-table
     [{:property "VM Name" :value (:vm-name runtime)}
      {:property "VM Version" :value (:vm-version runtime)}
      {:property "VM Vendor" :value (:vm-vendor runtime)}
      {:property "Arguments"
       :value (str/join " " (:input-arguments runtime))}])))
