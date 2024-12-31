(ns criterium.viewer.portal
  "A viewer that outputs to portal using tap>."
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.probability :as probability]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]))


(defonce tapped (atom {:values '()}))

(defn submit
  "Tap target function.

  This allows criterium to control the order of tapped output.

  ```clojure
  (def submit (criterium.portal/submit #'portal.api/submit))
  (add-tap #'submit)`
  (remove-tap #'submit)
  ``"
  [portal-submit]
  (swap! tapped assoc :portal-submit portal-submit)
  (fn
    [value]
    (swap! tapped update :values conj value)))

(defn flush
  "Flush tapped output"
  []
  (let [{:keys [portal-submit values]} @tapped]
    (doseq [value values]
      (portal-submit value))
    (swap! tapped assoc :values '())))

(defmethod view/flush-viewer :portal [_]
  (flush))

(defn portal-heading [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/hiccup})))

(defn portal-table [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/table})))

(defn portal-vega-lite [s]
  (tap> (with-meta
          (assoc s :$schema  "https://vega.github.io/schema/vega-lite/v5.json")
          {:portal.viewer/default :portal.viewer/vega-lite})))

(defn heading [s]
  (portal-heading [:b s]))

(defmethod view/metrics* :portal
  [{:keys [samples-id]} bench-map]
  (let [samples-id      (or samples-id :samples)
        metrics-samples (-> bench-map :data samples-id)
        metrics-defs    (:metrics-defs metrics-samples)
        metric-configs  (metric/all-metric-configs metrics-defs)]
    (portal-table
     (viewer-common/metrics-map
      (util/metric->values metrics-samples)
      metric-configs))))

(defmethod view/stats* :portal
  [{:keys [stats-id]} bench-map]
  (let [stats-id       (or stats-id :stats)
        stats-map      (-> bench-map :data stats-id)
        metrics-defs   (:metrics-defs stats-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms (:data bench-map) stats-id)]
    (heading "Summary stats")
    (portal-table
     (viewer-common/stats-map
      (util/stats stats-map)
      metric-configs
      transforms))))

(defmethod view/event-stats* :portal
  [{:keys [event-stats-id]} bench-map]
  (let [event-stats-id  (or event-stats-id :event-stats)
        event-stats-map (-> bench-map :data event-stats-id)
        metrics-defs    (have (:metrics-defs event-stats-map))]
    (heading "Event stats")
    (portal-table
     (viewer-common/event-stats
      metrics-defs
      (util/event-stats event-stats-map)))))

(defmethod view/quantiles* :portal
  [{:keys [quantiles-id]} bench-map]
  (let [quantiles-id   (or quantiles-id :quantiles)
        quantiles-map  (-> bench-map :data quantiles-id)
        metrics-defs   (:metrics-defs quantiles-map)
        metric-configs (metric/all-metric-configs metrics-defs)
        transforms     (util/get-transforms (:data bench-map) quantiles-id)]
    (heading "Quantiles")
    (portal-table
     (viewer-common/quantiles
      metric-configs
      (util/quantiles quantiles-map)
      transforms))))

(defmethod view/outlier-counts* :portal
  [{:keys [outliers-id] :as _view} bench-map]
  (let [outliers-id    (or outliers-id :outliers)
        outliers-map   (-> bench-map :data outliers-id)
        metrics-defs   (:metrics-defs outliers-map)
        metric-configs (metric/all-metric-configs metrics-defs)]
    (heading "Outliers")
    (portal-table
     (viewer-common/outlier-counts
      metric-configs
      (util/outliers outliers-map)))))

(defmethod view/outlier-significance* :portal
  [{:keys [outlier-significance-id] :as _view} bench-map]
  (let [outlier-sig-id  (or outlier-significance-id :outlier-significance)
        outlier-sig-map (-> bench-map :data outlier-sig-id)
        outlier-sig     (util/outlier-significance outlier-sig-map)
        metrics-defs    (:metrics-defs outlier-sig-map)
        metric-configs  (metric/all-metric-configs metrics-defs)]
    (heading "Outlier Significance")
    (portal-table
     (vec
      (for [m metric-configs]
        (get-in outlier-sig (:path m)))))))

(defmethod view/collect-plan* :portal
  [_view bench-map]
  (heading "Collect plan")
  (portal-table
   (viewer-common/collect-plan-data bench-map)))

(defn metric-layer
  [metric->values transforms outliers metric]
  {:pre [(have? map? metric->values)]}
  (let [path       (:path metric)
        k          (first path)
        field-name (name k)
        data       (mapv
                    #(let [outlier (get
                                    (:outliers (get-in outliers path))
                                    %2
                                    "")]
                       (assoc
                        {k
                         (util/transform-sample->
                          %1
                          transforms)}
                        :index %2
                        :outlier outlier))
                    (have some?
                          (metric->values path)
                          {:path path :available (keys metric->values)})
                    (range))]
    {:data     {:values data}
     :encoding {:x       {:field "index" :type "quantitative"}
                :y       {:field field-name
                          :type  "quantitative"
                          :scale {:zero false}}
                :tooltip [{:field "index" :type "quantitative"}
                          {:field field-name :type "quantitative"}]
                :color   {:field "outlier"}}
     :mark     "point"}))

(defn metric-histo-layer
  [metric->values transforms outliers metric]
  {:pre [metric->values]}
  (let [path       (:path metric)
        k          (first path)
        field-name (name k)
        data       (mapv
                    #(let [outlier (get
                                    (:outliers (get-in outliers path))
                                    %2
                                    "")]
                       (assoc
                        {k
                         (util/transform-sample->
                          %1
                          transforms)}
                        :index %2
                        :outlier outlier))
                    (metric->values path)
                    (range))]
    {:data     {:values data}
     :encoding {:y     {:aggregate "count"
                        :type      "quantitative"}
                :x     {:field field-name
                        :type  "quantitative"
                        :bin   {:maxbins 100}
                        :scale {:zero false}}
                :color {:field "outlier"}}
     :mark     "bar"}))

(defn normal-pdf-points
  [min-val max-val mean variance transforms]
  (let [sigma (Math/sqrt variance)
        delta (/ (- (double max-val) (double min-val)) 120)
        pdf   (probability/normal-pdf mean sigma)]
    (mapv
     (fn [z]
       {:z (util/transform-sample-> z transforms)
        :p (pdf z)})
     (range min-val max-val delta))))

(defn metric-sample-stats-layer
  [transforms stats metric-config]
  (let [{:keys [mean-minus-3sigma mean-plus-3sigma mean variance]}
        stats
        path (:path metric-config)
        k    (first path)
        data (normal-pdf-points
              mean-minus-3sigma
              mean-plus-3sigma
              mean
              variance
              transforms)]
    [{:resolve {:scale {:y "shared"}}
      :layer
      [{:data     {:values data}
        :encoding {:x       {:field "z"
                             :type  "quantitative"
                             :scale {:zero false}}
                   :y       {:field "p"
                             :type  "quantitative"}
                   :tooltip [{:field (name k)
                              :title (str "Normal")}]}
        :mark     {:type "line"}}
       {:data     {:values [{k
                             (util/transform-sample-> mean transforms)
                             :title "mean"}]}
        :encoding {:x       {:field (name k)
                             :type  "quantitative"
                             :scale {:zero false}}
                   :tooltip [{:field (name k)
                              :title (str "Mean " (name k))}]}
        :mark     "rule"}]}]))

(defn event-occurance [events metrics index]
  (reduce
   (fn [res metric-config]
     (let [path (:path metric-config)
           v    (get (get events path) index)]
       (if (pos? (long v))
         (assoc res (viewer-common/composite-key path) v :index index)
         res)))
   nil
   metrics))

(defn event-layer
  [events [_k metrics]]
  (let [data (->> (map
                   (partial event-occurance events (:values metrics))
                   (range (-> events
                              (get (:path (first (:values metrics))))
                              count)))
                  (filterv some?))]
    (when (seq data)
      [{:data     {:values data}
        :encoding {:x       {:field "index"
                             :type  "quantitative"
                             ;; :title title
                             }
                   :color   {:vvalue "white"}
                   :size    {:value 2},
                   :tooltip (conj
                             (mapv
                              #(hash-map
                                :field (name
                                        (viewer-common/composite-key (:path %)))
                                :type "quantitative"
                                :title (str (:label metrics) " " (:label %)))
                              (:values metrics))
                             {:field "index" :type "quantitative"})}
        :mark     {:type       "rule"
                   :strokeDash [2 2]}}])))

(defmethod view/samples* :portal
  [{:keys [] :as view} bench-map]
  (let [quant-samples-id     (:samples-id view :samples)
        event-samples-id     (:event-samples-id view quant-samples-id)
        outliers-analysis-id (:outliers-id view :outliers)

        quant-samples (-> bench-map :data quant-samples-id)
        event-samples (-> bench-map :data event-samples-id)
        outliers      (-> bench-map :data outliers-analysis-id)

        q-metrics-defs   (-> (:metrics-defs  quant-samples)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
        e-metrics-defs   (-> (:metrics-defs event-samples)
                             (metric/filter-metrics
                              (metric/type-pred :event)))
        metric-configs   (metric/all-metric-configs q-metrics-defs)
        e-metric-configs (metric/all-metric-configs e-metrics-defs)

        transforms (util/get-transforms (:data bench-map) quant-samples-id)]
    (heading "Samples")
    (portal-vega-lite
     {:data     {:values [{}]}
      :encoding {:x {:field "index" :type "quantitative"}}
      :resolve  {:scale {:y "independent"}}
      :vconcat
      (into
       [{:height 800
         :layer
         (vec
          (into
           [(metric-layer
             (util/metric->values quant-samples)
             transforms
             (when outliers (util/outliers outliers))
             (have (first metric-configs)))]
           (mapcat
            #(event-layer (util/metric->values event-samples) %)
            e-metrics-defs)))}]
       (mapv
        #(metric-layer
          (util/metric->values quant-samples)
          transforms
          outliers %)
        e-metric-configs))})))

(defmethod view/histogram* :portal
  [{:keys [samples-id stats-id] :as view} bench-map]
  (let [stats-id            (or stats-id :stats)
        quant-samples-id    (or samples-id :samples)
        outlier-analysis-id (:outlier-id view :outliers)
        quant-samples       (-> bench-map :data quant-samples-id)
        outlier-analysis    (-> bench-map :data outlier-analysis-id)
        stats               (-> bench-map :data stats-id)
        metrics-defs        (-> (:metrics-defs quant-samples)
                                (metric/filter-metrics
                                 (metric/type-pred :quantitative)))
        metric-configs      (metric/all-metric-configs metrics-defs)
        transforms          (util/get-transforms
                             (:data bench-map)
                             quant-samples-id)
        stats-transforms    (util/get-transforms
                             (:data bench-map)
                             (:source-id stats))]
    (heading "Histogram")
    (portal-vega-lite
     {:data    {:values []}
      :resolve {:scale {:x "independent" :y "independent"}}
      :vconcat (mapv
                (fn [metric-config]
                  {:resolve {:scale {:x "shared" :y "independent"}}
                   :height  800
                   :layer
                   (into
                    [(metric-histo-layer
                      (util/metric->values quant-samples)
                      transforms
                      (util/outliers outlier-analysis)
                      metric-config)]
                    (when stats
                      (->>
                       (metric-sample-stats-layer
                        stats-transforms
                        (get-in (util/stats stats) (:path metric-config))
                        metric-config))))})
                metric-configs)})))

(defn metric-percentile-layer
  [matric->values transforms metric]
  (let [path        (:path metric)
        k           (first path)
        field-name  (name k)
        vs          (->> (matric->values path)
                         (map #(util/transform-sample-> % transforms))
                         sort
                         vec)
        n           (count vs)
        max-val     (Math/log10 (double n))
        xs          (mapv
                     #(/ (- max-val (Math/log10 (- n (double %)))) max-val)
                     (range 0 n))
        delta       (/ 100.0 (dec n))
        percentiles (take n
                          (iterate
                           #(+ delta (double %)) 0))
        data        (mapv
                     #(hash-map k %1 :p %2 :x %3)
                     vs
                     percentiles
                     xs)]
    {:data   {:values data
              :name   "vals"}
     :height 800
     :encoding
     {:x
      {:field "x" :type "quantitative"
       :scale {:domain [0 1.0]}
       :axis
       {:labelExpr
        ;; inverse of xs
        (format
         "format( (%d - pow(%d, 1 - min(datum.index, 1.0))) / %d,'.3%%')",
         n,n,(dec n))
        :tickCount 10}}
      :y       {:field field-name
                :type  "quantitative"
                :scale {:zero false
                        :type "log"}}
      :tooltip [{:field "p" :type "quantitative"}
                {:field field-name :type "quantitative"}]}
     :mark   "point"}))

(defmethod view/sample-percentiles* :portal
  [{:keys [metric-ids] :as view} bench-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples    (-> bench-map :data quant-samples-id)
        metrics-defs     (-> (:metrics-defs quant-samples)
                             (metric/filter-metrics
                              (metric/type-pred :quantitative)))
        metric-configs   (metric/all-metric-configs metrics-defs)
        transforms       (util/get-transforms
                          (:data bench-map)
                          quant-samples-id)]
    (heading "Percentiles")
    (portal-vega-lite
     {:data    {:values [{}]} ; for portal
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(metric-percentile-layer
             (util/metric->values quant-samples)
             transforms
             (first metric-configs))]))}])})))

(defn metric-diff-layer
  [samples metric]
  (let [path       (:path metric)
        k          (first path)
        field-name (name k)
        vs         (->> (get samples path)
                        sort
                        vec)
        min-v      (double (first vs))
        diffs      (-> (mapv
                        #(- (double %) min-v)
                        vs)
                       sort
                       distinct
                       vec)
        data       (mapv
                    #(hash-map k %1 :x %2)
                    diffs
                    (range))]
    {:data   {:values data
              :name   "vals"}
     :height 800
     :encoding
     {:x
      {:field "x" :type "quantitative"}
      :y       {:field field-name
                :type  "quantitative"
                :scale {:zero false}}
      :tooltip [{:field field-name :type "quantitative"}]}
     :mark   "point"}))

(defmethod view/sample-diffs* :portal
  [{:keys [] :as view} bench-map]
  (let [quant-samples-id (:samples-id view :samples)
        quant-samples    (-> bench-map :data quant-samples-id)
        metric-configs   (:metric-configs quant-samples)]
    (heading "Sample diffs")
    (portal-vega-lite
     {:data    {:values [{}]} ; for portal
      :resolve {:scale {:y "independent"}}
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(metric-diff-layer
             (util/metric->values quant-samples)
             (first metric-configs))]))}])})))
