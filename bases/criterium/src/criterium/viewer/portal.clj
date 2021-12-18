(ns criterium.viewer.portal
  (:require
   [criterium.metric :as metric]
   [criterium.util.helpers :as util]
   [criterium.util.probability :as probability]
   [criterium.view :as view]
   [criterium.viewer.common :as viewer-common]))

(defn portal-table [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/table})))

(defn portal-vega-lite [s]
  (tap> (with-meta s {:portal.viewer/default :portal.viewer/vega-lite})))

(defmethod view/metrics* :portal
  [{:keys [samples-id metric-ids]} sampled]
  (let [samples-id     (or samples-id :samples)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (portal-table
     (viewer-common/metrics-map
      (sampled samples-id)
      metric-configs))))

(defmethod view/stats* :portal
  [{:keys [stats-id metric-ids]} sampled]
  (let [stats-id       (or stats-id :stats)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (portal-table
     (viewer-common/stats-map (get sampled stats-id) metric-configs))))

(defmethod view/event-stats* :portal
  [{:keys [event-stats-id metric-ids]} sampled]
  (let [event-stats-id  (or event-stats-id :event-stats)
        metrics-configs (metric/metrics-of-type
                         (:metrics-configs sampled)
                         :event metric-ids)]
    (portal-table
     (viewer-common/event-stats metrics-configs (get sampled event-stats-id)))))

(defmethod view/quantiles* :portal
  [{:keys [quantilies-id metric-ids]} sampled]
  (let [quantilies-id  (or quantilies-id :quantiles)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (portal-table
     (viewer-common/quantiles
      metric-configs
      (get sampled quantilies-id)))))

(defmethod view/outlier-counts* :portal
  [{:keys [metric-ids outliers-id] :as _view} sampled]
  (let [outliers-id    (or outliers-id :outliers)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (portal-table
     (viewer-common/outlier-counts
      metric-configs
      (get sampled outliers-id)))))

(defmethod view/outlier-significance* :portal
  [{:keys [metric-ids outlier-significance-id] :as _view} sampled]
  (let [outlier-sig-id (or outlier-significance-id :outlier-significance)
        outlier-sig    (get sampled outlier-sig-id)
        metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)]
    (portal-table
     (vec
      (for [m metric-configs]
        (get-in outlier-sig (:path m)))))))

(defmethod view/collect-plan* :portal
  [_view sampled]
  (portal-table
   (viewer-common/collect-plan-data sampled)))

(defn metric-layer
  [samples transforms outliers metric]
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
                    (get samples path)
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
  [samples transforms outliers metric]
  {:pre [samples]}
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
                    (get samples path)
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
  (let [sigma       (Math/sqrt variance)
        #_#_min-val (* min-val 0.9)
        #_#_sigma   (Math/abs sigma)
        delta       (/ (- max-val min-val) 120)
        pdf         (probability/normal-pdf mean sigma)]
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
              (util/transform->sample mean-minus-3sigma transforms)
              (util/transform->sample mean-plus-3sigma transforms)
              (util/transform->sample mean transforms)
              (util/transform->sample variance transforms)
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
       {:data     {:values [{k mean :title "mean"}]}
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
       (if (pos? v)
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
  [{:keys [metric-ids] :as view} sampled]
  (let [metric-configs (metric/metric-configs-of-type
                        (:metrics-configs sampled)
                        :quantitative metric-ids)
        event-metrics  (metric/metrics-of-type
                        (:metrics-configs sampled)
                        :event metric-ids)

        quant-samples-id (:samples-id view :samples)
        quant-samples    (get sampled quant-samples-id)
        event-samples-id (:event-samples-id view quant-samples-id)
        event-samples    (get sampled event-samples-id)

        outliers-analysis-id (:outliers-id view :outliers)
        outliers             (get sampled outliers-analysis-id)

        transforms (util/get-transforms sampled quant-samples-id)]
    (portal-vega-lite
     {:$schema  "https://vega.github.io/schema/vega-lite/v5.json"
      :data     {:values [{}]}
      :encoding {:x {:field "index" :type "quantitative"}}
      :resolve  {:scale {:y "independent"}}
      :padding  0
      :vconcat
      (into
       [{:height 800
         :layer
         (vec
          (into
           [(metric-layer
             quant-samples transforms outliers (first metric-configs))]
           (mapcat
            #(event-layer event-samples %)
            event-metrics)))}]
       (mapv
        #(metric-layer quant-samples transforms outliers %)
        (rest metric-configs)))})))

(defmethod view/histogram* :portal
  [{:keys [samples-id stats-id metric-ids] :as view} sampled]
  (let [metric-configs      (metric/metric-configs-of-type
                             (:metrics-configs sampled)
                             :quantitative metric-ids)
        quant-samples-id    (or samples-id :samples)
        stats-id            (or stats-id :stats)
        quant-samples       (get sampled quant-samples-id)
        outlier-analysis-id (:outlier-id view :outliers)
        outlier-analysis    (get sampled outlier-analysis-id)
        stats               (get sampled stats-id)
        transforms          (util/get-transforms
                             sampled
                             quant-samples-id)
        stats-transforms    (util/get-transforms
                             sampled
                             (:source-id (meta stats)))]
    (portal-vega-lite
     {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
      :data    {:values [{}]}
      :resolve {:scale {:x "independent" :y "independent"}}
      :padding 0
      :vconcat (mapv
                (fn [metric-config]
                  {:resolve {:scale {:x "shared" :y "independent"}}
                   :height  800
                   :layer
                   (into
                    [(metric-histo-layer
                      quant-samples transforms outlier-analysis metric-config)]
                    (when stats
                      (->>
                       (metric-sample-stats-layer
                        stats-transforms
                        (get-in stats (:path metric-config))
                        metric-config))))})
                metric-configs)})))

(defn metric-percentile-layer
  [samples transforms metric]
  (let [path        (:path metric)
        k           (first path)
        field-name  (name k)
        vs          (->> (get samples path)
                         (map #(util/transform-sample-> % transforms))
                         sort
                         vec)
        n           (count vs)
        max-val     (Math/log10 (double n))
        xs          (mapv
                     #(/ (- max-val (Math/log10 (- n %))) max-val)
                     (range 0 n))
        delta       (/ 100.0 (dec n))
        percentiles (take n (iterate #(+ delta %) 0))
        data        (mapv
                     #(hash-map k %1 :p %2 :x %3)
                     vs
                     percentiles
                     xs)]
    {:data    {:values data
               :name   "vals"}
     :padding 0
     :height  800
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
                :scale {:zero false}}
      :tooltip [{:field "p" :type "quantitative"}
                {:field field-name :type "quantitative"}]}
     :mark    "point"}))

(defmethod view/sample-percentiles* :portal
  [{:keys [metric-ids] :as view} sampled]
  (let [metric-configs   (metric/metric-configs-of-type
                          (:metrics-configs sampled)
                          :quantitative metric-ids)
        quant-samples-id (:samples-id view :samples)
        quant-samples    (get sampled quant-samples-id)
        transforms       (util/get-transforms sampled quant-samples-id)]
    (portal-vega-lite
     {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
      :data    {:values [{}]} ; for portal
      :resolve {:scale {:y "independent"}}
      :padding 0
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(metric-percentile-layer
             quant-samples transforms (first metric-configs))]))}])})))

(defn metric-diff-layer
  [samples metric]
  (let [path       (:path metric)
        k          (first path)
        field-name (name k)
        vs         (->> (get samples path)
                        sort
                        vec)
        min-v      (first vs)
        diffs      (-> (mapv #(- % min-v)  vs)
                       sort
                       distinct
                       vec)
        data       (mapv
                    #(hash-map k %1 :x %2)
                    diffs
                    (range))]
    {:data    {:values data
               :name   "vals"}
     :padding 0
     :height  800
     :encoding
     {:x
      {:field "x" :type "quantitative"}
      :y       {:field field-name
                :type  "quantitative"
                :scale {:zero false}}
      :tooltip [{:field field-name :type "quantitative"}]}
     :mark    "point"}))

(defmethod view/sample-diffs* :portal
  [{:keys [metric-ids] :as view} sampled]
  (let [metric-configs   (metric/metric-configs-of-type
                          (:metrics-configs sampled)
                          :quantitative metric-ids)
        quant-samples-id (:samples-id view :samples)
        quant-samples    (get sampled quant-samples-id)]
    (portal-vega-lite
     {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
      :data    {:values [{}]} ; for portal
      :resolve {:scale {:y "independent"}}
      :padding 0
      :vconcat
      (into
       [{:layer
         (vec
          (into
           [(metric-diff-layer
             quant-samples
             (first metric-configs))]))}])})))
