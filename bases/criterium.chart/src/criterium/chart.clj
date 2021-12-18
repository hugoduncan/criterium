(ns criterium.chart
  "Optional charting.
  Requires oz."
  (:refer-clojure :exclude [spit])
  (:require
   [criterium.chart.vega-lite :as vega-lite]
   [criterium.format :as format]
   [criterium.metric :as metric]
   [criterium.util :as util]))

(defn view [chart]
  (vega-lite/view! chart)
  nil)

(defn spit [chart path]
  (vega-lite/export! chart path))

(defn- histogram-chart
  [field title]
  {
   :layer
   [{:mark     {:type "bar" :tooltip true}
     :encoding {:x {:bin   true
                    :field field
                    :type  "quantitative"
                    :title title}
                :y {:aggregate "count"
                    :type      "quantitative"
                    :title     "count"}}}
    {:mark     {:type "rule" :tooltip true}
     :encoding {:x     {:aggregate "mean"
                        :field     field
                        :type      "quantitative"
                        :format    ".2f"
                        :title     title}
                :color {:value "red"}
                :size  {:value 2}}}]
   })

(defn histogram
  [sampled stats metrics options]
  {:pre [sampled stats metrics]}
  #_(let [samples (:samples sampled)
          _       (assert samples sampled)
          rf      (fn [all metric]
                    (assert metric)
                    (into
                     all
                     (mapv
                      (fn [i metric metric-path]
                        (assert metric)
                        (assert metric-path)
                        (let [{:keys [label dimension]}
                              (metric/metric-format metric-path)
                              mean        (get-in stats
                                                  (conj metric-path :mean))
                              [fact unit] (format/scale dimension mean)
                              tforms      (util/get-transforms
                                           sampled metric-path)
                              vname       (str "v" i)]
                          {:kw          (keyword vname)
                           :field       vname
                           :metric      metric
                           :metric-path metric-path
                           :label       (str label " [" unit "]")
                           :unit        unit
                           :factor      fact
                           :tform       (fn [s]
                                          (-> s
                                              (util/transform-sample-> tforms)
                                              (* fact)))}))
                      (range)
                      (repeat metric)
                      (metric/metric-paths metric))))
          mmp     (reduce rf [] metrics)
          values  (mapv
                   (fn [sample]
                     (reduce
                      (fn [res {:keys [kw metric-path tform]}]
                        (assoc res
                               kw (double (tform (get-in sample metric-path)))))
                      {}
                      mmp))
                   samples)]
      {:data   {:values values}
       :concat (mapv
                (fn [{:keys [field label]}]
                  (histogram-chart field label))
                mmp)}))

(defn- samples-chartlet
  [field title]
  {:layer
   [{:mark     {:type "line" :tooltip true}
     :encoding {:x {:field "index"
                    :type  "quantitative"
                    :title "sample"}
                :y {:field  field
                    :type   "quantitative"
                    :title  title
                    :format ".4f"
                    :scale  {:zero false}}}}
    {:mark     {:type "rule" :tooltip true}
     :encoding {:y     {:aggregate "mean"
                        :field     field
                        :type      "quantitative"
                        :format    ".2f"
                        :title     title}
                :color {:value "red"}
                :size  {:value 2}}}]})

(defn samples-chart
  "Return a vega-lite chart definition for plotting samples."
  [sampled stats metrics options]
  {:pre [sampled stats metrics]}
  #_(let [samples (:samples sampled)
          _       (assert samples sampled)
          index   (volatile! 0)
          rf      (fn [all metric]
                    (assert metric)
                    (into
                     all
                     (mapv
                      (fn [metric metric-path]
                        (assert metric)
                        (assert metric-path)
                        (let [{:keys [label dimension]}
                              (metric/metric-format metric-path)
                              value       (or (get-in stats
                                                      (conj metric-path :mean))
                                              (some
                                               #(let [v (get-in % metric-path)]
                                                  (when-not (zero? v)
                                                    v))
                                               samples)
                                              0)
                              [fact unit] (format/scale dimension value)
                              tforms      (util/get-transforms
                                           sampled metric-path)
                              i           (vswap! index inc)
                              vname       (str "v" i)]
                          {:kw          (keyword vname)
                           :field       vname
                           :metric      metric
                           :metric-path metric-path
                           :label       (str label " [" unit "]")
                           :unit        unit
                           :factor      fact
                           :tform       (fn [s]
                                          (some-> s
                                                  (util/transform-sample-> tforms)
                                                  (* fact)))}))
                      (repeat metric)
                      (metric/metric-paths metric))))
          mmp     (reduce rf [] metrics)
          values  (mapv
                   (fn [sample index]
                     (reduce
                      (fn [res {:keys [kw metric-path tform]}]
                        (assoc res
                               kw (double (tform (get-in sample metric-path)))))
                      {:index index}
                      mmp))
                   samples
                   (range))]
      {:data   {:values values}
       :concat (mapv
                (fn [{:keys [field label]}]
                  (samples-chartlet field label))
                mmp)}))
