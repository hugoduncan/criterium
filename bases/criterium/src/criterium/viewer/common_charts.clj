(ns criterium.viewer.common-charts
  "Common Vega-Lite chart generation functions for viewers.

  Provides reusable chart-building functions extracted from the Portal viewer
  for generating histograms, scatter plots, and percentile charts."
  (:require
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.probability :as probability]
   [criterium.viewer.common :as viewer-common]))

;;; Scatter plots

(defn metric-layer
  "Build a scatter plot layer showing samples with outlier coloring.

  Returns a Vega-Lite layer spec."
  [metric->values transforms outliers metric]
  {:pre [(have? map? metric->values)]}
  (let [path (:path metric)
        k (first path)
        field-name (name k)
        data (mapv
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
    {:data {:values data}
     :encoding {:x {:field "index" :type "quantitative"}
                :y {:field field-name
                    :type "quantitative"
                    :scale {:zero false}}
                :tooltip [{:field "index" :type "quantitative"}
                          {:field field-name :type "quantitative"}]
                :color {:field "outlier"
                        :legend {:orient "top-left" :offset 10}}}
     :mark "point"}))

;;; Histograms

(defn metric-computed-histo-layer
  "Build a histogram bar chart layer from pre-computed histogram data.

  Returns a Vega-Lite layer spec for displaying histogram bins."
  [transforms histogram metric _layer-num]
  (let [path (:path metric)
        k (first path)
        field-name (name k)
        {:keys [counts centers widths width density]}
        histogram

        tform #(util/transform-sample-> % transforms)
        centers (mapv tform centers)
        widths (when widths (mapv tform widths))
        width (when width (tform width))
        data (if widths
                  ;; Variable width histogram
               (mapv (fn [_count ^double center ^double width density]
                       (let [half-w (* 0.95 (/ width 2.0))]
                         {field-name (- center half-w)
                          "end" (+ center half-w)
                          "density" density}))
                     counts centers widths density)
                  ;; Fixed width histogram
               (mapv (fn [_count ^double center ^double density]
                       (let [half-w (* 0.95 (/ (double width) 2.0))]
                         {field-name (- center half-w)
                          "end" (+ center half-w)
                          "density" density}))
                     counts centers density))]
    {:data {:values data}
     :transform [{:calculate (str "'" "Histogram " (:label metric) "'") :as "layer"}]
     :encoding {:y2 {:datum 0
                     :type "quantitative"}
                :y {:field "density"
                    :type "quantitative"}
                :x {:field field-name
                    :type "quantitative"
                    :scale {:zero false}}
                :x2 {:field "end"
                     :type "quantitative"}
                :color {:field "layer" :type "nominal"
                        :legend {:orient "top-left" :offset 10}}}
     :mark {:type "bar"
            :binSpacing 0}}))

;;; Normal distribution overlay

(defn normal-pdf-points
  "Generate points for a normal probability density function curve.

  Returns a vector of {:z value :p probability} maps."
  [min-val max-val mean variance transforms]
  (let [sigma (Math/sqrt variance)
        delta (/ (- (double max-val) (double min-val)) 120)
        pdf (probability/normal-pdf mean sigma)]
    (mapv
     (fn [z]
       {:z (util/transform-sample-> z transforms)
        :p (pdf z)})
     (range min-val max-val delta))))

(defn metric-sample-stats-layer
  "Build a normal distribution overlay layer for histogram charts.

  Returns a vector of Vega-Lite layer specs showing the fitted normal
  distribution and mean line."
  [transforms stats metric-config _layer-num]
  (let [{:keys [mean-minus-3sigma mean-plus-3sigma mean variance]}
        stats
        path (:path metric-config)
        k (first path)
        data (normal-pdf-points
              mean-minus-3sigma
              mean-plus-3sigma
              mean
              variance
              transforms)]
    [{:resolve {:scale {:y "shared"}}
      :layer
      [{:data {:values data}
        :transform [{:calculate
                     (str "'" "LogNormal fit " (:label metric-config) "'")
                     :as "layer"}]
        :encoding {:x {:field "z"
                       :type "quantitative"
                       :scale {:zero false}}
                   :y {:field "p"
                       :type "quantitative"}
                   :tooltip [{:field (name k)
                              :title "Normal"}]
                   :color {:field "layer" :type "nominal"
                           :legend {:orient "top-left" :offset 10}}}
        :mark {:type "line"}}
       {:data {:values [{k
                         (util/transform-sample-> mean transforms)
                         :title "mean"}]}
        :encoding {:x {:field (name k)
                       :type "quantitative"
                       :scale {:zero false}}
                   :tooltip [{:field (name k)
                              :title (str "Mean " (name k))}]}
        :mark "rule"}]}]))

;;; Event markers

(defn event-occurrence
  "Extract event occurrence data for a single sample index.

  Returns nil if no events occurred, otherwise a map of metric keys to values."
  [events metrics index]
  (reduce
   (fn [res metric-config]
     (let [path (:path metric-config)
           v (get (get events path) index)]
       (if (pos? (long v))
         (assoc res (viewer-common/composite-key path) v :index index)
         res)))
   nil
   metrics))

(defn event-layer
  "Build an event marker layer showing when events occurred during sampling.

  Returns a vector containing the Vega-Lite layer spec, or nil if no events."
  [events [_k metrics]]
  (let [data (->> (map
                   (partial event-occurrence events (:values metrics))
                   (range (-> events
                              (get (:path (first (:values metrics))))
                              count)))
                  (filterv some?))]
    (when (seq data)
      [{:data {:values data}
        :encoding {:x {:field "index"
                       :type "quantitative"}
                   :color {:vvalue "white"}
                   :size {:value 2},
                   :tooltip (conj
                             (mapv
                              #(hash-map
                                :field (name
                                        (viewer-common/composite-key (:path %)))
                                :type "quantitative"
                                :title (str (:label metrics) " " (:label %)))
                              (:values metrics))
                             {:field "index" :type "quantitative"})}
        :mark {:type "rule"
               :strokeDash [2 2]}}])))

;;; Percentile charts

(defn metric-percentile-layer
  "Build a percentile distribution chart layer.

  Uses a logarithmic scale on the x-axis to emphasize tail percentiles.
  Returns a Vega-Lite layer spec."
  [metric->values transforms metric]
  (let [path (:path metric)
        k (first path)
        field-name (name k)
        vs (->> (metric->values path)
                (map #(util/transform-sample-> % transforms))
                sort
                vec)
        n (count vs)
        max-val (Math/log10 (double n))
        xs (mapv
            #(/ (- max-val (Math/log10 (- n (double %)))) max-val)
            (range 0 n))
        delta (/ 100.0 (dec n))
        percentiles (take n
                          (iterate
                           #(+ delta (double %)) 0))
        data (mapv
              #(hash-map k %1 :p %2 :x %3)
              vs
              percentiles
              xs)]
    {:data {:values data
            :name "vals"}

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
      :y {:field field-name
          :type "quantitative"
          :scale {:zero false
                  :type "log"}}
      :tooltip [{:field "p" :type "quantitative"}
                {:field field-name :type "quantitative"}]}
     :mark "point"}))

;;; Sample diffs

(defn metric-diff-layer
  "Build a sample differences chart layer.

  Shows sorted unique differences from minimum value.
  Returns a Vega-Lite layer spec."
  [samples metric]
  (let [path (:path metric)
        k (first path)
        field-name (name k)
        vs (->> (get samples path)
                sort
                vec)
        min-v (double (first vs))
        diffs (-> (mapv
                   #(- (double %) min-v)
                   vs)
                  sort
                  distinct
                  vec)
        data (mapv
              #(hash-map k %1 :x %2)
              diffs
              (range))]
    {:data {:values data
            :name "vals"}

     :encoding
     {:x
      {:field "x" :type "quantitative"}
      :y {:field field-name
          :type "quantitative"
          :scale {:zero false}}
      :tooltip [{:field field-name :type "quantitative"}]}
     :mark "point"}))

;;; Regression helpers

(defn regression-model-fn
  "Return a function that applies the model transform for plotting."
  [model-id {:keys [a b c]}]
  (case model-id
    :logarithmic (fn [x] (+ (* a (Math/log x)) b))
    :linear (fn [x] (+ (* a x) b))
    :n-log-n (fn [x] (+ (* a x (Math/log x)) b))
    :nlogn-linear (fn [x] (+ (* a x (Math/log x)) (* b x) c))
    :quadratic (fn [x] (+ (* a x x) b))
    (fn [x] (+ (* a x) b))))

(defn regression-equation-str
  "Format the fitted regression equation for a model."
  [model-id {:keys [a b c]}]
  (if c
    ;; Composite model: y = a*f1(x) + b*f2(x) + c
    (let [sign-b (if (neg? b) "-" "+")
          sign-c (if (neg? c) "-" "+")]
      (format "y = %.4g*n*log(n) %s %.4g*n %s %.4g"
              a sign-b (Math/abs ^double b) sign-c (Math/abs ^double c)))
    ;; Simple model: y = a*f(x) + b
    (when (and a b)
      (let [transform-str (case model-id
                            :logarithmic "log(n)"
                            :linear "n"
                            :n-log-n "n*log(n)"
                            :quadratic "n²"
                            "x")
            sign (if (neg? b) "-" "+")]
        (format
         "y = %.4g*%s %s %.4g"
         a
         transform-str
         sign
         (Math/abs ^double b))))))

