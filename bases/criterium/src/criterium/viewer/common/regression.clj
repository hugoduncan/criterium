(ns criterium.viewer.common.regression
  "Domain regression data preparation functions.

  Provides functions to prepare regression model data for table and chart
  rendering, including model fit data, log-log analysis, and an orchestration
  function for rendering regression views."
  (:require
   [criterium.viewer.common.core :as core]))

;;; Model table preparation

(defn prepare-regression-model-table
  "Prepare model table rows for single-impl regression display.
  Returns vector of row maps
  with :model :r-squared :aic :bic :equation :best-fit keys.
  AIC and BIC values may be nil when insufficient data points.
  Options:
    :best-fit-marker - string to show for best fit (default \"✓\")
    :plotted-marker - string to show for plotted but not best (default \"\")

    :tolerance - fraction within best r-squared to mark as
                 plotted (default 0.01)"
  [{:keys [models best-fit]}
   {:keys [best-fit-marker plotted-marker ^double tolerance]
    :or {best-fit-marker "✓" plotted-marker "" tolerance 0.01}}]
  (when (seq models)
    (let [best-r-squared (->> models
                              (filter #(= (:id %) best-fit))
                              first
                              :r-squared
                              double)
          plotted-ids (when best-r-squared
                        (->> models
                             (filter #(>= (double (:r-squared %))
                                          (* best-r-squared (- 1 tolerance))))
                             (map :id)
                             set))
          sorted-models (sort-by :r-squared > models)]
      (mapv (fn [{:keys [id label equation-str r-squared aic bic]}]
              (let [plotted? (and plotted-ids (plotted-ids id))]
                {:model label
                 :r-squared (format "%.4f" r-squared)
                 :aic (when aic (format "%.1f" aic))
                 :bic (when bic (format "%.1f" bic))
                 :equation (or equation-str "")
                 :best-fit (cond
                             (= id best-fit) best-fit-marker
                             plotted? plotted-marker
                             :else "")}))
            sorted-models))))

(defn prepare-regression-model-table-multi-impl
  "Prepare model table rows for multi-impl regression display.
  Returns vector of row maps
  with :implementation :model :r-squared :aic :bic :equation :best-fit.

  Options same as prepare-regression-model-table."
  [by-impl impl-keys options]
  (vec
   (mapcat
    (fn [impl-key]
      (let [impl-data (get by-impl impl-key)
            rows (prepare-regression-model-table impl-data options)]
        (mapv #(assoc % :implementation (name impl-key)) rows)))
    impl-keys)))

;;; Scatter plot data preparation

(defn prepare-regression-points
  "Prepare data points for regression scatter plot.
  Returns {:points [...] :total-scale number :unit string :x-vals [...]} or nil.
  Points have keys: x, y, and optionally yLower, yUpper for error bounds.
  For multi-impl mode, points also have :impl key."
  [extract-data {:keys [axis impl-axis has-error-bounds? metric]}]
  (when extract-data
    (let [{:keys [data]} extract-data
          get-value (if has-error-bounds?
                      (fn [[_ v]] (when v (:value v)))
                      (fn [[_ v]] v))
          multi-impl? (some? impl-axis)
          valid-data (filterv (fn [datum]
                                (let [[coord _] datum
                                      value (get-value datum)]
                                  (and (some? value)
                                       (map? coord)
                                       (contains? coord axis)
                                       (or (not multi-impl?)
                                           (contains? coord impl-axis)))))
                              data)]
      (when (seq valid-data)
        (let [raw-values (mapv get-value valid-data)
              {:keys [^double total-scale unit]}
              (core/compute-si-scaling metric raw-values)
              points (mapv (fn [[coord v]]
                             (let [y-val (double
                                          (if has-error-bounds?
                                            (:value v)
                                            v))
                                   x-val (double (get coord axis))]
                               (cond-> {"x" x-val
                                        "y" (* y-val total-scale)}
                                 has-error-bounds?
                                 (assoc "yLower" (* (double (:lower v))
                                                    total-scale)
                                        "yUpper" (* (double (:upper v))
                                                    total-scale))
                                 multi-impl?
                                 (assoc "impl" (name (get coord impl-axis))))))
                           valid-data)
              x-vals (mapv #(get % "x") points)]
          {:points points
           :total-scale total-scale
           :unit unit
           :x-vals x-vals
           :valid-data valid-data})))))

(defn prepare-regression-fit-lines
  "Generate fit line points for plotting.
  For single-impl mode, models is a seq of model maps.
  For multi-impl mode, by-impl is a map of impl-key ->
  {:models [...] :best-fit id}.
  Returns vector of point maps with x, y, and model or impl key."
  [{:keys [x-vals ^double total-scale]} {:keys [models by-impl impl-keys]}]
  (when (seq x-vals)
    (let [x-min (double (reduce min x-vals))
          x-max (double (reduce max x-vals))
          x-range (range x-min (+ x-max 1) (/ (- x-max x-min) 50))]
      (if by-impl
        ;; Multi-impl: one best-fit line per implementation
        (vec
         (mapcat
          (fn [impl-key]
            (let [{:keys [models best-fit]} (get by-impl impl-key)
                  best-model (first (filter #(= (:id %) best-fit) models))]
              (when best-model
                (let [mfn (:predict-fn best-model)]
                  (mapv (fn [x]
                          {"x" x
                           "y" (* (double (mfn x)) total-scale)
                           "impl" (name impl-key)})
                        x-range)))))
          impl-keys))
        ;; Single-impl: lines for all models to plot
        (vec
         (mapcat
          (fn [model]
            (let [mfn (:predict-fn model)]
              (mapv (fn [x]
                      {"x" x
                       "y" (* (double (mfn x)) total-scale)
                       "model" (:label model)})
                    x-range)))
          models))))))

(defn prepare-regression-residuals
  "Compute residual points for plotting.
  Returns vector of point maps with x, residual, and model or impl key."
  [{:keys [valid-data ^double total-scale]}
   {:keys [axis impl-axis has-error-bounds?
           models by-impl impl-keys]}]
  (let [get-value (if has-error-bounds?
                    (fn [[_ v]] (when v (:value v)))
                    (fn [[_ v]] v))]
    (if by-impl
      ;; Multi-impl mode
      (vec
       (mapcat
        (fn [impl-key]
          (let [{:keys [models best-fit]} (get by-impl impl-key)
                best-model (first (filter #(= (:id %) best-fit) models))]
            (when best-model
              (let [mfn (:predict-fn best-model)]
                (keep (fn [[coord v]]
                        (when (= (get coord impl-axis) impl-key)
                          (let [y-val (double (get-value [coord v]))
                                x-val (double (get coord axis))
                                predicted (double (mfn x-val))]
                            {"x" x-val
                             "residual" (* (- y-val predicted) total-scale)
                             "impl" (name impl-key)})))
                      valid-data)))))
        impl-keys))
      ;; Single-impl mode
      (vec
       (mapcat
        (fn [model]
          (let [mfn (:predict-fn model)]
            (mapv (fn [[coord v]]
                    (let [y-val (double (get-value [coord v]))
                          x-val (double (get coord axis))
                          predicted (double (mfn x-val))]
                      {"x" x-val
                       "residual" (* (- y-val predicted) total-scale)
                       "model" (:label model)}))
                  valid-data)))
        models)))))

;;; Log-Log Regression view helpers

(defn prepare-log-log-points
  "Prepare log-log transformed data points for scatter plot.
  Returns {:points [...] :axis-name string} or nil.

  Points have keys: x (log(n)), y (log(metric)), and optionally
  yLower, yUpper for log-transformed error bounds.
  For multi-impl mode, points also have :impl key.

  The log-log-data comes from the :regressions map of a
  :criterium/domain-log-log-regression result."
  [log-log-data {:keys [axis _impl-axis]}]
  (when log-log-data
    (let [multi-impl? (contains? log-log-data :by-impl)]
      (if multi-impl?
        ;; Multi-implementation mode
        (let [by-impl (:by-impl log-log-data)
              impl-keys (keys by-impl)
              all-points
              (vec
               (mapcat
                (fn [impl-key]
                  (let [{:keys [xs ys log-xs log-ys log-lowers log-uppers]}
                        (get by-impl impl-key)]
                    (when (and log-xs log-ys)
                      (map-indexed
                       (fn [i log-x]
                         (let [log-y (nth log-ys i)
                               orig-x (when xs (nth xs i nil))
                               orig-y (when ys (nth ys i nil))]
                           (cond-> {"x" log-x
                                    "y" log-y
                                    "impl" (name impl-key)}
                             orig-x (assoc "origX" orig-x)
                             orig-y (assoc "origY" orig-y)
                             (and log-lowers (nth log-lowers i nil))
                             (assoc "yLower" (nth log-lowers i))
                             (and log-uppers (nth log-uppers i nil))
                             (assoc "yUpper" (nth log-uppers i)))))
                       log-xs))))
                impl-keys))]
          (when (seq all-points)
            {:points all-points
             :axis-name (name axis)
             :has-error-bounds? (boolean
                                 (some #(contains? % "yLower") all-points))}))
        ;; Single implementation mode
        (let [{:keys [xs ys log-xs log-ys log-lowers log-uppers]} log-log-data]
          (when (and log-xs log-ys)
            (let [points (vec
                          (map-indexed
                           (fn [i log-x]
                             (let [log-y (nth log-ys i)
                                   orig-x (when xs (nth xs i nil))
                                   orig-y (when ys (nth ys i nil))]
                               (cond-> {"x" log-x
                                        "y" log-y}
                                 orig-x (assoc "origX" orig-x)
                                 orig-y (assoc "origY" orig-y)
                                 (and log-lowers (nth log-lowers i nil))
                                 (assoc "yLower" (nth log-lowers i))
                                 (and log-uppers (nth log-uppers i nil))
                                 (assoc "yUpper" (nth log-uppers i)))))
                           log-xs))]
              {:points points
               :axis-name (name axis)
               :has-error-bounds? (boolean
                                   (some
                                    #(contains? % "yLower")
                                    points))})))))))

(defn prepare-log-log-fit-line
  "Generate fit line points for log-log plot.
  Returns vector of point maps with x, y, and optionally impl key.

  Uses the pre-computed slope and intercept: y = slope * x + intercept
  where x = log(n), y = log(metric)."
  [log-log-data {:keys [_axis _impl-axis]}]
  (when log-log-data
    (let [multi-impl? (contains? log-log-data :by-impl)]
      (if multi-impl?
        ;; Multi-implementation mode: one line per impl
        (let [by-impl (:by-impl log-log-data)]
          (vec
           (mapcat
            (fn [[impl-key {:keys [slope intercept log-xs]}]]
              (when (and slope intercept log-xs (seq log-xs))
                (let [x-min (double (reduce min log-xs))
                      x-max (double (reduce max log-xs))
                      x-range (range
                               x-min
                               (+ x-max 0.1)
                               (/ (- x-max x-min) 50))]
                  (mapv (fn [^double x]
                          {"x" x
                           "y" (+ (* (double slope) x) (double intercept))
                           "impl" (name impl-key)})
                        x-range))))
            by-impl)))
        ;; Single implementation mode
        (let [{:keys [slope intercept log-xs]} log-log-data]
          (when (and slope intercept log-xs (seq log-xs))
            (let [x-min (double (reduce min log-xs))
                  x-max (double (reduce max log-xs))
                  x-range (range x-min (+ x-max 0.1) (/ (- x-max x-min) 50))]
              (vec
               (mapv (fn [^double x]
                       {"x" x
                        "y" (+ (* (double slope) x) (double intercept))})
                     x-range)))))))))

(defn prepare-log-log-residuals
  "Compute residual points for log-log plot.
  Returns vector of point maps with x (log(n)), residual, and optionally impl.

  Residuals are in log space: residual = log(y) - (slope * log(x) + intercept)"
  [log-log-data {:keys [_axis _impl-axis]}]
  (when log-log-data
    (let [multi-impl? (contains? log-log-data :by-impl)]
      (if multi-impl?
        ;; Multi-implementation mode
        (let [by-impl (:by-impl log-log-data)]
          (vec
           (mapcat
            (fn [[impl-key {:keys [log-xs residuals]}]]
              (when (and log-xs residuals)
                (map-indexed
                 (fn [i log-x]
                   {"x" log-x
                    "residual" (nth residuals i)
                    "impl" (name impl-key)})
                 log-xs)))
            by-impl)))
        ;; Single implementation mode
        (let [{:keys [log-xs residuals]} log-log-data]
          (when (and log-xs residuals)
            (vec
             (map-indexed
              (fn [i log-x]
                {"x" log-x
                 "residual" (nth residuals i)})
              log-xs))))))))

(defn format-log-log-slope
  "Format log-log slope as complexity class estimate.
  Returns string like 'O(n^1.02)' or 'O(n)' for integer slopes."
  [^double slope]
  (let [rounded (Math/round slope)
        ;; Use 0.05 threshold (5% tolerance) for integer rounding. This is
        ;; generous enough to handle typical measurement noise while avoiding
        ;; false simplifications. A slope of 1.94 displays as O(n²) but 1.90
        ;; shows the precise O(n^1.90). Chosen empirically to balance
        ;; readability with accuracy for common complexity classes.
        integer-tolerance 0.05]
    (if (< (Math/abs (- slope rounded)) integer-tolerance)
      ;; Close to integer - use simplified form
      (case rounded
        0 "O(1)"
        1 "O(n)"
        2 "O(n²)"
        3 "O(n³)"
        (format "O(n^%d)" rounded))
      ;; Show decimal
      (format "O(n^%.2f)" slope))))

;;; Domain regression rendering orchestration

(defn with-domain-regression-data
  "Orchestrate domain-regression rendering by iterating over metrics and
  implementations, calling the provided render functions at appropriate points.

  The opts map must contain:
    :regression-id - key for regression in data-map (default :regression)
    :extract-id    - key for extract in data-map (default :extract)
    :log-log-id    - key for log-log in data-map (default :log-log)
    :tolerance     - tolerance for selecting plotted models (default 0.01)
    :table-options - options for prepare-regression-model-table functions

  The handlers map must contain render functions:
    :render-log-log-charts     - (fn [{:keys
                                       [title axis metric impl-axis points
                                       line-pts residual-pts chart-opts]}] ...)
    :render-model-heading      - (fn [{:keys
                                       [title axis metric impl-axis]}] ...)
    :render-model-table        - (fn [{:keys [table-rows multi-impl?]}] ...)
    :render-regression-charts  - (fn [{:keys [points line-pts residual-pts
                                             y-title residual-title axis
                                             chart-opts multi-impl?]}] ...)

  For multi-impl mode, the impl-key is included in callback maps.
  For single-impl mode, slope/r-squared from log-log are included for display."
  [data-map opts handlers]
  (let [{:keys [regression-id extract-id log-log-id tolerance table-options]}
        opts
        regression-id (or regression-id :regression)
        regression (data-map regression-id)
        log-log-id (or log-log-id :log-log)
        log-log (data-map log-log-id)
        tolerance (double (or tolerance 0.01))
        extract-id (or extract-id :extract)
        extract (data-map extract-id)

        {:keys [render-log-log-charts render-model-heading
                render-model-table render-regression-charts]}
        handlers]

    (when regression
      (let [{:keys [axis regressions impl-axis implementations]} regression
            multi-impl? (> (count implementations) 1)]

        (if multi-impl?
          ;; Multi-implementation mode
          (doseq [[metric-id {:keys [metric by-impl with-error-bounds]}]
                  regressions]
            (let [metric-extract-data (get-in extract [:metrics metric-id])
                  impl-keys (sort (keys by-impl))
                  log-log-data (get-in log-log [:regressions metric-id])]

              ;; Log-log diagnostic charts (before model fit)
              (when log-log-data
                (when-let [point-data (prepare-log-log-points
                                       log-log-data
                                       {:axis axis :impl-axis impl-axis})]
                  (let [{:keys [points has-error-bounds?]} point-data
                        line-pts (prepare-log-log-fit-line
                                  log-log-data
                                  {:axis axis :impl-axis impl-axis})
                        residual-pts (prepare-log-log-residuals
                                      log-log-data
                                      {:axis axis :impl-axis impl-axis})]
                    (when (seq points)
                      (render-log-log-charts
                       {:title (str "Log-Log Diagnostic (axis: " (name axis)
                                    ", metric: " (pr-str metric)
                                    ", by: " (name impl-axis) ")")
                        :axis axis
                        :metric metric
                        :impl-axis impl-axis
                        :points points
                        :line-pts line-pts
                        :residual-pts residual-pts
                        :chart-opts {:axis-name (name axis)
                                     :color-field "impl"
                                     :has-error-bounds? has-error-bounds?}})))))

              ;; Model heading
              (render-model-heading
               {:title (str "Domain Regression (axis: " (name axis)
                            ", metric: " (pr-str metric)
                            ", by: " (name impl-axis) ")")
                :axis axis
                :metric metric
                :impl-axis impl-axis})

              ;; Model table
              (render-model-table
               {:table-rows (when (seq by-impl)
                              (prepare-regression-model-table-multi-impl
                               by-impl impl-keys table-options))
                :multi-impl? true})

              ;; Regression charts
              (when-let [point-data (prepare-regression-points
                                     metric-extract-data
                                     {:axis axis
                                      :impl-axis impl-axis
                                      :has-error-bounds? with-error-bounds
                                      :metric metric})]
                (let [{:keys [points unit]} point-data
                      line-pts (prepare-regression-fit-lines
                                point-data
                                {:by-impl by-impl :impl-keys impl-keys})
                      y-title (if (seq unit)
                                (str (pr-str metric) " (" unit ")")
                                (pr-str metric))
                      residual-title (if (seq unit)
                                       (str "Residual (" unit ")")
                                       "Residual")]
                  (when (seq points)
                    (let [residual-pts (prepare-regression-residuals
                                        point-data
                                        {:axis axis
                                         :impl-axis impl-axis
                                         :has-error-bounds? with-error-bounds
                                         :by-impl by-impl
                                         :impl-keys impl-keys})]
                      (render-regression-charts
                       {:points points
                        :line-pts line-pts
                        :residual-pts residual-pts
                        :y-title y-title
                        :residual-title residual-title
                        :axis axis
                        :chart-opts {:axis-name (name axis)
                                     :color-field "impl"
                                     :has-error-bounds? with-error-bounds}
                        :multi-impl? true})))))))

          ;; Single-implementation mode
          (doseq [[metric-id {:keys [metric models best-fit with-error-bounds]}]
                  regressions]
            (let [metric-extract-data (get-in extract [:metrics metric-id])
                  log-log-data (get-in log-log [:regressions metric-id])
                  best-r-squared (when best-fit
                                   (->> models
                                        (filter #(= (:id %) best-fit))
                                        first
                                        :r-squared))
                  models-to-plot (when best-r-squared
                                   (->> models
                                        (filter
                                         #(>= (double (:r-squared %))
                                              (* (double best-r-squared)
                                                 (- 1.0 tolerance))))
                                        (sort-by :r-squared >)))]

              ;; Log-log diagnostic charts (before model fit)
              (when log-log-data
                (when-let [point-data (prepare-log-log-points
                                       log-log-data {:axis axis})]
                  (let [{:keys [points has-error-bounds?]} point-data
                        line-pts (prepare-log-log-fit-line
                                  log-log-data {:axis axis})
                        {:keys [slope r-squared]} log-log-data
                        residual-pts (prepare-log-log-residuals
                                      log-log-data {:axis axis})]
                    (when (seq points)
                      (render-log-log-charts
                       {:title (str "Log-Log Diagnostic (axis: " (name axis)
                                    ", metric: " (pr-str metric) ")")
                        :axis axis
                        :metric metric
                        :points points
                        :line-pts line-pts
                        :residual-pts residual-pts
                        :slope slope
                        :r-squared r-squared
                        :chart-opts {:axis-name (name axis)
                                     :has-error-bounds? has-error-bounds?
                                     :slope slope
                                     :r-squared r-squared}})))))

              ;; Model heading
              (render-model-heading
               {:title (str "Domain Regression (axis: " (name axis)
                            ", metric: " (pr-str metric) ")")
                :axis axis
                :metric metric})

              ;; Model table
              (render-model-table
               {:table-rows (when (seq models)
                              (prepare-regression-model-table
                               {:models models :best-fit best-fit}
                               table-options))
                :multi-impl? false})

              ;; Regression charts
              (when (and metric-extract-data (seq models-to-plot))
                (when-let [point-data (prepare-regression-points
                                       metric-extract-data
                                       {:axis axis
                                        :has-error-bounds? with-error-bounds
                                        :metric metric})]
                  (let [{:keys [points unit]} point-data
                        line-pts (prepare-regression-fit-lines
                                  point-data {:models models-to-plot})
                        y-title (if (seq unit)
                                  (str (pr-str metric) " (" unit ")")
                                  (pr-str metric))
                        residual-title (if (seq unit)
                                         (str "Residual (" unit ")")
                                         "Residual")]
                    (when (seq points)
                      (let [residual-pts (prepare-regression-residuals
                                          point-data
                                          {:axis axis
                                           :has-error-bounds? with-error-bounds
                                           :models models-to-plot})]
                        (render-regression-charts
                         {:points points
                          :line-pts line-pts
                          :residual-pts residual-pts
                          :y-title y-title
                          :residual-title residual-title
                          :axis axis
                          :chart-opts {:axis-name (name axis)
                                       :color-field "model"
                                       :has-error-bounds? with-error-bounds}
                          :multi-impl? false})))))))))))))
