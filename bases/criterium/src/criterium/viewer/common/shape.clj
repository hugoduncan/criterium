(ns criterium.viewer.common.shape
  "Shape statistics view helpers.

  Provides functions for formatting and classifying shape statistics
  (skewness, kurtosis, CV) from bootstrap results.")

;;; Shape Statistics Classification

(defn- classify-skewness
  "Classify skewness based on absolute value.
  Uses standard thresholds: |s| > 1 highly skewed, |s| > 0.5 moderately skewed."
  [^double s]
  (let [abs-s (Math/abs s)]
    (cond
      (> abs-s 1.0) (if (neg? s) :highly-left-skewed :highly-right-skewed)
      (> abs-s 0.5) (if (neg? s) :moderately-left-skewed :moderately-right-skewed)
      (> abs-s 0.1) (if (neg? s) :slightly-left-skewed :slightly-right-skewed)
      :else :symmetric)))

(defn- classify-kurtosis
  "Classify excess kurtosis (kurtosis - 3).
  Positive = heavy tails (leptokurtic), negative = light tails (platykurtic)."
  [^double k]
  (let [excess (- k 3.0)]
    (cond
      (> excess 1.0) :heavy-tails
      (< excess -1.0) :light-tails
      :else :normal-tails)))

(defn- classify-cv
  "Classify coefficient of variation.
  CV < 0.1 = low variability, CV > 0.5 = high variability."
  [^double cv]
  (cond
    (< cv 0.1) :low-variability
    (< cv 0.3) :moderate-variability
    :else :high-variability))

(defn shape-stats-data
  "Extract and format shape statistics (skewness, kurtosis, CV) from bootstrap results.
  Returns a vector of maps with :metric :skewness :kurtosis :cv and classification info."
  [metric-configs bootstrap-data]
  (reduce
   (fn [res metric-config]
     (let [path (:path metric-config)
           stat (get-in bootstrap-data path)]
       (if (and stat (:skewness stat) (:kurtosis stat) (:cv stat))
         (let [skew-pe (-> stat :skewness :point-estimate double)
               kurt-pe (-> stat :kurtosis :point-estimate double)
               cv-pe (-> stat :cv :point-estimate double)]
           (conj res
                 {:metric (:label metric-config)
                  :skewness (format "%.4f" skew-pe)
                  :skewness-class (classify-skewness skew-pe)
                  :kurtosis (format "%.4f" kurt-pe)
                  :kurtosis-class (classify-kurtosis kurt-pe)
                  :cv (format "%.4f" cv-pe)
                  :cv-class (classify-cv cv-pe)}))
         res)))
   []
   metric-configs))
