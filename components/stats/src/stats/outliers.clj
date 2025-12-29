(ns stats.outliers
  "Outlier detection using boxplot thresholds.")

(defn boxplot-outlier-thresholds
  "Outlier thresholds for given quartiles.
  Returns [severe-low mild-low mild-high severe-high]."
  [^double q1 ^double q3]
  {:pre [(number? q1) (number? q3)]}
  (let [iqr    (- q3 q1)
        severe (* iqr 3.0)
        mild   (* iqr 1.5)]
    [(- q1 severe)
     (- q1 mild)
     (+ q3 mild)
     (+ q3 severe)]))
