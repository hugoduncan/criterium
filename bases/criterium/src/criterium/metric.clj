(ns criterium.metric
  "A metric is a value that can be collected.
  It is described by a configuration map.")

(defn metric-configs
  "Return a sequence of metric-config maps for the given metrics-configs map."
  [metrics-configs]
  (mapcat :values (vals metrics-configs)))

(defn all-metric-configs [metrics-configs]
  (reduce-kv
   (fn [res _k metric-group]
     (reduce
      conj
      res
      (or (:values metric-group)
          (mapcat :values (vals (:groups metric-group))))))
   []
   metrics-configs))

(defn- map-filter
  [pred m]
  (select-keys m (for [[k v] m :when (pred v)] k)))

(defn metrics-of-type
  "Return a map of metrics configs of the metric-type for the metric-ids.
  If metric-ids is nil, defaults to return all metrics."
  [metrics-config metric-type metric-ids]
  (->>
   (if metric-ids
     (select-keys metrics-config metric-ids)
     metrics-config)
   (map-filter #(= metric-type (:type %)))))

(defn metric-configs-of-type
  "Return a sequence of metric-configs of the metric-type for the metric-ids.
  If metric-ids is nil, defaults to return all metrics."
  [metrics-configs metric-type metric-ids]
  (->> (metrics-of-type metrics-configs metric-type metric-ids)
       metric-configs
       (filterv #(= metric-type (:type %)))))

;;; Sample Metric Accessors

(defn elapsed-time
  ^long [sample]
  (:elapsed-time sample))
