(ns criterium.collector
  "Metrics collector.

  A metrics collector collects metrics associated with executing a
  measured.

  A metrics collector is a pipeline is a pipeline with two stages. It
  collects metrics into and array with an element for each metric,
  without creating any allocation garbage. The array is then
  transformed into a map, keyed by metric id.

  The `collect-array` function takes a measure, a measured state, and
  an eval count.  It returns an array of sample data.  The array is
  allocated once, and all objects allocated during sampling are recorded
  in the array, in order to make the sample phase garbage free.

  The pipeline `transform` takes the sample array, and returns a sample
  map.  The transform is free to create garbage.

  A pipeline is specified via keywords, which specify sample metrics to
  be collecteds and a pipeline terminal function, which is responsible
  for actually calling the measured.

  Each sample function can collect data before and after the measured's
  execution."
  (:require
   [criterium.collector.impl :as impl]
   [criterium.collector.metrics :as metrics]))

;;; Collector Pipeline Stages

(defn stage?
  [x]
  (and (map? x) (fn? (:m x)) (fn? (:x x)) (keyword? (:id x))))

(defn terminal?
  [x]
  (and (stage? x) (= :terminal (-> x meta ::stage-type))))

(defn maybe-var-get-stage [x]
  (impl/maybe-var-get-stage x))

;;; Collector Pipeline Construction

(defn collector
  "Build a metrics collector pipeline by specifying metric-ids.

  Returns a collector map, containing pipeline phase functions, :f
  and :x, and :metrics-configs keys."
  [collector-config]
  (let [collector-config (impl/maybe-var-get-config collector-config)]
    (-> collector-config
        impl/pipeline*
        (assoc :metrics-configs
               (select-keys (metrics/metrics)
                            (impl/metric-ids collector-config))))))

;;; Collector Pipeline Execution

(defn collect-array
  "Collect a metrics array from measured, returning the array.

  Runs the measured eval-count times.

  Return an array with an element for the data collected by each metric
  in the collector pipeline."
  ^objects [{:keys [f length] :as _collector} measured measured-args eval-count]
  (let [^objects collected (make-array Object length)]
    (f collected measured measured-args eval-count 0)
    collected))

(defn transform
  "Transform the collection array into a metrics data map.
  Return a map with a top level key for each metric in the collector."
  [collector sample]
  ((:x collector) sample 0)
  (reduce merge {} sample))

(defn collect
  "Collect metrics from measured, returning a metrics data map.

  Convenience function."
  [collector measured measured-args eval-count]
  (let [collected (collect-array collector measured measured-args eval-count)]
    (transform collector collected)))
