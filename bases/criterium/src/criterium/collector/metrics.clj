(ns criterium.collector.metrics
  (:require
   [criterium.jvm :as jvm]))

(defn metrics
  "Return the default platform metrics configuration map."
  []
  {:elapsed-time
   {:type   :quantitative
    :values [{:path      [:elapsed-time]
              :dimension :time
              :scale     1e-9
              :type      :quantitative
              :label     "Elapsed Time"}
             {:path  [:expr-value]
              :type  :nominal
              :label "Expr value"}]}
   :memory
   {:type   :quantitative
    :values [{:path      [:memory :heap :used]
              :dimension :memory
              :scale     1
              :type      :quantitative
              :label     "Heap Memory Used"}
             {:path      [:memory :non-heap :used]
              :dimension :memory
              :scale     1
              :type      :quantitative
              :label     "Non-Heap Memory Used"}
             {:path      [:memory :total :used]
              :dimension :memory
              :scale     1
              :type      :quantitative
              :label     "Total Memory Used"}]}
   :thread-allocation
   {:type   :quantitative
    :values [{:path      [:thread-allocation]
              :dimension :memory
              :scale     1
              :type      :quantitative
              :label     "Thread allocated memory"}]}
   :class-loader
   {:type    :event
    :label   "ClassLoader"
    :summary "%32s: loaded %s and unloaded %s classes in %s samples"
    :values  [{:path      [:class-loader :loaded-count]
               :dimension :count
               :scale     1
               :label     "Num loaded classes"}
              {:path      [:class-loader :unloaded-count]
               :dimension :count
               :scale     1
               :label     "Num unloaded classes"}]}
   :compilation
   {:type    :event
    :label   "JIT compilation"
    :summary "%32s: ran for %s in %s samples"
    :values  [{:path      [:compilation :time-ms]
               :dimension :time
               :scale     1e-3
               :label     "time"}]}
   :garbage-collector
   {:type   :event
    :label  "Garbage Collector"
    :groups ; the groups map has the layout of the top level map
    (into
     {}
     (mapv
      (fn [n k]
        [k (hash-map
            :label n
            :summary "%32s: ran %s times for a total of %s in %s samples"
            :values
            [{:path      [:garbage-collector k :time-ms]
              :dimension :time
              :scale     1e-3
              :label     (str n  " time")
              :group     k}
             {:path      [:garbage-collector k :count]
              :dimension :count
              :scale     1
              :label     (str n " count")
              :group     k}])])
      (jvm/garbage-collector-names)
      (jvm/garbage-collector-keywords)))}})
