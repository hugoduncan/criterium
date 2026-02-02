(ns criterium.viewer.kindly
  "A viewer that outputs Kindly-annotated data structures for Clay notebooks.

  Uses an accumulator pattern where view functions append Kindly-annotated
  values to an atom. The `flush-viewer` multimethod returns a `kind/fragment`
  combining all accumulated values.

  No runtime dependency on scicloj/kindly - produces plain maps with
  appropriate `:kindly/kind` metadata."
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.domain.types :as domain.types]
   [criterium.view :as view]
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.common-charts.profile :as charts.profile]
   ;; Sub-namespaces - provide specialized views
   [criterium.viewer.kindly.allocation]
   [criterium.viewer.kindly.autocorrelation]
   [criterium.viewer.kindly.core :as core]
   [criterium.viewer.kindly.distribution]
   [criterium.viewer.kindly.domain]
   [criterium.viewer.kindly.modal]
   [criterium.viewer.kindly.shape]
   [criterium.viewer.kindly.tail]))

;;; Re-export from core for backwards compatibility

(def accumulated
  "Accumulator for Kindly-annotated values."
  core/accumulated)

(def last-fragment
  "Last flushed Kindly fragment for retrieval after bench completes."
  core/last-fragment)

(def kindly-add
  "Add a value to the accumulator."
  core/kindly-add)

(def kindly-heading
  "Add a markdown heading to the accumulator."
  core/kindly-heading)

(def kindly-table
  "Add a table to the accumulator.
  Optionally accepts :column-names in opts for explicit column ordering."
  core/kindly-table)

(def kindly-vega-lite
  "Add a Vega-Lite chart to the accumulator."
  core/kindly-vega-lite)

(def kindly-vega
  "Add a full Vega chart to the accumulator."
  core/kindly-vega)

(def flush
  "Return accumulated values as a kind/fragment and clear the accumulator.
  Also stores the fragment in `last-fragment` for retrieval after bench completes."
  core/flush)

;;; Call Tree Views

(defmethod view/call-tree* :kindly
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (kindly-heading
         (clojure.core/format "Call Tree (%d total calls)" total-calls))
        (kindly-vega (charts.profile/call-tree-tree-vega-spec call-tree {}))))))

(defmethod view/call-flame* :kindly
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (kindly-heading "Call Flame Chart")
        (kindly-vega
         (charts.profile/call-tree-flame-vega-spec
          call-tree
          total-calls
          {}))))))

(defmethod view/most-called* :kindly
  [_ {:keys [most-called-id]} data-map]
  (let [most-called-id (or most-called-id :most-called)
        most-called-data (get data-map most-called-id)]
    (when most-called-data
      (let [methods (:most-called most-called-data)
            total-in-list (reduce + 0 (map :total-calls methods))]
        (kindly-heading
         (clojure.core/format "Most Called Methods (top %d, %d total calls)"
                              (count methods) total-in-list))
        (kindly-vega-lite
         (charts.profile/most-called-vega-lite-spec most-called-data {}))))))

;;; Domain Apply View

(defn- resolve-view-fn-without-flush
  "Resolve a view spec to a function without automatic flushing.
  Returns nil and logs a warning if the view-spec cannot be resolved.
  Used by domain-apply to accumulate all run outputs before flushing."
  [view-spec]
  (let [[view-kw opts] (if (sequential? view-spec)
                         [(first view-spec) (second view-spec)]
                         [view-spec {}])
        view-fn-var (ns-resolve 'criterium.view (symbol (name view-kw)))]
    (if view-fn-var
      (view-fn-var (or opts {}))
      (binding [*out* *err*]
        (println
         (format
          "WARNING: Unknown view-spec '%s' - no such view function in criterium.view"
          view-kw))))))

(defmethod view/domain-apply* :kindly
  [viewer {:keys [domain-id view-spec]} data-map]
  (let [domain-id (or domain-id :domain)
        domain (get data-map domain-id)]
    (cond
      (nil? view-spec)
      (binding [*out* *err*]
        (println "WARNING: domain-apply requires :view-spec option"))

      (nil? domain)
      nil

      :else
      ;; Use direct view function resolution to avoid automatic flush
      ;; that benchmark/->view performs after each call.
      ;; For kindly, we want to accumulate all runs' output first.
      (when-let [view-fn (resolve-view-fn-without-flush view-spec)]
        (doseq [{:keys [coord data]} (domain.types/runs domain)]
          (kindly-heading (format "Run: %s" (pr-str coord)))
          (view-fn viewer data))))))
