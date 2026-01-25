(ns criterium.viewer.portal
  "A viewer that outputs to portal using tap>.

  Core functionality (tap infrastructure, metrics, stats, extremes, bootstrap,
  samples, outliers, events, KDE) is in criterium.viewer.portal.core.

  Domain analysis views (grouped, extract, comparison, regression, apply)
  are in criterium.viewer.portal.domain.

  Allocation profiling views (summary, hotspots, by-type, treemap)
  are in criterium.viewer.portal.allocation.

  Distribution fit views (models, parameter CIs, PDF, CDF, Q-Q charts)
  are in criterium.viewer.portal.distribution.

  Tail analysis views (summary, ratios, high quantiles, charts)
  are in criterium.viewer.portal.tail.

  Shape statistics views (skewness, kurtosis, CV)
  are in criterium.viewer.portal.shape.

  Modal analysis views (multimodal warnings)
  are in criterium.viewer.portal.modal.

  Autocorrelation analysis views (ACF plots, classification, ESS)
  are in criterium.viewer.portal.autocorrelation."
  (:refer-clojure :exclude [flush])
  (:require
   [criterium.view :as view]
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.common-charts.profile :as charts.profile]
   [criterium.viewer.portal.allocation]
   [criterium.viewer.portal.autocorrelation]
   [criterium.viewer.portal.core :as portal.core]
   [criterium.viewer.portal.distribution]
   [criterium.viewer.portal.domain]
   [criterium.viewer.portal.modal]
   [criterium.viewer.portal.shape]
   [criterium.viewer.portal.tail]))

;;; Re-exported from portal.core for backwards compatibility

(def tapped
  "Atom storing tapped values and portal-submit function.
   Delegates to criterium.viewer.portal.core."
  portal.core/tapped)

(def submit
  "Tap target function. Delegates to criterium.viewer.portal.core."
  portal.core/submit)

(def flush
  "Flush tapped output. Delegates to criterium.viewer.portal.core."
  portal.core/flush)

(def portal-heading
  "Send hiccup-formatted heading to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/portal-heading)

(def portal-table
  "Send table data to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/portal-table)

(def portal-vega-lite
  "Send Vega-Lite spec to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/portal-vega-lite)

(def portal-vega
  "Send Vega spec to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/portal-vega)

(def heading
  "Send bold heading to tap>. Delegates to criterium.viewer.portal.core."
  portal.core/heading)

;;; Call Tree Views

(defmethod view/call-tree* :portal
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (heading (format "Call Tree (%d total calls)" total-calls))
        (portal-vega (charts.profile/call-tree-tree-vega-spec call-tree {}))))))

(defmethod view/call-flame* :portal
  [_ {:keys [call-tree-id]} data-map]
  (let [call-tree-id (or call-tree-id :call-tree)
        call-tree (get data-map call-tree-id)]
    (when call-tree
      (let [total-calls (call-graph/total-call-count call-tree)]
        (heading "Call Flame Chart")
        (portal-vega (charts.profile/call-tree-flame-vega-spec call-tree total-calls {}))))))

(defmethod view/most-called* :portal
  [_ {:keys [most-called-id]} data-map]
  (let [most-called-id (or most-called-id :most-called)
        most-called-data (get data-map most-called-id)]
    (when most-called-data
      (let [methods (:most-called most-called-data)
            total-in-list (reduce + 0 (map :total-calls methods))]
        (heading (format "Most Called Methods (top %d, %d total calls)"
                         (count methods) total-in-list))
        (portal-vega-lite (charts.profile/most-called-vega-lite-spec most-called-data {}))))))


