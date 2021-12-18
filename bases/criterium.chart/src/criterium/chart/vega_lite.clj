(ns criterium.chart.vega-lite
  (:require
   [oz.core :as oz]))

(defonce ^:internal server (atom nil))

(defn- start-server-if-neederd! [start-f]
  (if @server
    (throw
     (ex-info "server already started, run stop-server! to shut it down" {}))
    (let [s (start-f)]
      (if (compare-and-set! server nil s)
        s
        (do
          (s)      ; someone got there first, so stop the one we started.
          @server)))))

(defn start-server!
  ([] (start-server-if-neederd! #(oz/start-server!)))
  ([port] (start-server-if-neederd! #(oz/start-server! port))))

(defn ensure-server! []
  (when-not @server (start-server!)))

(defn stop-server!
  []
  (when-let [s @server]
    (when (compare-and-set! server s nil)
      (s))))

(defn view! [x]
  (ensure-server!)
  (oz/view! x))

(defn export! [spec file]
  (oz/export! spec file))

(defn publish!
  "Publish to gist"
  [spec]
  (ensure-server!)
  (oz/publish! spec))
