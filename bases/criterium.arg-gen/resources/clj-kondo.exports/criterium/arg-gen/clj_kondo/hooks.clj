(ns clj-kondo.hooks
  (:require
   [clj-kondo.hooks-api :as api]))

(defn measured
  "Rewrite atg-gen measured forms."
  [{:keys [node]}]
  (let [[_measured & more]         (:children node)
        [_options bindings & body] (if (api/map-node? (first more))
                                     more
                                     (into [nil] more))
        new-node                   (api/list-node
                                    (list*
                                     (api/token-node 'let)
                                     bindings
                                     body))]
    ;; un-comment below to debug changes
    {:node (with-meta new-node (meta node))}))
