(ns clj-kondo.hooks
  (:require
   [clj-kondo.hooks-api :as api]))

(defn measured
  "Rewrite arg-gen measured forms."
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
    {:node (with-meta new-node (meta node))}))

(defn args-fn
  "Rewrite arg-gen args-fn forms.
  Transforms (args-fn opts? [bindings...] body) to (let [bindings...] body)
  for clj-kondo analysis."
  [{:keys [node]}]
  (let [[_args-fn & more]          (:children node)
        [_options bindings & body] (if (api/map-node? (first more))
                                     more
                                     (into [nil] more))
        new-node                   (api/list-node
                                    (list*
                                     (api/token-node 'let)
                                     bindings
                                     body))]
    {:node (with-meta new-node (meta node))}))
