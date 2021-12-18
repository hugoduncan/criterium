(ns hooks.impl
  "Hooks for criterium implementation linting"
  (:require
   [clj-kondo.hooks-api :as api]))

(defn def-multi-view
  "Rewrite def-multi-view to define the funcrion and multi-method."
  [{:keys [node]}]
  (let [[_ n]    (:children node)
        mm-n     (api/token-node (symbol (str (:string-value n) "*")))
        new-node (api/list-node
                  (list
                   (api/token-node 'do)
                   (api/list-node
                    (list
                     (api/token-node 'defmulti)
                     mm-n
                     (api/token-node 'identity)))
                   (api/list-node
                    (list
                     (api/token-node 'defn) n
                     (api/list-node
                      (list (api/vector-node [])))
                     (api/list-node
                      (list
                       (api/vector-node [(api/token-node 'arg)])
                       (api/token-node 'arg)))))
                   ;; use the multi-view
                   (api/list-node
                    (list n))))]
    ;; un-comment below to debug changes
    ;; (prn :def-multi-view (api/sexpr new-node))
    {:node (with-meta new-node (meta node))}))
