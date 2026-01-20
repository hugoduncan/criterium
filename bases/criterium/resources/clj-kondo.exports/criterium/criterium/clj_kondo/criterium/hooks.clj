(ns clj-kondo.criterium.hooks
  (:require
   [clj-kondo.hooks-api :as api]))

(defn domain-expr
  "Rewrite domain-expr forms for clj-kondo analysis.

  Transforms (domain-expr [n range1 m range2] body options?)
  into (let [n (first range1) m (first range2)] body options?)

  This allows clj-kondo to verify that:
  - Axis symbols are properly bound (to single elements, not collections)
  - Body expressions can reference axis symbols
  - Range expressions are valid"
  [{:keys [node]}]
  (let [[_domain-expr bindings body options] (:children node)
        ;; Transform [sym1 range1 sym2 range2 ...] to [sym1 (first range1) sym2 (first range2) ...]
        binding-pairs (partition 2 (:children bindings))
        transformed-bindings (mapcat (fn [[sym range-expr]]
                                       [sym (api/list-node
                                             [(api/token-node 'first)
                                              range-expr])])
                                     binding-pairs)
        new-bindings (api/vector-node transformed-bindings)
        let-body (if options
                   (list body options)
                   (list body))
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   new-bindings
                   let-body))]
    {:node (with-meta new-node (meta node))}))
