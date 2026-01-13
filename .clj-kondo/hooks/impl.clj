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

(defn cond*
  "Hook for cond* macro that handles :let bindings.
  Transforms cond* into nested let and if forms so clj-kondo can track bindings."
  [{:keys [node]}]
  (let [children (rest (:children node))] ; skip 'cond*
    (letfn [(process-clauses [clauses]
              (if (seq clauses)
                (let [guard (first clauses)
                      remaining (rest clauses)]
                  (if (seq remaining)
                    (let [body (first remaining)
                          rest-clauses (rest remaining)]
                      (if (= :let (api/sexpr guard))
                        ;; :let clause - create a let form
                        (api/list-node
                         (list
                          (api/token-node 'let)
                          body
                          (or (process-clauses rest-clauses)
                              (api/token-node 'nil))))
                        ;; regular cond clause - create if form
                        (api/list-node
                         (list
                          (api/token-node 'if)
                          guard
                          body
                          (or (process-clauses rest-clauses)
                              (api/token-node 'nil))))))
                    ;; odd number of clauses, error case
                    (api/token-node 'nil)))
                ;; no more clauses
                nil))]
      (let [new-node (or (process-clauses children)
                         (api/token-node 'nil))]
        ;; un-comment below to debug changes
        ;; (prn :cond* :original (api/sexpr node))
        ;; (prn :cond* :transformed (api/sexpr new-node))
        {:node (with-meta new-node (meta node))}))))

(defn invoke-dd
  "Hook for invoke-dd macro - transforms (invoke-dd f v) to (f v)."
  [{:keys [node]}]
  (let [[_ f v] (:children node)
        new-node (api/list-node (list f v))]
    {:node (with-meta new-node (meta node))}))
