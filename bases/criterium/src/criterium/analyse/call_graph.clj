(ns criterium.analyse.call-graph
  "Analysis functions for call graph data from method tracing.

  Provides analysis functions that operate on call tree data collected
  via the method tracing agent.")

;;; Call Tree Flattening

(defn- flatten-call-tree
  "Flatten a hierarchical call tree into a sequence of node maps.
  Each node retains :class, :method, :file, :line, and :call-count."
  [node]
  (when node
    (let [current {:class (:class node)
                   :method (:method node)
                   :file (:file node)
                   :line (:line node)
                   :call-count (or (:call-count node) 0)}
          children (:children node)]
      (if (seq children)
        (cons current (mapcat flatten-call-tree children))
        [current]))))

(defn- aggregate-by-method
  "Aggregate flattened nodes by class+method, summing call counts.
  Returns a map of [class method] -> {:class :method :file :line :total-calls}."
  [nodes]
  (reduce
   (fn [acc {:keys [class method file line call-count]}]
     (let [k [class method]]
       (if-let [existing (get acc k)]
         (assoc acc k (update existing :total-calls + call-count))
         (assoc acc k {:class class
                       :method method
                       :file file
                       :line line
                       :total-calls call-count}))))
   {}
   nodes))

;;; Most-Called Analysis

(defn most-called
  "Analysis function that identifies the most frequently called methods.

  Flattens the call tree and aggregates by class+method, returning a sorted
  vector of the top N methods by total call count.

  Parameters:
    opts - Optional map with keys:
      :id           - Key for result in output (default: :most-called)
      :call-tree-id - Key for source call tree (default: :call-tree)
      :limit        - Maximum methods to return (default: 20)

  The returned function:
  - Takes a data-map containing :call-tree
  - Returns the data-map with :most-called added
  - Returns data-map unchanged if call-tree is not present

  Result structure:
  {:type :criterium/most-called
   :most-called [{:class \"com.example.Foo\"
                  :method \"bar\"
                  :file \"Foo.java\"
                  :line 42
                  :total-calls 1500}
                 ...]}

  Example:
  (let [analyze (most-called {:limit 10})
        result (analyze {:call-tree {...}})]
    (:most-called result))"
  ([] (most-called {}))
  ([{:keys [id call-tree-id limit]
     :or {id :most-called
          call-tree-id :call-tree
          limit 20}}]
   (fn [data-map]
     (let [call-tree (get data-map call-tree-id)]
       (if-not call-tree
         data-map
         (let [flat-nodes (flatten-call-tree call-tree)
               aggregated (aggregate-by-method flat-nodes)
               sorted (->> (vals aggregated)
                           (sort-by :total-calls >)
                           (take limit)
                           vec)
               result {:type :criterium/most-called
                       :source-id call-tree-id
                       :limit limit
                       :most-called sorted}]
           (assoc data-map id result)))))))
