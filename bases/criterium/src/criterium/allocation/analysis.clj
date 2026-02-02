(ns criterium.allocation.analysis
  "Analysis functions for allocation trace data.

  Provides functions for summarizing allocations, detecting hotspots,
  and grouping by object type. Functions follow the pipeline pattern
  where each takes options and returns a transformer function that
  operates on a data-map."
  (:require
   [criterium.util.helpers :as util]))

;;; Helper functions

(defn- useful-string?
  "Check if a string contains useful information."
  [s]
  (and (string? s) (seq s)))

(defn- call-site-key
  "Extract call-site key for grouping from an allocation record.
  Falls back to alloc-* fields when call-* fields are not useful.
  When neither has useful class info, uses object-type as fallback."
  [record]
  (let [call-class (:call-class record)
        alloc-class (:alloc-class record)
        call-useful? (useful-string? call-class)
        alloc-useful? (useful-string? alloc-class)]
    (cond
      call-useful?
      {:call-class call-class
       :call-method (:call-method record)
       :call-file (:call-file record)
       :call-line (:call-line record)}

      alloc-useful?
      {:call-class alloc-class
       :call-method (:alloc-method record)
       :call-file (:alloc-file record)
       :call-line (:alloc-line record)}

      :else
      {:call-class (:object-type record)
       :call-method nil
       :call-file nil
       :call-line nil})))

;;; Analysis Functions

(defn summary-fn
  "Returns a function that computes allocation summary statistics.

  Parameters:
    opts - Map with keys:
      :id       - Key for result in output (default: :allocation-summary)

      :trace-id - Path for source allocation trace
                  (default: [:samples :allocation-trace])

  The returned function:
  - Takes a data-map containing an allocation trace at :trace-id path
  - Returns the data-map with summary added under :id
  - Returns data-map unchanged if trace is not present (no-op)

  Summary contains:
    :type            - :criterium/allocation-summary
    :total-allocated - Total bytes allocated
    :total-freed     - Total bytes from freed objects
    :num-allocations - Total number of allocations
    :num-freed       - Number of allocations that were freed
    :freed-ratio     - Ratio of bytes freed to bytes allocated (0.0 to 1.0)"
  ([] (summary-fn {}))
  ([{:keys [id trace-id]}]
   (fn [data-map]
     (let [trace-id (or trace-id [:samples :allocation-trace])
           id (or id :allocation-summary)
           trace (get-in data-map trace-id)]
       (if-not trace
         data-map
         (let [records (:records trace)
               result (reduce
                       (fn [acc record]
                         (let [size (long (:object_size record 0))
                               freed? (:freed record)]
                           (-> acc
                               (update :total-allocated + size)
                               (update :num-allocations inc)
                               (cond->
                                 freed? (-> (update :total-freed + size)
                                            (update :num-freed inc))))))
                       {:type :criterium/allocation-summary
                        :transform {:sample-> identity :->sample identity}
                        :total-allocated 0
                        :total-freed 0
                        :num-allocations 0
                        :num-freed 0}
                       records)
               total-allocated (long (:total-allocated result))
               total-freed (:total-freed result)
               freed-ratio (if (pos? total-allocated)
                             (/ (double total-freed) (double total-allocated))
                             0.0)]
           (assoc data-map id (assoc result :freed-ratio freed-ratio))))))))

(defn hotspots-fn
  "Returns a function that identifies allocation hotspots.

  Identified by call-site and object type.

  Parameters:
    opts - Map with keys:
      :id       - Key for result in output (default: :allocation-hotspots)
      :trace-id - Path for source allocation trace
                  (default: [:samples :allocation-trace])
      :limit    - Maximum number of hotspots to return (default: 10)
      :order-by - Sort key, :bytes or :count (default: :bytes)

  The returned function:
  - Takes a data-map containing an allocation trace at :trace-id path
  - Returns the data-map with hotspots added under :id
  - Returns data-map unchanged if trace is not present (no-op)

  Hotspots result contains:
    :type     - :criterium/allocation-hotspots
    :hotspots - Vector of maps sorted by bytes descending:
                [{:call-site {:call-class ...
                              :call-method ...
                              :call-file ...
                              :call-line ...}
                  :object-type \"Ljava/lang/String;\"
                  :count N
                  :bytes M
                  :freed-count K
                  :freed-bytes L} ...]

  Note: When call-* fields are empty, alloc-* fields are used as fallback."
  ([] (hotspots-fn {}))
  ([{:keys [id trace-id limit order-by]}]
   (fn [data-map]
     (let [trace-id (or trace-id [:samples :allocation-trace])
           id (or id :allocation-hotspots)
           trace (get-in data-map trace-id)]
       (if-not trace
         data-map
         (let [limit (or limit 10)
               sort-key (or order-by :bytes)
               records (:records trace)
               ;; Group by (call-site, object-type) pair
               grouped (reduce
                        (fn [acc record]
                          (let [site (call-site-key record)
                                obj-type (:object-type record)
                                key [site obj-type]
                                size (long (:object_size record 0))
                                freed? (:freed record)]
                            (update acc key
                                    (fn [stats]
                                      (let [stats (or
                                                   stats
                                                   {:count 0
                                                    :bytes 0
                                                    :freed-count 0
                                                    :freed-bytes 0})]
                                        (-> stats
                                            (update :count inc)
                                            (update :bytes + size)
                                            (cond->
                                              freed?
                                              (->
                                               (update :freed-count inc)
                                               (update
                                                :freed-bytes
                                                + size)))))))))
                        {}
                        records)
               ;; Convert to vector and sort
               hotspots (->> grouped
                             (mapv (fn [[[site obj-type] stats]]
                                     (assoc stats
                                            :call-site site
                                            :object-type obj-type)))
                             (sort-by sort-key >)
                             (take limit)
                             vec)]
           (assoc data-map id {:type :criterium/allocation-hotspots
                               :transform {:sample-> identity
                                           :->sample identity}
                               :hotspots hotspots})))))))

(defn by-type-fn
  "Returns a function that groups allocations by object type.

  Parameters:
    opts - Map with keys:
      :id       - Key for result in output (default: :allocation-by-type)
      :trace-id - Path for source allocation trace
                  (default: [:samples :allocation-trace])

  The returned function:
  - Takes a data-map containing an allocation trace at :trace-id path
  - Returns the data-map with by-type grouping added under :id
  - Returns data-map unchanged if trace is not present (no-op)

  Result contains:
    :type    - :criterium/allocation-by-type
    :by-type - Map of object-type to stats:
               {\"Ljava/lang/String;\" {:count N
                                        :bytes M
                                        :freed-count K
                                        :freed-bytes L} ...}"
  ([] (by-type-fn {}))
  ([{:keys [id trace-id]}]
   (fn [data-map]
     (let [trace-id (or trace-id [:samples :allocation-trace])
           id (or id :allocation-by-type)
           trace (get-in data-map trace-id)]
       (if-not trace
         data-map
         (let [records (:records trace)
               by-type (reduce
                        (fn [acc record]
                          (let [obj-type (:object-type record)
                                size (long (:object_size record 0))
                                freed? (:freed record)]
                            (update acc obj-type
                                    (fn [stats]
                                      (let [stats (or stats {:count 0
                                                             :bytes 0
                                                             :freed-count 0
                                                             :freed-bytes 0})]
                                        (-> stats
                                            (update :count inc)
                                            (update :bytes + size)
                                            (cond->
                                              freed?
                                              (-> (update :freed-count inc)
                                                  (update
                                                   :freed-bytes
                                                   + size)))))))))
                        {}
                        records)]
           (assoc data-map id {:type :criterium/allocation-by-type
                               :transform {:sample-> identity
                                           :->sample identity}
                               :by-type by-type})))))))

;;; Analysis Pipeline

(defn- resolve-allocation-analyse-fn
  "Resolves a single allocation analysis function specification.
  If x is a sequence, treats first element as function and rest as args.
  Otherwise treats x as a function name to resolve.
  Returns a function of one argument (the data-map)."
  [x]
  (let [options {:default-ns 'criterium.allocation.analysis}]
    (if (sequential? x)
      (apply (util/maybe-var-get (first x) options) (rest x))
      ((util/maybe-var-get x options)))))

(defn- filter-records
  "Filter allocation records based on freed status."
  [records filter-by]
  (case filter-by
    :freed (filterv :freed records)
    :not-freed (filterv (complement :freed) records)
    :all records
    records))

(defn- compute-value
  "Compute the value for a node based on size-by option."
  [^long count ^long bytes size-by]
  (case size-by
    :count count
    :bytes bytes
    :bytes-per-allocation (if (pos? count)
                            (/ (double bytes) (double count))
                            0.0)
    bytes))

(defn- build-treemap-node
  "Build a treemap node with children, computing values bottom-up.
  Leaf nodes in the hierarchy have
      {:count N :bytes M :freed-count K :freed-bytes L}.
  Leaf nodes in output preserve all stats for tooltips."
  [name children-map size-by]
  (if (empty? children-map)
    {:name name :value 0}
    (let [children (mapv (fn [[child-name child-data]]
                           (if (and (map? child-data)
                                    (contains? child-data :count)
                                    (contains? child-data :bytes))
                             ;; Leaf node with stats - preserve them
                             {:name child-name
                              :value (compute-value (long (:count child-data))
                                                    (long (:bytes child-data))
                                                    size-by)
                              :bytes (:bytes child-data)
                              :count (:count child-data)
                              :freed-bytes (:freed-bytes child-data 0)
                              :freed-count (:freed-count child-data 0)}
                             ;; Intermediate node - recurse
                             (build-treemap-node
                              child-name
                              child-data
                              size-by)))
                         children-map)
          total-value (reduce + 0.0 (map :value children))]
      (cond-> {:name name :value (if (every? integer? (map :value children))
                                   (long total-value)
                                   total-value)}
        (seq children) (assoc :children children)))))

(defn- group-by-hierarchy
  "Group records into nested maps according to hierarchy.
  Returns nested maps where leaves have
     {:count N :bytes M :freed-count K :freed-bytes L}."
  [records group-by-opt]
  (let [extract-keys (case group-by-opt
                       :class→line→type
                       (fn [r]
                         (let [site (call-site-key r)]
                           [(:call-class site)
                            (str "L" (:call-line site))
                            (:object-type r)]))

                       :type→class→line
                       (fn [r]
                         (let [site (call-site-key r)]
                           [(:object-type r)
                            (:call-class site)
                            (str "L" (:call-line site))]))

                       ;; Default to :class→line→type
                       (fn [r]
                         (let [site (call-site-key r)]
                           [(:call-class site)
                            (str "L" (:call-line site))
                            (:object-type r)])))]
    (reduce
     (fn [tree record]
       (let [[k1 k2 k3] (extract-keys record)
             size (long (:object_size record 0))
             freed? (:freed record)]
         (update-in tree [k1 k2 k3]
                    (fn [stats]
                      (let [stats (or stats {:count 0 :bytes 0
                                             :freed-count 0 :freed-bytes 0})]
                        (-> stats
                            (update :count inc)
                            (update :bytes + size)
                            (cond->
                              freed? (-> (update :freed-count inc)
                                         (update :freed-bytes + size)))))))))
     {}
     records)))

(defn treemap-fn
  "Returns a function that transforms allocation records into treemap data.

  Parameters:
    opts - Map with keys:
      :id        - Key for result in output (default: :allocation-treemap)
      :trace-id  - Path for source allocation trace
                   (default: [:samples :allocation-trace])
      :group-by  - Hierarchy option:
                   :class→line→type (default):
                        calling-class → calling-line → object-type
                   :type→class→line:
                        object-type → calling-class → calling-line
      :size-by   - Value for node sizing: :count, :bytes, :bytes-per-allocation
                   (default: :bytes)
      :filter-by - Filter records: :freed, :not-freed, :all (default: :all)

  The returned function:
  - Takes a data-map containing an allocation trace at :trace-id path
  - Returns the data-map with treemap added under :id
  - Returns data-map unchanged if trace is not present (no-op)

  Treemap result contains:
    :type     - :criterium/allocation-treemap
    :group-by - The grouping option used
    :size-by  - The sizing option used
    :root     - Root node with :name, :value, and optional :children
                Leaf nodes have no :children key
                Values at each level are sum of children values"
  ([] (treemap-fn {}))
  ([{:keys [id trace-id group-by size-by filter-by]}]
   (fn [data-map]
     (let [trace-id (or trace-id [:samples :allocation-trace])
           id (or id :allocation-treemap)
           group-by (or group-by :class→line→type)
           size-by (or size-by :bytes)
           filter-by (or filter-by :all)
           trace (get-in data-map trace-id)]
       (if-not trace
         data-map
         (let [records (:records trace)
               filtered (filter-records records filter-by)
               hierarchy (group-by-hierarchy filtered group-by)
               root (build-treemap-node "allocations" hierarchy size-by)]
           (assoc data-map id
                  {:type :criterium/allocation-treemap
                   :transform {:sample-> identity :->sample identity}
                   :group-by group-by
                   :size-by size-by
                   :root root})))))))

(defn ->allocation-analyse
  "Creates a composite analysis function from a sequence of analysis specs.

  Each spec is either a keyword/symbol to resolve a function, or a vector
  with a keyword/symbol first element followed by an options map.

  Analysis functions are resolved from the criterium.allocation.analysis
  namespace.  They are composed in sequence, each taking and returning a
  data-map.

  Example specs:
    [[:summary-fn {:id :summary}]
     [:hotspots-fn {:limit 5}]
     [:by-type-fn {}]]

  Returns a function that takes a data-map and returns the analyzed data-map."
  [analyse-plan]
  (when-not (or (nil? analyse-plan) (sequential? analyse-plan))
    (throw
     (ex-info "analyse must be a sequence of specs" {:analyse analyse-plan})))
  (let [fns (mapv resolve-allocation-analyse-fn analyse-plan)]
    (reduce comp (reverse fns))))
