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
      {:call-class  call-class
       :call-method (:call-method record)
       :call-file   (:call-file record)
       :call-line   (:call-line record)}

      alloc-useful?
      {:call-class  alloc-class
       :call-method (:alloc-method record)
       :call-file   (:alloc-file record)
       :call-line   (:alloc-line record)}

      :else
      {:call-class  (:object-type record)
       :call-method nil
       :call-file   nil
       :call-line   nil})))

;;; Analysis Functions

(defn summary-fn
  "Returns a function that computes allocation summary statistics.

  Parameters:
    opts - Map with keys:
      :id       - Key for result in output (default: :allocation-summary)
      :trace-id - Path for source allocation trace (default: [:samples :allocation-trace])

  The returned function:
  - Takes a data-map containing an allocation trace at :trace-id path
  - Returns the data-map with summary added under :id
  - Returns data-map unchanged if trace is not present (no-op)

  Summary contains:
    :type            - :criterium/allocation-summary
    :total-allocated - Total bytes allocated
    :total-freed     - Total bytes from freed objects
    :num-allocations - Total number of allocations
    :num-freed       - Number of allocations that were freed"
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
                               freed? (pos? (long (:freed record 0)))]
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
                       records)]
           (assoc data-map id result)))))))

(defn hotspots-fn
  "Returns a function that identifies allocation hotspots by call-site.

  Parameters:
    opts - Map with keys:
      :id       - Key for result in output (default: :allocation-hotspots)
      :trace-id - Path for source allocation trace (default: [:samples :allocation-trace])
      :limit    - Maximum number of hotspots to return (default: 10)
      :order-by - Sort key, :bytes or :count (default: :bytes)

  The returned function:
  - Takes a data-map containing an allocation trace at :trace-id path
  - Returns the data-map with hotspots added under :id
  - Returns data-map unchanged if trace is not present (no-op)

  Hotspots result contains:
    :type     - :criterium/allocation-hotspots
    :hotspots - Vector of maps sorted by bytes descending:
                [{:call-site {:call-class ... :call-method ... :call-file ... :call-line ...}
                  :object-types #{\"Ljava/lang/String;\" ...}
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
               ;; Group by call-site and aggregate
               grouped (reduce
                        (fn [acc record]
                          (let [site (call-site-key record)
                                obj-type (:object-type record)
                                size (long (:object_size record 0))
                                freed? (pos? (long (:freed record 0)))]
                            (update acc site
                                    (fn [stats]
                                      (let [stats (or stats {:count 0 :bytes 0
                                                             :freed-count 0 :freed-bytes 0
                                                             :object-types #{}})]
                                        (-> stats
                                            (update :count inc)
                                            (update :bytes + size)
                                            (update :object-types conj obj-type)
                                            (cond->
                                              freed? (-> (update :freed-count inc)
                                                         (update :freed-bytes + size)))))))))
                        {}
                        records)
               ;; Convert to vector and sort
               hotspots (->> grouped
                             (mapv (fn [[site stats]]
                                     (assoc stats :call-site site)))
                             (sort-by sort-key >)
                             (take limit)
                             vec)]
           (assoc data-map id {:type :criterium/allocation-hotspots
                               :transform {:sample-> identity :->sample identity}
                               :hotspots hotspots})))))))

(defn by-type-fn
  "Returns a function that groups allocations by object type.

  Parameters:
    opts - Map with keys:
      :id       - Key for result in output (default: :allocation-by-type)
      :trace-id - Path for source allocation trace (default: [:samples :allocation-trace])

  The returned function:
  - Takes a data-map containing an allocation trace at :trace-id path
  - Returns the data-map with by-type grouping added under :id
  - Returns data-map unchanged if trace is not present (no-op)

  Result contains:
    :type    - :criterium/allocation-by-type
    :by-type - Map of object-type to stats:
               {\"Ljava/lang/String;\" {:count N :bytes M :freed-count K :freed-bytes L} ...}"
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
                                freed? (pos? (long (:freed record 0)))]
                            (update acc obj-type
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
                        records)]
           (assoc data-map id {:type :criterium/allocation-by-type
                               :transform {:sample-> identity :->sample identity}
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

(defn ->allocation-analyse
  "Creates a composite analysis function from a sequence of analysis specs.

  Each spec is either a keyword/symbol to resolve a function, or a vector
  with a keyword/symbol first element followed by an options map.

  Analysis functions are resolved from the criterium.allocation.analysis namespace.
  They are composed in sequence, each taking and returning a data-map.

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
