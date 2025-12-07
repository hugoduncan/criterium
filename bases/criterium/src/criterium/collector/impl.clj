(ns criterium.collector.impl
  (:require
   [criterium.collector.fns :as fns]
   [criterium.util.helpers :as util]))

(defn maybe-var-get-stage [x]
  (util/maybe-var-get x {:default-ns 'criterium.collector.fns}))

(defn maybe-var-get-stages [x]
  (mapv maybe-var-get-stage x))

(defn maybe-var-get-config
  [config]
  (-> config
      (update :stages maybe-var-get-stages)
      (update :terminator maybe-var-get-stage)))

(defn metric-ids
  "Return a sequence of all metrics produced by a pipeline with the
  given pipeline config."
  [{:keys [stages terminator] :as _pipline-config}]
  {:pre [stages terminator]}
  (mapv :id (conj stages terminator)))

;;; Pipeline construction

(defn pipeline-xform-fn
  "Build a pipeline xform by specifying pipeline function keywords.

  Returns an xform function."
  [{:keys [stages terminator] :as _collector-config}]
  (let [terminal-fn (:x terminator)]
    (when-not terminal-fn
      (throw (ex-info "Unknown terminator function"
                      {:stages     stages
                       :terminator terminator})))
    (reduce
     (fn [pipeline stage]
       (let [f (:x stage)]
         (when-not f
           (throw (ex-info "Unknown pipeline xform function" {:stage stage})))
         (f pipeline)))
     terminal-fn
     stages)))

(defn- stage-m [stage]
  (let [m (:m stage)]
    (when-not m
      (throw (ex-info "Unknown pipeline function m" {:stage stage})))
    m))

(defn pipeline-sample-fn*
  "Build a pipeline sample function by specifying pipeline function keywords.

  Returns a function to collect a sample."
  ;; We want a garbage free sample function.  This means:
  ;;    can't use persistent collections
  ;; => we need to index into a result collction
  ;; => we need to use primitive math
  ;; => we can't use function composition
  ;;
  ;; the result is using macros and eval.
  [{:keys [stages terminator] :as collector-config}]
  (let [terminal-m        (:m terminator)
        _                 (when-not terminal-m
                            (throw
                             (ex-info
                              "Unknown terminator function"
                              {:collector-config collector-config})))
        stage-ms          (conj
                           (mapv stage-m (reverse stages))
                           terminal-m)
        sample-sym        (fns/sample-gensym)
        measured-sym      (gensym "measured")
        state-sym         (gensym "state")
        eval-count-sym    (gensym "eval-count")
        ;; eval-count-sym    (with-meta (gensym "eval-count") {:tag 'long})
        result-index-sym1 (gensym "result-index")
        result-index-sym  (with-meta (gensym "result-index") {:tag 'long})]
    `(fn ~'sample
       [~sample-sym
        ~measured-sym
        ~state-sym
        ~eval-count-sym
        ~result-index-sym1]
       (let [~result-index-sym ~result-index-sym1]
         ~((first stage-ms)
           (rest stage-ms)
           sample-sym
           measured-sym
           state-sym
           eval-count-sym
           result-index-sym)))))

(defn pipeline-sample-fn
  "Build a pipeline sample function by specifying pipeline function keywords.

  Returns a function to collect a sample."
  [collector-config]
  (binding [*compiler-options*
            (merge *compiler-options*
                   {:direct-linking true})]
    (eval (pipeline-sample-fn* collector-config))))

(defrecord SamplePipeline
  [f x length])

(defn pipeline*
  "Build a pipeline by specifying pipeline function keywords.

  Returns an updated state, adding :pipeline and :metrics keys."
  [collector-config]
  (map->SamplePipeline
   {:f      (pipeline-sample-fn collector-config)
    :x      (pipeline-xform-fn collector-config)
    :length (inc (count (:stages collector-config 0)))}))
