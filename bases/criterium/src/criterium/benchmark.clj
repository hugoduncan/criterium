(ns criterium.benchmark
  (:require
   [criterium.util.helpers :as util]))

(defn- maybe-var-get-analyse-1 [x]
  (let [options {:default-ns 'criterium.analyse}]
    (if (sequential? x)
      (apply (util/maybe-var-get (first x) options) (rest x))
      ((util/maybe-var-get x options)))))

(defn- maybe-var-get-view-1 [x]
  (let [options {:default-ns 'criterium.view}]
    (if (sequential? x)
      (apply (util/maybe-var-get (first x) options) (rest x))
      ((util/maybe-var-get x options)))))

(defn- maybe-var-get-analyse [spec]
  (mapv maybe-var-get-analyse-1 spec))

(defn- maybe-var-get-view [spec]
  (mapv maybe-var-get-view-1 spec))

(defn- compose
  [{:keys [analyse view]}]
  (let [a (reduce comp (reverse analyse))]
    (fn [sampled]
      (let [result (a sampled)]
        (doseq [v view]
          (v result))
        result))))

(defn ->benchmark
  "Compose a benchmark based on a declarative map.

  The :analyse and :view keywords take a vector of function specs.  A
  function spec is either a keyword or symbol used to resolve a
  function, or a vector of with a keyword or symbol first element,
  followed by arguments to use.

  Functions should return a function of a single argument, the sampled
  map, and return the same."
  [benchmark-spec]
  (-> benchmark-spec
      (update :analyse maybe-var-get-analyse)
      (update :view maybe-var-get-view)
      compose))
