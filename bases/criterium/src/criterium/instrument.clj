(ns criterium.instrument
  "Instrumentation of a function to gather samples when calling the function."
  (:refer-clojure :exclude [reset!])
  (:require
   [criterium.collect :as collect]
   [criterium.collector :as collector]
   [criterium.jvm :as jvm]
   [criterium.measured :as measured]))

(def ^:private original-f ::original-f)
(def ^:private samples ::samples)

(defn- wrap [f sample-atom pipeline]
  {:pre [(fn? f)
         (instance? clojure.lang.Atom sample-atom)]}
  (let [measured (measured/measured
                  (fn state-f [] (assert false))
                  (fn measured-f [args eval-count]
                    (assert (= 1 eval-count))
                    (let [start  (jvm/timestamp)
                          res    (apply f args)
                          finish (jvm/timestamp)]
                      [(unchecked-subtract finish start) res])))]
    (assert (:length pipeline) pipeline)
    (fn instrumented-f [& args]
      (let [sample (collector/collect pipeline measured args 1)]
        (swap! sample-atom conj sample)
        (:expr-value sample)))))

(defn instrument!
  "Add instrumentation to the var, v.

  You must use uninstrument! to remove the instrumentation."
  [v pipeline]
  (let [sample-atom (atom [])]
    (alter-meta! v assoc original-f @v samples sample-atom)
    (alter-var-root v wrap sample-atom pipeline)))

(defn uninstrument!
  "Remove instrumentation from the var, v."
  [v]
  (when-let [f (original-f (meta v))]
    (alter-var-root v (constantly f))
    (alter-meta! v dissoc original-f samples)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn reset!
  "Reset the sample collection on the var, v."
  [v]
  (clojure.core/reset! (some-> v meta samples) []))

(defn sample-map
  "Convert samples into a sample map.

  The sample map can be analysed."
  [metrics-configs samples]
  {:batch-size      1
   :eval-count      (count samples)
   :samples         (with-meta
                      ((collect/sample-maps->map-of-samples metrics-configs)
                       samples)
                      {:type      :criterium/samples
                       :transform {:sample-> identity :->sample identity}})
   :metrics-configs metrics-configs})

(defmacro with-instrumentation
  "Provides a scope within which the top level function f is instrumented.
  Returns a tuple with the body result and a sampled result."
  [[f collector-config] & body]
  {:pre [f collector-config]}
  `(let [v#                (var ~f)
         collector-config# ~collector-config
         pipeline#         (collector/collector collector-config#)
         metrics-configs#  (:metrics-configs pipeline#)]
     (try
       (instrument! v# pipeline#)
       (let [start#   (jvm/timestamp)
             res#     (do ~@body)
             finsih#  (jvm/timestamp)
             samples# (-> v# meta ~samples deref)]
         [(assoc (sample-map metrics-configs# samples#)
                 :elapsed-time (unchecked-subtract finsih# start#))
          res#])
       (finally
         (uninstrument! v#)))))
