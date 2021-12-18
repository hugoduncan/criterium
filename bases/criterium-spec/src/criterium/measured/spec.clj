(ns criterium.measured.spec
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as sgen]
   [criterium.domain :as domain]
   [criterium.measured :as measured]))

(s/def ::measured/state-fn (s/fspec :args (s/cat) :ret any?))
(s/def ::measured/measured-tuple (s/tuple ::domain/elapsed-time any?))

(s/def ::measured/measure-fn (s/fspec
                              :args (s/cat :state any? :eval-count ::domain/eval-count)
                              :ret ::measured/measured-tuple))

(s/def ::measured/expr-fn (s/or :empty nil?
                       :fn (s/fspec
                            :args (s/cat)
                            :ret any?)))

(s/def ::measured/measured
  (s/with-gen
    (s/and measured/measured?
           (comp ifn? :state-fn)
           (comp ifn? :f)
           (comp (some-fn nil? ifn?) :expr-fn))
    #(sgen/fmap
      (fn [[s t u]] (measured/measured s t u))
      (sgen/tuple
       (s/gen ::measured/state-fn)
       (s/gen ::measured/measure-fn)
       (s/gen ::measured/expr-fn)))))

(s/fdef measured/measured
  :args (s/cat :state-fn ::measured/state-fn
               ;; We can't conform this, as we can't restrain state values
               ;; to those produced by state-fn
               :measure-fn fn? ;;  :measure-fn
               :expr-fn (s/? ::measured/expr-fn))
  :ret ::measured/measured
  :fn (fn [s]
        ((-> s :args :measure-fn)
         ((-> s :args :state-fn))
         1)))

(s/fdef measured/measured?
  :args (s/cat :obj any?)
  :ret boolean?)

(s/fdef measured/invoke
  :args (s/cat :measured ::measured/measured
               :state any?
               :eval-count ::domain/eval-count)
  :ret ::measured/measured-tuple)

(s/fdef measured/generate-state
  :args (s/cat :measured ::measured/measured)
  :ret any?)

(s/fdef measured/symbolic
  :args (s/cat :measured ::measured/measured)
  :ret any?)

(s/fdef measured/expr
  :args (s/alt
         :unary (s/cat :expr any?)
         :binary (s/cat :expr any? :options (s/keys :opt-un [::measured/time-fn])))
  :ret ::measured/measured)
