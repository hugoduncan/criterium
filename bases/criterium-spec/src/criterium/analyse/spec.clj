(ns criterium.analyse.spec
  (:require
   [clojure.spec.alpha :as s]))


(s/def ::input-state (s/keys :req-un [:criterium.config/config
                                      :criterium.sample-scheme/sampled]))

(s/def ::output-state (s/keys :req-un [:criterium.config/config
                                       :criterium.sample-scheme/sampled]))

(s/fdef analyse
  :args (s/cat :input-state ::input-state)
  :ret ::output-state)
