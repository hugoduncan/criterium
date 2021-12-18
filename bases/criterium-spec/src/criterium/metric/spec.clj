(ns criterium.metric.spec
  (:require
   [clojure.spec.alpha :as s]
   [criterium.domain :as domain]))

(s/fdef elapsed-time
  :args (s/cat :sample ::sample)
  :ret ::domain/elapsed-time
  :fn #(= (:ret %) (-> % :args :sample :elapsed-time)))

(s/def ::total-memory (s/and number? pos?))

(s/fdef total-memory
  :args (s/cat :sample ::sample)
  :ret ::total-memory
  :fn #(= (:ret %) (-> % :args :sample :memory :total :used)))

;; (s/fdef divide
;;   :args (s/cat :sample ::sample :divisor number?)
;;   :ret ::sample
;;   :fn #(= (-> % :ret :elapsed-time)
;;           (/ (-> % :args :sample :elapsed-time)
;;              (-> % :args :divisor))))
