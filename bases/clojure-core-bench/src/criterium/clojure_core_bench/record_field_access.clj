(ns criterium.clojure-core-bench.record-field-access
  (:require
   [clojure.test.check.generators :as gen]
   [criterium.arg-gen :as arg-gen]
   [criterium.time :as time]))

;;; Record field access

(defrecord Record [^long field] )

(defn record-field-access-measured
  []
  (arg-gen/measured
      {:size 3}
      [v (gen/fmap ->Record gen/large-integer)]
    (.field ^Record v)))

(defn measure-record-field-access
  [options]
  (time/time-measured
   (record-field-access-measured)
   (merge
    {:limit-time-s 1
     :return-value [::nil]}
    options)))

(defn record-keyword-access-measured
  []
  (arg-gen/measured
      {:size 3}
      [v (gen/fmap ->Record gen/large-integer)]
    (:field v)))

(defn measure-keyword-access-measured
  [options]
  (time/time-measured
   (record-keyword-access-measured)
   (merge
    {:limit-time-s 1
     :return-value [::nil]}
    options)))

(do ;;comment
  (measure-keyword-access-measured {:limit-time-s 10})
  (measure-record-field-access {:limit-time-s 10}))

;; (def record (->Record 1))

;; (time/time (.field ^Record record) :limit-time-s 10)
;; (time/time (:field ^Record record) :limit-time-s 10)
