;;;; Copyright (c) Hugo Duncan. All rights reserved.

;;;; The use and distribution terms for this software are covered by the
;;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;; which can be found in the file epl-v10.html at the root of this distribution.
;;;; By using this software in any fashion, you are agreeing to be bound by
;;;; the terms of this license.
;;;; You must not remove this notice, or any other, from this software.

(ns criterium.toolkit
  "Standalone instrumentation"
  (:require [criterium
             [jvm :as jvm]
             [util :as util]]))


(defn assoc-delta
  "Assoc finish merged with start using op, onto the :delta key."
  [{:keys [start finish] :as data}]
  (assoc data :delta (util/diff finish start)))


;; Macros to wrap an expr execution.  The macros use a first data
;; argument to accumulate data about the expr evaluation.


(defmacro instrumented
  "Introduces an instrumented expression expr.

  The expr contains nested with-* macro calls, and returns
  a map with the collected data.
  "
  [expr]
  `(-> {} ~expr))


(defmacro with-expr-value
  "Execute expr, adding the return value to the data map's :expr-value key."
  [data expr]
  `(assoc ~data :expr-value ~expr))


(defmacro with-expr
  "Execute expr."
  [data expr]
  `(let [d# ~data]
     ~expr
     d#))


(defmacro with-time
  "Execute expr, adding timing to the data map.

  Adds maps to the :time key in data, with the :before, :after, and
  :delta sub-data.  Each map contains the :elapsed key with a timestamp in
  nanoseconds."
  [data expr]
  `(-> ~data
       (assoc-in [:time :start] {:elapsed (jvm/timestamp)})
       ~expr
       (assoc-in [:time :finish] {:elapsed (jvm/timestamp)})
       (update-in [:time] assoc-delta)))


(defmacro with-class-loader-counts
  "Execute expr, add class loading counts to the data map.

  Adds JvmClassLoaderState records to the :class-loader key in data,
  with the :before, :after, and :delta sub-keys.
  "
  [data expr]
  `(-> ~data
       (assoc-in [:class-loader :start] (jvm/class-loader-counts))
       ~expr
       (assoc-in [:class-loader :finish] (jvm/class-loader-counts))
       (update-in [:class-loader] assoc-delta)))


(defmacro with-compilation-time
  "Execute expr, add compilation time to the data map.

  Adds JvmClassLoaderState records to the :compilation key in data,
  with the :before, :after, and :delta sub-keys.
  "
  [data expr]
  `(-> ~data
       (assoc-in [:compilation :start] (jvm/compilation-time))
       ~expr
       (assoc-in [:compilation :finish] (jvm/compilation-time))
       (update-in [:compilation] assoc-delta)))


(defmacro with-memory
  "Execute expr, add compilation time to the data map.

  Adds JvmClassLoaderState records to the :compilation key in data,
  with the :before, :after, and :delta sub-keys.
  "
  [data expr]
  `(-> ~data
       (assoc-in [:memory :start] (jvm/memory))
       ~expr
       (assoc-in [:memory :finish] (jvm/memory))
       (update-in [:memory] assoc-delta)))


(defmacro with-finalization-count
  "Execute expr, add compilation time to the data map.

  Adds JvmClassLoaderState records to the :compilation key in data,
  with the :before, :after, and :delta sub-keys.
  "
  [data expr]
  `(-> ~data
       (assoc-in [:finalization :start] (jvm/finalization-count))
       ~expr
       (assoc-in [:finalization :finish] (jvm/finalization-count))
       (update-in [:finalization] assoc-delta)))


(defmacro with-garbage-collector-stats
  "Execute expr, add garbage collection counts and times to the data map."
  [data expr]
  `(-> ~data
       (assoc-in [:garbage-collector :start] (jvm/garbage-collector-stats))
       ~expr
       (assoc-in [:garbage-collector :finish] (jvm/garbage-collector-stats))
       (update-in [:garbage-collector] assoc-delta)))


(defn deltas
  "Return a data map containing only the delta data.

  Discards all :start and :finish values, and moves :delta
  values up a level in the map."
  [data]
  (reduce-kv
    (fn [data k v]
      (if-let [delta (and (map? v) (:delta v))]
        (assoc data k delta)
        (assoc data k v)))
    {}
    data))

(comment
  (assoc-delta {:start {:a 1} :finish {:a 4}})

  (deltas
    (instrumented
      (with-garbage-collector-stats
        (with-finalization-count
          (with-compilation-time
            (with-class-loader-counts
              (with-time
                (with-expr-value (Thread/sleep 1)))))))))

  (deltas
    (instrumented
      (with-time
        (with-expr (Thread/sleep 1)))))

  )
