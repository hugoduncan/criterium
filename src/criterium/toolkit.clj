;;;; Copyright (c) Hugo Duncan. All rights reserved.

;;;; The use and distribution terms for this software are covered by the
;;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;; which can be found in the file epl-v10.html at the root of this distribution.
;;;; By using this software in any fashion, you are agreeing to be bound by
;;;; the terms of this license.
;;;; You must not remove this notice, or any other, from this software.

(ns criterium.toolkit
  "Standalone instrumentation"
  (:require [criterium.jvm :as jvm]))


(defn assoc-delta
  "Assoc finish merged with start using op, onto the :delta key."
  [{:keys [start finish] :as data} op]
  (assoc data :delta (merge-with op finish start)))


(defn assoc-delta-minus
  "Assoc finish merged with start using op, onto the :delta key."
  [data]
  (assoc-delta data -))


;; Macros to wrap an expr execution.  The macros use a first data
;; argument to accumulate data about the expr evaluation.


(defmacro instrumented
  "Introduces an instrumented expression expr.

  The expr contains nested with-* macro calls, and returns
  a map with the collected data.
  "
  [expr]
  `(-> {} ~expr))


(defmacro with-return-value
  "Execute expr, adding the return value to the data map's :return-value key."
  [data expr]
  `(assoc ~data :return-value ~expr))

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
     (update-in [:time] assoc-delta-minus)))


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
       (update-in [:class-loader] assoc-delta-minus)))


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
       (update-in [:compilation] assoc-delta-minus)))


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
  (assoc-delta-minus {:start {:a 1} :finish {:a 4}})

  (deltas
    (instrumented
      (with-compilation-time
        (with-class-loader-counts
          (with-time
            (with-return-value 1))))))

  )
