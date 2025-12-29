(ns utils.helpers
  "Generic utility functions."
  (:refer-clojure :exclude [update-vals]))

(defn assoc-tag
  "Associate a type tag to a symbol's metadata."
  [sym t]
  (vary-meta sym assoc :tag t))

(defn spy
  "Debug helper: print message and value, return value."
  [msg x]
  (prn msg x)
  x)

(defmacro sqr
  "Square of argument"
  [x] `(let [x# ~x] (* x# x#)))

(defn sqrd
  "Square of argument"
  ^double [^double x] (* x x))

(defn cubed
  "Cube of argument"
  ^double [^double x]
  (* x x x))

(defn trunc
  "Round towards zero to an integeral value."
  [^double x]
  (if (pos? x)
    (Math/floor x)
    (Math/ceil x)))

;;; update-vals polyfill for pre-1.11 Clojure

(def update-vals-impl
  '(with-meta
     (persistent!
      (reduce-kv (fn [acc k v] (assoc! acc k (f v)))
                 (if (instance? clojure.lang.IEditableCollection m)
                   (transient m)
                   (transient {}))
                 m))
     (meta m)))

(defmacro provide-update-vals
  []
  (if (resolve 'clojure.core/update-vals)
    '(clojure.core/update-vals m f)
    update-vals-impl))

#_{:clj-kondo/ignore [:redefined-var :unused-binding]}
(defn update-vals
  "m f => {k (f v) ...}

  Given a map m and a function f of 1-argument, returns a new map where
  the keys of m are mapped to result of applying f to the corresponding
  values of m."
  [m f]
  (provide-update-vals))

(defn filter-map
  "Filter map entries based on a predicate applied to values.

  Return a new map containing only the entries where (pred value)
  returns true."
  [pred m]
  (into {} (filter (comp pred val)) m))

(defn reduce-double-vector
  "Reduce a double primitive value over a vector."
  ^double [^clojure.lang.IFn$DOD f
           ^double init
           ^clojure.lang.APersistentVector v]
  (let [n (.count v)]
    (loop [acc init
           i 0]
      (if (< i n)
        (recur (.invokePrim f acc (.nth v i)) (unchecked-inc i))
        acc))))

;;; Modified version of clojure.walk to preserve metadata

(defn walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall."

  {:added "1.1"}
  [inner outer form]
  (cond
    (list? form)
    (outer (with-meta
             (apply list (map inner form))
             (meta form)))

    (instance? clojure.lang.IMapEntry form)
    (outer
     (clojure.lang.MapEntry/create
      (inner (key form)) (inner (val form))))

    (seq? form)
    (outer (with-meta
             (doall (map inner form))
             (meta form)))

    (instance? clojure.lang.IRecord form)
    (outer (reduce (fn [r x] (conj r (inner x))) form form))

    (coll? form)
    (outer (with-meta
             (into (empty form) (map inner form))
             (meta form)))
    :else (outer form)))

(defn postwalk
  "Performs a depth-first, post-order traversal of form.  Calls f on
  each sub-form, uses f's return value in place of the original.
  Recognizes all Clojure data structures. Consumes seqs as with doall."
  {:added "1.1"}
  [f form]
  (walk (partial postwalk f) f form))

(defn deep-merge
  "Merge maps recursively."
  [& ms]
  (letfn [(merge* [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with merge* xs)
              (last xs)))]
    (reduce merge* ms)))

(defn report
  "Print format output"
  [format-string & values]
  (print (apply format format-string values)))
