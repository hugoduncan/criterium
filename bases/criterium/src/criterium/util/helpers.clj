(ns criterium.util.helpers)

(defn- safe-keys
  [m]
  (assert (or (map? m) (nil? m)) (pr-str m))
  {:pre [(or (map? m) (nil? m))]}
  (dissoc m :state :expr-value))

(defn- merge-fn [op]
  (fn merge-fn-inner [a b]
    (if (or (map? a) (map? b))
      (merge-with
       merge-fn-inner
       (safe-keys a)
       (safe-keys b))
      (op a b))))

(defn sum
  ([] {})
  ([a b]
   (merge-with
    (merge-fn +)
    (safe-keys a)
    (safe-keys b))))

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

;; from clojure 1.11-aplha-2
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

;; Modified version of clojure.walk to preserve metadata
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

;;; Value transforms

;; These allow sample values to be transformed to mesurements, and vice versa.
;; This enables, using a log-normal transform of the samples.

(defn add-transform-paths [v sample-> ->sample]
  (-> v
      (update :sample-> (fnil conj '()) sample->)
      (update :->sample (fnil conj '[]) ->sample)))

(defn get-transforms [sampled path]
  (loop [transforms (update-vals
                     (-> sampled (get path) meta :transform)
                     vector)
         path       (-> sampled (get path) meta :source-id)]
    (if path
      (let [t (-> sampled (get path) meta :transform)]
        (recur
         (add-transform-paths transforms (:sample-> t) (:->sample t))
         (-> sampled (get path) meta :source-id)))
      transforms)))

(defn transform-sample-> [value transforms]
  (reduce (fn [v f] (f v)) value (:sample-> transforms)))

(defn transform->sample [value transforms]
  (reduce (fn [v f] (f v)) value (reverse (:->sample transforms))))

;;; Thread
(defn valid-thread-priority
  [p]
  (when p
    (cond
      (= :max-priority p)            Thread/MAX_PRIORITY
      (= :min-priority p)            Thread/MIN_PRIORITY
      (and (integer? p)
           (<= Thread/MIN_PRIORITY
               p
               Thread/MAX_PRIORITY)) p
      :else
      (throw (ex-info "Invalid thread priority"
                      {:priority     p
                       :min-priority Thread/MIN_PRIORITY
                       :max-priority Thread/MAX_PRIORITY})))))

(defmacro with-thread-priority
  [p & body]
  `(let [priority#      (valid-thread-priority ~p)
         orig-priority# (.getPriority (Thread/currentThread))]
     (when priority#
       (.setPriority (Thread/currentThread) priority#))
     (try
       ~@body
       (finally
         (when priority#
           (.setPriority (Thread/currentThread) orig-priority#))))))
;;; Resolve

(defn maybe-ver-get-named
  "Resolve and deref a Named, or return the argument."
  [k {:keys [default-ns]}]
  {:pre [(or (keyword? k) (symbol? k))]}
  (if-let [n (namespace k)]
    (or (some-> (ns-resolve (symbol n) (symbol (name k))) deref) k)
    (or (some-> (ns-resolve default-ns (symbol (name k))) deref) k)))

(defn maybe-var-get
  "Resolve and deref a Named, or return the argument."
  [x options]
  (if (or (keyword? x) (symbol? x))
    (maybe-ver-get-named x options)
    x))
