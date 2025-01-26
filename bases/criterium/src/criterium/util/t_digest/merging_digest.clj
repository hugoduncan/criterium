(ns criterium.util.t-digest.merging-digest
  "Implementation of the t-digest algorithm for streaming quantile estimation.
   Based on the MergingDigest variant from https://github.com/tdunning/t-digest"
  (:require
   [criterium.util.forms :refer [cond*]]
   [criterium.util.helpers :as util]
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.t-digest.scale :as scale :refer [finite?]])
  (:import
   [clojure.lang
    IPersistentVector]
   [criterium.util.t_digest.scale
    Scale]))

(defrecord Centroid
  [^double mean
   ^double weight])

(defn centroid-weight
  ^double [^Centroid centroid]
  (.weight centroid))

(defn centroid-mean
  ^double [^Centroid centroid]
  (.mean centroid))

(defrecord TDigest
  [^double compression
   ^IPersistentVector centroids    ;existing centroids
   ^IPersistentVector temp-centroids ;buffer for new points
   ^double total-weight
   ^double unmerged-weight
   ^double minimum
   ^double maximum
   ^Scale scale
   ^long buffer-size])

(def ^:private default-compression 100.0)

(def ^:private default-buffer-size 128)

(defn new-digest
  "Creates a new t-digest with given compression factor."
  ([] (new-digest default-compression))
  ([compression]
   (new-digest compression default-buffer-size))
  ([compression buffer-size]
   (->TDigest
    (double compression)
    []    ; centroids
    []    ; temp-centroids
    0.0   ; total-weight
    0.0   ; unmerged-weight
    Double/NaN  ; minimum
    Double/NaN ; maximum
    scale/k2
    buffer-size)))

(defn- weighted-average
  "Compute weighted average of two points"
  ^double [^double x1 ^double w1 ^double x2 ^double w2]
  (/ (+ (* x1 w1) (* x2 w2))
     (+ w1 w2)))

(defn- merge-centroids
  "Merges centroids while maintaining t-digest invariants."
  [compression sorted-centroids total-weight scale
   & {:keys [use-weight-limit] :or {use-weight-limit true}}]
  (if (<= (count sorted-centroids) 1)
    (vec sorted-centroids)
    (let [total-weight (double  total-weight)
          normalizer   (scale/normalizer scale compression total-weight)
          k1           (scale/k scale 0.0 normalizer)
          w-limit      (* total-weight (scale/q scale (+ k1 1.0) normalizer))]
      (loop [result   []
             w-so-far 0.0
             [^Centroid c1 ^Centroid c2 & more :as _centroids]
             sorted-centroids]
        (have vector? result)
        (if (nil? c2)
          (let [final (if c1 (conj result c1) result)]
            (have #(= total-weight %)
                  (reduce + 0.0 (mapv #(.weight ^Centroid %) final)))
            #_(have #(= 1.0 %)
                    (.weight ^Centroid (first final))
                    {:final final})
            #_(have #(= 1.0 %)
                    (.weight ^Centroid (peek final))
                    {:final final})
            final)
          (let [proposed-weight (+ (.weight c1) (.weight c2))
                add-this?
                (cond
                  (= (count result) 0) ; first iteration
                  false
                  (empty? more)       ; last pair
                  false
                  :else
                  (let [projected-w (+ w-so-far proposed-weight)]
                    (if use-weight-limit
                      (let [q0 (/ w-so-far total-weight)
                            q2 (/ projected-w total-weight)]
                        (<= proposed-weight
                            (* total-weight
                               (min
                                (scale/max-size scale q0 normalizer)
                                (scale/max-size scale q2 normalizer)))))
                      (<= projected-w w-limit))))]
            (if add-this?
              ;; merge c2 into c1
              (recur result
                     w-so-far
                     (into [(->Centroid
                             (weighted-average
                              (.mean c1) (.weight c1)
                              (.mean c2) (.weight c2))
                             proposed-weight)]
                           more))
              ;; emit c1, continue with rest
              (recur (conj result c1)
                     (+ w-so-far (.weight ^Centroid c1))
                     (into [c2] more)))))))))

(defn- merge-new-values
  "Merges any buffered points into the digest."
  [{:keys [temp-centroids centroids] :as ^TDigest digest}]
  (if (seq temp-centroids)
    (let [sorted-centroids (sort-by :mean temp-centroids)
          total-weight     (.total-weight digest)
          unmerged-weight  (.unmerged-weight digest)
          compression      (.compression digest)
          new-total-weight (+ total-weight unmerged-weight)
          merged-centroids (merge-centroids
                            compression
                            (sort-by :mean (into sorted-centroids centroids))
                            new-total-weight
                            (:scale digest))]
      (assoc digest
             :centroids (have vector? merged-centroids)
             :temp-centroids []
             :total-weight new-total-weight
             :unmerged-weight 0.0))
    digest))

(defn add-point
  "Adds a single value with weight to the digest"
  ([^Centroid digest value]
   (add-point digest value 1.0))
  ([{:keys [temp-centroids] :as ^TDigest digest}
    ^double value
    ^double weight]
   (when (Double/isNaN value)
     (throw (ex-info "Cannot add NaN to t-digest" {:value value})))
   (let [buffer-size     (.buffer-size digest)
         ^TDigest digest (if (>= (count temp-centroids) buffer-size)
                           (merge-new-values digest)
                           digest)
         minimum         (.minimum digest)
         maximum         (.maximum digest)
         new-min         (if (NaN? minimum)
                           value
                           (min value minimum))
         new-max         (if (NaN? maximum)
                           value
                           (max value maximum))]
     (-> digest
         (update :temp-centroids conj (->Centroid value weight))
         (update :unmerged-weight + weight)
         (assoc :minimum new-min
                :maximum new-max)))))

(defn compress
  "Merges any buffered points into the digest."
  [digest]
  (merge-new-values digest))

(defn vfirst
  [^IPersistentVector v]
  (.nth v 0))

(defn vsecond
  [^IPersistentVector v]
  (.nth v 1))

(defn vpeek
  [^IPersistentVector v]
  (.peek v))

;; NOTE clojure's primitive functions can only have four arguments
(defn- relative-pos ^double [^double x0 ^double x1 ^double x]
  (/ (- x x0) (- x1 x0)))

(defn- interpolate-rel
  ^double [^double v0 ^double v1 ^double t]
  (+ v0 (* (- v1 v0) t)))

(defmacro interpolate [v0 v1 x0 x1 x]
  `(interpolate-rel ~v0 ~v1 (relative-pos ~x0 ~x1 ~x)))

(defn quantile
  "Returns estimated value at given quantile (0-1).
   Returns NaN if digest is empty."
  ^double [{:keys [^IPersistentVector  centroids] :as ^TDigest digest}
           ^double q]
  {:pre [(have? #(<= 0.0 % 1.0) q)
         (have? digest)
         (have? vector? centroids)]}
  (let [n            (count centroids)
        total-weight (.total-weight digest)
        minimum      (.minimum digest)
        maximum      (.maximum digest)]
    (have finite? total-weight)
    (cond*
      ;; no centroids or single centroid
      (= n 0) Double/NaN

      :let    [^Centroid first-centroid (vfirst centroids)]
      (= n 1) (.mean first-centroid)

      ;; multiple centroids
      :else
      (let [index (* q total-weight)]
        (cond*
          ;; Boundaries return min/max
          (< index 1.0)
          minimum

          (> index (- total-weight 1.0))
          maximum

          ;; left centroid interpolation
          :let [first-weight (.weight first-centroid)]
          (and (> first-weight 1.0)
               (< index (/ first-weight 2.0)))
          (interpolate
           minimum
           (.mean first-centroid)
           0.0
           (/ first-weight 2.0)
           index)

          ;; right centroid interpolation
          :let [^Centroid last-centroid (.peek centroids)
                last-weight (.weight last-centroid)]
          (and (> last-weight 1)
               (<= (- total-weight index)
                   (/ last-weight 2.0)))
          (interpolate
           (.mean last-centroid)
           maximum
           (- total-weight last-weight)
           total-weight
           index)

          ;; interpolate between centroids
          :else
          (loop [weight-so-far (/ first-weight 2.0)
                 centroids     centroids]
            (let [^Centroid c1 (vfirst centroids)
                  ^Centroid c2 (vsecond centroids)
                  dw           (/ (+ (.weight c1) (.weight c2)) 2)
                  nextw        (+ weight-so-far dw)]
              (if (<= nextw index)
                (recur nextw (subvec centroids 1))
                ;; centroids c1 and c2 bracket our point
                (let [left-unit?  (= (.weight c1) 1.0)
                      right-unit? (= (.weight c2) 1.0)]
                  (cond
                    (and left-unit? (< (- index weight-so-far) 0.5))
                    (.mean c1)
                    (and right-unit? (<= (- nextw index) 0.5))
                    (.mean c2)
                    :else
                    (let [z1 (- index weight-so-far)
                          z2 (- (+ weight-so-far dw) index)]
                      (weighted-average
                       (.mean c1) z2
                       (.mean c2) z1))))))))))))

(defn interpolate-centroids
  ^double [^Centroid left ^Centroid right ^double x]
  (let [left-weight   (.weight left)
        right-weight  (.weight right)
        left-single?  (= left-weight 1.0)
        right-single? (= right-weight 1.0)]
    ;; For singleton centroids, their entire weight is exactly at
    ;; the centroid and thus shouldn't be interpolated.
    (if (and left-single? right-single?)
      0.5
      (let [left-x  (.mean left)
            right-x (.mean right)
            base    (if left-single? 0.5 0.0)
            dw      (/ (+ (if left-single? 0.0 left-weight)
                          (if right-single? 0.0 right-weight))
                       2.0)]
        (+ base (* dw (/ (- x left-x) (- right-x left-x))))))))

(defn cdf
  "Returns cumulative probability at x.
   Returns NaN if digest is empty."
  ^double [{:keys [centroids] :as ^TDigest digest} ^double x]
  (when (or (Double/isNaN x) (Double/isInfinite x))
    (throw (ex-info "Invalid value" {:x x})))

  (let [n            (count centroids)
        minimum      (.minimum digest)
        maximum      ( .maximum digest)
        total-weight (.total-weight digest)]
    (cond*
      (zero? n) Double/NaN

      (= 1 n)  ; single centroid case
      (let [width (- maximum minimum)]
        (cond
          (< x minimum)  0.0
          (> x maximum)  1.0
          (<= width 0.0) 0.5  ; min ≈ max
          :else          (/ (- x minimum) width)))

      (< x minimum) 0.0
      (> x maximum) 1.0

      :let [^Centroid first-centroid (vfirst centroids)
            first-mean (.mean first-centroid)]

      ;; Left tail
      (< x first-mean)
      (if (> first-mean minimum)
        (if (= x minimum)
          (/ 0.5 total-weight)
          (/ (interpolate
              0.0
              (/ (.weight first-centroid) 2.0)
              minimum
              first-mean
              x)
             total-weight))
        0.0)

      :let [^Centroid last-centroid (vpeek centroids)
            last-mean (.mean last-centroid)]

      ;; Right tail
      (> x last-mean)
      (if (> maximum last-mean)
        (if (= x maximum)
          (- 1.0 (/ 0.5 total-weight))
          (- 1.0
             (/ (interpolate
                 (- total-weight (/ (.weight last-centroid) 2.0))
                 total-weight
                 last-mean
                 maximum
                 x)
                total-weight)))
        1.0)

      :else
      ;; Main interpolation between centroids
      (loop [weight-so-far (/ (.weight first-centroid) 2)
             centroids     centroids]
        (let [^Centroid c1 (vfirst centroids)]
          (cond*
            (= (.mean c1) x)
            ;; Handle exact match
            (let [more       (when (> (count centroids) 2)
                               (subvec centroids 2))
                  ^double dw (loop [w  (.weight c1)
                                    cs more]
                               (let [^Centroid c (and (pos? (count cs))
                                                      (vfirst cs))]
                                 (if (and c (= (.mean c) x))
                                   (recur (+ w (.weight c)) (subvec cs 1))
                                   w)))]
              (/ (+ weight-so-far (/ dw 2)) total-weight))

            :let [^Centroid c2 (vsecond centroids)]
            (and (<= (.mean c1) x) (< x (.mean c2)))
            ;; Interpolate between c1 and c2
            (/ (+ weight-so-far
                  (interpolate-centroids c1 c2 x))
               total-weight)

            :else
            (let [dw (/ (+ (.weight c1) (.weight c2)) 2.0)]
              (recur (+ weight-so-far dw) (subvec centroids 1)))))))))

(defn compressed?
  [{:keys [temp-centroids] :as _digest}]
  (empty temp-centroids))

(defn- transform-centroid
  [f ^Centroid centroid]
  (update centroid :mean f))

(defn transform
  [{:keys [buffer-size
           compression
           centroids]
    :as   ^TDigest digest} f]
  (have compressed? digest)
  (->TDigest
   (double compression)
   (mapv (partial transform-centroid f) centroids) ; centroids
   []                                   ; temp-centroids
   (.total-weight digest) ; total-weight
   0.0                                  ; unmerged-weight
   (f (.minimum digest))                          ; minimum
   (f (.maximum digest))                         ; maximum
   scale/k2
   buffer-size))

(defn sample-count
  ^double [^TDigest digest]
  (.total-weight digest))

(defn minimum
  ^double [^TDigest digest]
  (.minimum digest))

(defn maximum
  ^double [^TDigest digest]
  (.maximum digest))

(defn mean
  ^double [{:keys [centroids] :as ^TDigest digest}]
  (let [sum-weights  (.total-weight digest)
        weighted-sum (util/reduce-double-vector
                      (fn ^double [^double acc ^Centroid centroid]
                        (+ acc (* (.mean centroid) (.weight centroid))))
                      0.0
                      centroids)]
    (/ weighted-sum sum-weights)))

(defn variance
  (^double [digest]
   (variance digest (mean digest)))
  (^double [{:keys [centroids] :as ^TDigest digest} ^double mean]
   (let [sum-weights (.total-weight digest)
         sum-squares (util/reduce-double-vector
                      (fn ^double [^double acc ^Centroid centroid ]
                        (+ acc
                           (* (* (.mean centroid)
                                 (.mean centroid))
                              (.weight centroid))))
                      0.0
                      centroids)
         e-x-squared (/ sum-squares sum-weights)]
     (- e-x-squared (* mean mean)))))

(defn centroid-means
  [digest]
  (mapv centroid-mean (:centroids digest)))
