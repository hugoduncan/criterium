(ns criterium.util.t-digest.merging-digest
  "Implementation of the t-digest algorithm for streaming quantile estimation.
   Based on the MergingDigest variant from https://github.com/tdunning/t-digest"
  (:require
   [criterium.util.invariant :refer [have have?]]
   [criterium.util.t-digest.scale :as scale])
  (:import
   [criterium.util.t_digest.scale
    Scale]))

(defrecord Centroid
  [^double mean
   ^double weight])

(defrecord TDigest
  [^double compression
   ^clojure.lang.PersistentVector centroids    ;existing centroids
   ^clojure.lang.PersistentVector temp-centroids ;buffer for new points
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
    Double/POSITIVE_INFINITY  ; minimum
    Double/NEGATIVE_INFINITY  ; maximum
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
    sorted-centroids
    (let [total-weight (double  total-weight)
          normalizer   (scale/normalizer scale compression total-weight)
          k1           (scale/k scale 0.0 normalizer)
          w-limit      (* total-weight (scale/q scale (+ k1 1.0) normalizer))]
      (loop [result   []
             w-so-far 0.0
             [^Centroid c1 ^Centroid c2 & more :as _centroids]
             sorted-centroids]
        (if (nil? c2)
          (let [final (if c1 (conj result c1) result)]
            (have #(= total-weight %)
                  (reduce + 0.0 (mapv #(.weight ^Centroid %) final)))
            (have #(= 1.0 %)
                  (.weight ^Centroid (first final))
                  (.weight ^Centroid (peek final)))
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
             :centroids merged-centroids
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
         new-min         (if (Double/isInfinite minimum)
                       value
                       (min value minimum))
         new-max         (if (Double/isInfinite maximum)
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

(defn quantile
  "Returns estimated value at given quantile (0-1).
   Returns nil if digest is empty."
  ^double [^TDigest digest ^double q]
  {:pre [(have? #(<= 0.0 % 1.0) q)]}
  (let [{:keys [centroids] :as ^TDigest digest} (merge-new-values digest)]
    (when (seq centroids)
      (let [sorted-centroids         (sort-by :mean centroids)
            n                        (count sorted-centroids)
            total-weight             (.total-weight digest)
            minimum                  (.minimum digest)
            maximum                  (.maximum digest)
            ^Centroid first-centroid (first sorted-centroids)]
        (cond
          ;; no centroids or single centroid
          (<= n 0) Double/NaN
          (= n 1)  (.mean first-centroid)

          ;; multiple centroids
          :else
          (let [index (* q total-weight)]
            (cond
              ;; boundaries return min/max
              (< index 1)
              minimum

              ;; left centroid interpolation
              (and (> (.weight first-centroid) 1.0)
                   (< index (/ (.weight first-centroid) 2.0)))
              (+ minimum
                 (/ (* (- index 1)
                       (- (.mean first-centroid) minimum))
                    (- (/ (.weight first-centroid) 2.0) 1.0)))

              (> index (- total-weight 1.0))
              maximum

              ;; right centroid interpolation
              (and (> (.weight ^Centroid (last sorted-centroids)) 1)
                   (<= (- total-weight index)
                       (/ (.weight ^Centroid (last sorted-centroids)) 2)))
              (- maximum
                 (/ (* (- total-weight index 1)
                       (- maximum (.mean ^Centroid (last sorted-centroids))))
                    (- (/ (.weight ^Centroid (last sorted-centroids)) 2) 1)))

              ;; interpolate between centroids
              :else
              (loop [weight-so-far
                     (/ (.weight first-centroid) 2)
                     [^Centroid c1 ^Centroid c2 & rest] sorted-centroids]
                (let [dw (/ (+ (.weight c1) (.weight c2)) 2)]
                  (if (> (+ weight-so-far dw) index)
                    ;; centroids c1 and c2 bracket our point
                    (let [left-unit
                          (if (= (.weight c1) 1.0)
                            (if (< (- index weight-so-far) 0.5)
                              ;; within singleton's sphere
                              (reduced (.mean c1))
                              0.5)
                            0)
                          right-unit
                          (if (= (.weight c2) 1.0)
                            (if (<= (- (+ weight-so-far dw) index) 0.5)
                              ;; near singleton
                              (reduced (.mean c2))
                              0.5)
                            0)]
                      ;; handle early returns from unit weight checks
                      (if (reduced? left-unit)
                        (unreduced left-unit)
                        (if (reduced? right-unit)
                          (unreduced right-unit)
                          (let [z1 (- index weight-so-far ^double left-unit)
                                z2 (- (+ weight-so-far dw) index ^double right-unit)]
                            (weighted-average (.mean c1) z2 (.mean c2) z1)))))
                    (recur (+ weight-so-far dw) (into [c2] rest))))))))))))

(defn interpolate
  ^double [^Centroid left ^Centroid right ^double x]
  (let [left-weight   (.weight left)
        right-weight  (.weight right)
        left-single?  (= left-weight 1.0)
        right-single? (= right-weight 1.0)]
    ;; For singleton centroids, their entire weight is exactly at
    ;; the centroid and thus shouldn't be interpolated.
    (if (and left-single? right-single?)
      1.0
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
  ^double [^TDigest digest ^double x]
  (when (or (Double/isNaN x) (Double/isInfinite x))
    (throw (ex-info "Invalid value" {:x x})))

  (let [{:keys [centroids] :as ^TDigest digest} (merge-new-values digest)
        n                                       (count centroids)
        minimum                                 (.minimum digest)
        maximum                                 ( .maximum digest)
        total-weight                            (.total-weight digest)]
    (cond
      (zero? n) Double/NaN

      (= 1 n)  ; single centroid case
      (let [width (- maximum minimum)]
        (cond
          (< x minimum)  0.0
          (> x maximum)  1.0
          (<= width 0.0) 0.5  ; min ≈ max
          :else          (/ (- x minimum) width)))

      :else
      (let [sorted-centroids         (sort-by :mean centroids)
            ^Centroid first-centroid (first sorted-centroids)
            ^Centroid last-centroid  (last sorted-centroids)]
        (cond
          (< x minimum) 0.0
          (> x maximum) 1.0

                                        ; Left tail
          (< x (.mean first-centroid))
          (if (> (- (.mean first-centroid) minimum) 0)
            (if (= x minimum)
              (/ 0.5 total-weight)
              (/ (+ 1.0
                    (* (- x minimum)
                       (/ (- (/ (.weight first-centroid) 2) 1)
                          (- (.mean first-centroid) minimum))))
                 total-weight))
            0.0)

                                        ; Right tail
          (> x (.mean last-centroid))
          (if (> (- maximum (.mean last-centroid)) 0)
            (if (= x maximum)
              (- 1.0 (/ 0.5 total-weight))
              (- 1.0
                 (/ (+ 1.0
                       (* (- maximum x)
                          (/ (- (/ (.weight last-centroid) 2) 1)
                             (- maximum (.mean last-centroid)))))
                    total-weight)))
            1.0)

          :else
          ;; Main interpolation between centroids
          (loop [weight-so-far                      (/ (.weight first-centroid) 2)
                 [^Centroid c1 ^Centroid c2 & more] sorted-centroids]
            (cond
              (= (.mean c1) x)
              ;; Handle exact match
              (let [^double dw (loop [w                    (.weight c1)
                                      [^Centroid c & more] more]
                                 (if (and c (= (.mean c) x))
                                   (recur (+ w (.weight c)) more)
                                   w))]
                (/ (+ weight-so-far (/ dw 2)) total-weight))

              (and (<= (.mean c1) x) (< x (.mean c2)))
              ;; Interpolate between c1 and c2
              (/ (+ weight-so-far (interpolate c1 c2 x)) total-weight)

              :else
              (let [dw (/ (+ (.weight c1) (.weight c2)) 2)]
                (recur (+ weight-so-far dw) (into [c2] more))))))))))
