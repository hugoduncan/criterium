(ns criterium.util.t-digest.scale
  "Scale functions for t-digest algorithm.
   These control how cluster sizes are determined and affect accuracy in different ways."
  (:require    [criterium.util.invariant :refer [have?]]))

(defn finite? [^double x]
  (and (not (NaN? x)) (not (infinite? x))))

(definterface Scale
  ;; "Convert quantile q to k-scale value using normalized compression."
  (k [^double q ^double normalizer])
  (k [^double q ^double compression ^double n])
  ;; "Convert k-scale value back to quantile q using normalized compression."
  (q [^double k ^double normalizer])
  (q [^double k ^double compression ^double n])
  ;; "Maximum allowed cluster size at quantile q using normalized compression."
  (max_size [^double q ^double normalizer])
  (max_size [^double q ^double compression ^double n])
  ;; "Normalizing factor for compression."
  (normalizer [^double compression ^double n]))

(defn k
  "Convert quantile q to k-scale value using normalized compression."
  (^double [^Scale scale ^double q ^double normalizer]
   {:pre  [(have? finite? q) (have? finite? normalizer)]
    :post [#(have? finite? %)]}
   (.k scale q normalizer))
  (^double [^Scale scale ^double q ^double compression ^double n]
   {:pre  [(have? finite? q) (have? finite? compression) (have? finite? n)]
    :post [#(have? finite? %)]}
   (.k scale q compression n)))

(defn q
  "Convert k-scale value back to quantile q using normalized compression."
  (^double [^Scale scale ^double k ^double normalizer]
   {:pre  [(have? finite? k) (have? finite? normalizer)]
    :post [#(have? finite? %)]}
   (.q scale k normalizer))
  (^double [^Scale scale ^double k ^double compression ^double n]
   {:pre  [(have? finite? k) (have? finite? compression) (have? finite? n)]
    :post [#(have? finite? %)]}
   (.q scale k compression n)))

(defn max-size
  "Maximum allowed cluster size at quantile q using normalized compression."
  (^double [^Scale scale ^double q ^double normalizer]
   {:pre  [(have? finite? q) (have? finite? normalizer)]
    :post [#(have? finite? %)]}   (.max_size scale q normalizer))
  (^double [^Scale scale ^double q ^double compression ^double n]
   {:pre  [(have? finite? q) (have? finite? compression) (have? finite? n)]
    :post [#(have? finite? %)]}
   (.max_size scale q compression n)))

(defn normalizer
  ;; "Normalizing factor for compression."
  ^double [^Scale scale ^double compression ^double n]
  {:pre  [(have? finite? compression) (have? finite? n)]
   :post [#(have? finite? %)]}
  (.normalizer scale compression n))

(defn limit ^double [^double x ^double low ^double high]
  (cond
    (< x low)  low
    (> x high) high
    :else      x))

(defn bound [^double x]
  (cond
    (< x 0.0) 0.0
    (> x 1)   1
    :else     x))

(def k0
  "Scale function that generates uniform cluster sizes.
   Used mainly for testing and comparison."
  (reify Scale
    (k [_  q  normalizer]
      (* normalizer q))
    (k [_  q  compression  _n]
      (/ (* compression q) 2.0))

    (q [_  k  normalizer]
      (/ k normalizer))
    (q [_  k  compression  _]
      (/ (* 2 k) compression))

    (max_size [_ _ normalizer]
      (/ 1.0 normalizer))
    (max_size [_ _ compression _n]
      (/ 2.0 compression))

    (normalizer [_ compression _n]
      (/ compression 2.0))))

(def ^:const k1-q-limit-low 1e-15)
(def ^:const k1-q-limit-high (- 1.0 1e-15))

(def ^:const k1-k-limit-f-low (/ (Math/asin (- (* 2.0 k1-q-limit-low) 1.0))
                                 (* 2.0 Math/PI)))
(def ^:const k1-q-limit-f-high (/ (Math/asin (- (* 2.0 k1-q-limit-high) 1.0))
                                  (* 2.0 Math/PI)))

(def ^:const k1-x-limit-low (Math/asin (- (* 2.0 k1-q-limit-low) 1.0)))
(def ^:const k1-x-limit-high (Math/asin (- (* 2.0 k1-q-limit-high) 1.0)))

(def k1
  "Scale function that generates cluster sizes proportional to sqrt(q*(1-q)).
  Gives constant relative accuracy if accuracy is proportional to
  squared cluster size."
  (reify Scale
    (k [_ q compression _n]
      (let [q (limit q k1-q-limit-low k1-q-limit-high)]
        (/ (* compression (Math/asin (- (* 2.0 q) 1.0)))
           (* 2.0 Math/PI))))
    (k [_ q normalizer]
      (let [q (limit q k1-q-limit-low k1-q-limit-high)]
        (* normalizer (Math/asin (- (* 2.0 q) 1.0)))))

    (q [_ k compression _n]
      (let [k (limit
               k
               (* compression k1-q-limit-f-high)
               (* compression k1-q-limit-high))]
        (/ (+ 1.0 (Math/sin  (* k (/ (* 2.0 Math/PI) compression))) 1.0) 2.0)))

    (q [_ k normalizer]
      (let [x (/ k normalizer)
            x (limit x k1-x-limit-low k1-x-limit-high)]
        (/ (+ (Math/sin x) 1.0) 2.0)))

    (max_size [_ q compression _n]
      (if (or (<= q 0.0) (>= q 1.0))
        0.0
        (* 2.0
           (Math/sin (/ Math/PI compression))
           (Math/sqrt (* q (- 1.0 q))))))
    (max_size [_ q normalizer]
      (if (or (<= q 0.0) (>= q 1.0))
        0.0
        (* 2.0
           (Math/sin (/ 0.5 normalizer))
           (Math/sqrt (* q (- 1.0 q))))))

    (normalizer [_ compression _n]
      (/ compression (* 2.0 Math/PI)))))

;; (def ^:const xx (Math/asin (- (* 2.0 1e-15) 1.0)))

;; (Math/asin -1.0)
;; (/ Math/PI 2)
;; (doseq [^double c (range 1.0 10.0 1.0)]
;;   (prn)
;;   (prn :c c)
;;   (prn :k-limit (k k1 0.0 c))
;;   (prn :q-for-k-limit (q k1 (k k1 0.0 c) c))
;;   (prn :k-limit' (/ c 4.0))
;;   (prn :xx (/ c xx)))

(defn- zk2 ^double [^double compression ^double n]
  (+ (* 4.0 (Math/log (/ n compression))) 24.0))

(def k2
  "Scale function that generates cluster sizes proportional to q*(1-q).
   Makes tail error bounds tighter than K1."
  (reify Scale
    (k [_ q compression n]
      (if (<= n 1.0)
        (cond
          (<= q 0.0) -10.0
          (>= q 1.0) 10.0
          :else      0.0)
        (let [q (limit q k1-q-limit-low k1-q-limit-high)]
          (/ (* compression (Math/log (/ q (- 1.0 q))))
             (zk2 compression n)))))
    (k [_ q normalizer]
      (let [q (limit q k1-q-limit-low k1-q-limit-high)]
        (* normalizer (Math/log (/ q (- 1.0 q))))))

    (q [_ k compression n]
      (let [w (Math/exp (/ (* k (zk2 compression n)) compression))]
        (if (infinite? w)
          1.0
          (/ w (+ 1.0 w)))))

    (q [_ k normalizer]
      (let [w (Math/exp (/ k normalizer))]
        (if (infinite? w)
          1.0
          (/ w (+ 1.0 w)))))

    (max_size [_ q compression n]
      (/ (* (zk2 compression n) q (- 1.0 q)) compression))

    (max_size [_ q normalizer]
      (/ (* q (- 1.0 q)) normalizer))

    (normalizer [_ compression n]
      (/ compression (zk2 compression n)))))
