(ns criterium.random.ziggurat
  "Ziggurat algorithm for generating normal random variates.

  See: An improved Ziggurat method to generate normal random samples,
  Doornik, 2005"
  (:require
   [criterium.random.well :as well])
  (:import
   [criterium.random.well WellRng1024a]))

(def ^:dynamic ^Long *zignor-c* 128) ; "Number of blocks."
;; "Start of the right tail" (R * phi(R) + Pr(X>=R)) * sqrt(2\pi)
(def ^:dynamic ^Double *zignor-r* 3.442619855899e0)
(def ^:dynamic ^Double *zignor-v* 9.91256303526217e-3)

(defmacro sqr [x] `(let [x# ~x] (* x# x#)))

(defn zignor-init
  "Initialise tables."
  [c r v]
  (let [c                  (int c)
        r                  (double r)
        v                  (double v)
        #^doubles s-adzigx (double-array (inc c))
        #^doubles s-adzigr (double-array c)
        f                  (Math/exp (* -0.5e0 r r))]
    (aset s-adzigx 0 (/ v f)) ;; [0] is bottom block: V / f(R)
    (aset s-adzigx 1 r)
    (aset s-adzigx c 0.0)
    (loop [i (int 2)
           f f]
      (aset s-adzigx i
            (Math/sqrt (* -2e0 (Math/log (+ (/ v (aget s-adzigx (dec i))) f)))))
      (when (< i c)
        (recur
         (inc i)
         (Math/exp (* -0.5e0 (sqr (aget s-adzigx i)))))))

    (doseq [#^Integer i (range c)]
      (let [j (int i)]
        (aset s-adzigr j (/ (aget s-adzigx (inc j)) (aget s-adzigx j)))))
    [s-adzigr s-adzigx r (dec c)]))

;;; NormalRng implementation

(definterface INormalRng
  (^double nextGaussian []))

(deftype NormalRng [^WellRng1024a uniform-rng
                    ^doubles s-adzigr
                    ^doubles s-adzigx
                    ^double zignor-r
                    ^int mask]
  INormalRng
  (nextGaussian [_]
    (loop []
      (let [r (double (.nextDouble uniform-rng))
            u (double (- (* 2.0 r) 1.0))
            i (bit-and (int (* Integer/MAX_VALUE
                               (double (.nextDouble uniform-rng))))
                       mask)]
        ;; First try the rectangular boxes
        (if (< (Math/abs u) (aget s-adzigr i))
          (* u (aget s-adzigx i))

          ;; Bottom box: sample from the tail
          (if (zero? i)
            (let [negative (neg? u)]
              (loop []
                (let [l (Math/log (.nextDouble uniform-rng))
                      x (/ l zignor-r)
                      y (Math/log (.nextDouble uniform-rng))]
                  (if (>= (* -2.0 y) (* x x))
                    (if negative
                      (- x zignor-r)
                      (- zignor-r x))
                    (recur)))))

            ;; Sample from the wedges
            (let [x  (* u (aget s-adzigx i))
                  f0 (Math/exp (* -0.5 (- (sqr (aget s-adzigx i)) (sqr x))))
                  f1 (Math/exp (* -0.5 (- (sqr (aget s-adzigx (inc i)))
                                          (sqr x))))]
              (if (< (+ f1 (* (double (.nextDouble uniform-rng)) (- f0 f1)))
                     1.0)
                x
                (recur))))))))

  Object
  (toString [_]
    (str "#<NormalRng>")))

(defn next-gaussian!
  "Generate the next random gaussian in N(0,1), mutating the RNG state.
  Returns a double."
  ^double [^NormalRng rng]
  (.nextGaussian rng))

(defn make-normal-rng
  "Create a NormalRng instance for generating standard normal variates.
  Returns a mutable RNG that generates doubles from N(0,1) via next-gaussian!.

  Arities:
  - () - Uses default WELL RNG with standard ziggurat parameters
  - (uniform-rng) - Uses provided WellRng1024a with standard parameters
  - (uniform-rng c r v) - Uses provided RNG with custom ziggurat parameters

  Standard parameters: c=128 blocks, r=3.442619855899, v=9.91256303526217e-3

  See: An improved Ziggurat method to generate normal random samples,
  Doornik, 2005"
  ([]
   (make-normal-rng (well/make-well-rng-1024a)))
  ([uniform-rng]
   (make-normal-rng uniform-rng *zignor-c* *zignor-r* *zignor-v*))
  ([uniform-rng c r v]
   (let [[s-adzigr s-adzigx zr mask] (zignor-init c r v)]
     (->NormalRng uniform-rng s-adzigr s-adzigx zr mask))))

