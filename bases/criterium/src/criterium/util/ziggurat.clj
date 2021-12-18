(ns criterium.util.ziggurat
  (:require
   [criterium.util.well :as well]))

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
    (aset s-adzigx c (double 0.0))
    (loop [i (int 2)
           f f]
      (aset s-adzigx i
            (Math/sqrt (* -2e0 (Math/log (+ (/ v (aget s-adzigx (dec i))) f)))))
      (when (< i c)
        (recur
         (inc i)
         (Math/exp (* -0.5e0 (sqr (aget s-adzigx i)))))))

    (for [#^Integer i (range c)]
      (let [j (int i)]
        (aset s-adzigr j (/ (aget s-adzigx (inc j)) (aget s-adzigx j)))))
    [s-adzigr s-adzigx r (dec c)]))

(defn random-normal-zig
  "Pseudo-random normal variates.
  An implementation of ZIGNOR
  See:
    An improved Ziggurat method to generate normal random samples, Doornik, 2005"
  ([]
   (random-normal-zig (well/well-rng-1024a)
                      (zignor-init *zignor-c* *zignor-r* *zignor-v*)))
  ([rng-seq]
   (random-normal-zig rng-seq (zignor-init *zignor-c* *zignor-r* *zignor-v*)))
  ([rng-seq c r v] (random-normal-zig rng-seq (zignor-init c r v)))
  ([c r v]
   (random-normal-zig (well/well-rng-1024a) (zignor-init c r v)))
  ([rng-seq [#^doubles s-adzigr #^doubles s-adzigx ^Double zignor-r mask]]
   (letfn [(random-normal-tail
             [^double min-r negative rng-seq]
             (loop [rng-seq rng-seq]
               (let [l (Math/log ^Double (first rng-seq))
                     x (/ l min-r)
                     y (Math/log (first (next rng-seq)))]
                 (if (>= (* -2e0 y) (* x x))
                   (if negative
                     [(- x min-r) (drop 2 rng-seq)]
                     [(- min-r x) (drop 2 rng-seq)])
                   (recur (drop 2 rng-seq))))))]
     (let [zignor-r (double zignor-r)
           mask     (int mask)
           [deviate rng-seq]
           (loop [rng-seq rng-seq]
             (let [r (double (first rng-seq))
                   u (double (- (* 2e0 r) 1e0))
                   i (bit-and
                      (int (* Integer/MAX_VALUE
                              (double ^Double (first (drop 1 rng-seq)))))
                      mask)]
               ;; first try the rectangular boxes
               (if (< (Math/abs u) (aget s-adzigr i))
                 [(* u (aget s-adzigx i)) (drop 2 rng-seq)]

                 ;; bottom box: sample from the tail
                 (if (zero? i)
                   (random-normal-tail zignor-r (neg? u) (drop 2 rng-seq))

                   ;; is this a sample from the wedges?
                   (let [x  (* u (aget s-adzigx i))
                         f0 (Math/exp
                             (* -0.5e0
                                (- (sqr (aget s-adzigx i)) (sqr x))))
                         f1 (Math/exp
                             (* -0.5e0
                                (- (sqr (aget s-adzigx (inc i)))
                                   (sqr x))))]
                     (if  (< (+ f1 (* (double ^Double (first (drop 2 rng-seq)))
                                      (- f0 f1)))
                             1.0)
                       [x (drop 3 rng-seq)]
                       (recur (drop 3 rng-seq))))))))]
       (lazy-seq (cons deviate (random-normal-zig rng-seq)))))))
