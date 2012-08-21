;;;; Copyright (c) Hugo Duncan. All rights reserved.

;;;; The use and distribution terms for this software are covered by the
;;;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;;; which can be found in the file epl-v10.html at the root of this distribution.
;;;; By using this software in any fashion, you are agreeing to be bound by
;;;; the terms of this license.
;;;; You must not remove this notice, or any other, from this software.

;;; Implementation of ZIGNOR
;;; An improved Ziggurat method to generate normal random samples, Doornik, 2005

(ns criterium.ziggurat
  (:require criterium.well))

(def ^:dynamic *zignor-c* 128 ) ; "Number of blocks."
 ; "Start of the right tail" (R * phi(R) + Pr(X>=R)) * sqrt(2\pi)
(def ^:dynamic *zignor-r* 3.442619855899e0)
(def ^:dynamic *zignor-v* 9.91256303526217e-3)

(defn- sqr [x] (* x x))

(defn zignor-init
  "Initialise tables."
  [c r v]
  (let [c (int c)
        r (double r)
        v (double v)
        #^doubles s-adzigx (double-array (inc c))
        #^doubles s-adzigr (double-array c)
        f (Math/exp (* -0.5e0 r r))]
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
         (Math/exp (* -0.5e0 (aget s-adzigx i) (aget s-adzigx i))))))

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
     (random-normal-zig (criterium.well/well-rng-1024a)
                        (zignor-init *zignor-c* *zignor-r* *zignor-v*)))
  ([rng-seq]
     (random-normal-zig rng-seq (zignor-init *zignor-c* *zignor-r* *zignor-v*)))
  ([rng-seq c r v] (random-normal-zig rng-seq (zignor-init c r v)))
  ([c r v]
     (random-normal-zig (criterium.well/well-rng-1024a) (zignor-init c r v)))
  ([rng-seq [#^doubles s-adzigr #^doubles s-adzigx zignor-r mask]]
     (letfn [(random-normal-tail
               [min negative rng-seq]
               (loop [rng-seq rng-seq]
                 (let [x (/ (Math/log (first rng-seq)) min)
                       y (Math/log (first (next rng-seq)))]
                   (if (>= (* -2e0 y) (* x x))
                     (if negative
                       [(- x min) (drop 2 rng-seq)]
                       [(- min x) (drop 2 rng-seq)])
                     (recur (drop 2 rng-seq))))))]
       (let [[deviate rng-seq]
             (loop [rng-seq rng-seq]
               (let [r  (first rng-seq)
                     u  (double (- (* 2e0 r) 1e0))
                     i  (bit-and
                         (int (* Integer/MAX_VALUE (first (drop 1 rng-seq))))
                         mask)]
                 ;; first try the rectangular boxes
                 (if (< (Math/abs u) (nth s-adzigr i))
                   [(* u (nth s-adzigx i)) (drop 2 rng-seq)]

                   ;; bottom box: sample from the tail
                   (if (zero? i)
                     (random-normal-tail zignor-r (neg? u) (drop 2 rng-seq))

                     ;; is this a sample from the wedges?
                     (let [x (* u (nth s-adzigx i))
                           f0 (Math/exp
                               (* -0.5e0
                                  (- (Math/pow (nth s-adzigx i) 2) (sqr x))))
                           f1 (Math/exp
                               (* -0.5e0
                                  (- (Math/pow (nth s-adzigx (inc i)) 2)
                                     (sqr x))))]
                       (if  (< (+ f1 (* (first (drop 2 rng-seq) ) (- f0 f1)))
                               1.0)
                         [x (drop 3 rng-seq)]
                         (recur (drop 3 rng-seq) )))))))]
         (lazy-seq (cons deviate (random-normal-zig rng-seq)))))))
