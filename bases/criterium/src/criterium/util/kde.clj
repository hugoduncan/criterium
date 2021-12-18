(ns criterium.util.kde)

;; Mellin-Meijer-kernel density estimation on R+. Gery Geenens∗ School of
;; Mathematics and Statistics, UNSW Sydney, Australia July 17, 2017

(defn gamma-fn
  "Returns Gamma(z + 1 = number) using Lanczos approximation.
  Taken from rosettacode."
  [number]
  (if (< number 0.5)
    (/ Math/PI (* (Math/sin (* Math/PI number))
                  (gamma-fn (- 1 number))))
    (let [n (dec number)
          c [0.99999999999980993 676.5203681218851 -1259.1392167224028
             771.32342877765313 -176.61502916214059 12.507343278686905
             -0.13857109526572012 9.9843695780195716e-6 1.5056327351493116e-7]]
      (* (Math/sqrt (* 2 Math/PI))
         (Math/pow (+ n 7 0.5) (+ n 0.5))
         (Math/exp (- (+ n 7 0.5)))
         (+ (first c)
            (apply + (map-indexed #(/ %2 (+ n %1 1)) (next c))))))))

(comment
  (gamma-fn 1)  ; 1
  (gamma-fn 2)  ; 1
  (gamma-fn 3)  ; 2
  (gamma-fn 4)  ; 6
  (gamma-fn 5)  ; 24
  )

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn mellin-transform
  "Mellin transform.

  Mellin-Meijer-kernel density estimation on R+. Gery Geenens∗ School of
  Mathematics and Statistics, UNSW Sydney, Australia July 17, 2017

  Eq, 2.19"
  [nu gamma zeta theta z]
  (let [tan-theta (Math/tan theta)
        cos-theta (Math/cos theta)
        ;; sin-theta (Math/sin theta)
        a1        (/ (* zeta zeta) (* gamma gamma cos-theta cos-theta))
        a2        (/ (* zeta zeta) (* gamma gamma cos-theta cos-theta))
        e1        (+ a1 (* zeta (- z 1)))
        e2        (+ a2 (* zeta (- 1 z)))]
    (* (Math/pow nu (- z 1))
       (Math/pow (/ 1 (* tan-theta tan-theta)) (* zeta (- z 1)))
       (/ (* (gamma-fn e1) (gamma-fn e2))
          (* (gamma-fn a1) (gamma-fn a2))))))

;; (mellin-transform 1 1 1 0 1)
;; (mellin-transform 3 1 2 0 1)
