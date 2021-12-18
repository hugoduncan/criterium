(ns criterium.budget.spec-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.test.check.clojure-test :refer [defspec]]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.budget :as budget]
   [criterium.budget.spec]))

(alias 'stc 'clojure.spec.test.check)

;; (s/def ::budget/budget (s/with-gen ::budget/budget
;;                           #(sgen/fmap
;;                             budget/budget*
;;                             (sgen/tuple
;;                              (s/gen ::domain/elapsed-time)
;;                              (s/gen ::domain/eval-count)))))

;; spec can't instrument functions with primitives, so explicitly
;; list the ones that can be instrumented.
;; (stest/instrument
;;  [`budget/add
;;   `budget/subtract
;;   `budget/budget?
;;   `budget/budget
;;   `budget/budget*])

;; we want function specific options, so explicitly test individual functions
(stest/check `budget/budget `budget/budget*)
(stest/check `budget/add {::stc/opts {:num-tests 100}})
;; this needs a better generator for sub, to find positive results
(stest/check `budget/subtract {::stc/opts {:num-tests 5}})

(defn- budget= [a b]
  (and (= (.elapsed-time a) (.elapsed-time b))
       (= (.eval-count a) (.eval-count b))))

(defspec budget-is-readable
  (prop/for-all [b (s/gen ::budget/budget)]
                (budget= b (read-string (pr-str b)))))

(defspec budget-add-is-inverse-of-subtract
  (prop/for-all [b1 (s/gen ::budget/budget)
                 b2 (s/gen ::budget/budget)]
                (budget= b1 (budget/subtract (budget/add b1 b2) b2))))

(defspec phase-budget-with-no-period-ns-scales-budget
  (prop/for-all [b (s/gen ::budget/budget)
                 f (gen/double* {:min 0.0 :max 1.0})]
                (budget=
                 (budget/budget
                  (long (* (double (.elapsed-time b)) f))
                  (long (* (double (.eval-count b)) f)))
                 (budget/phase-budget b nil nil f))))
