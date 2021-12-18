(ns criterium.budget.spec
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.gen.alpha :as sgen]
   [criterium.budget :as budget]
   [criterium.domain :as domain])
  (:import
   [criterium.budget
    Budget]))

(s/fdef budget/budget?
  :args (s/cat :x any?)
  :ret boolean?
  :fn #(= (:ret %) (budget/budget? (-> % :args :x))))

(s/def ::budget/budget
  (s/with-gen
    (s/and budget/budget?
           (comp nat-int? (fn [^Budget b] (.elapsed-time b)))
           (comp nat-int? (fn [^Budget b] (.eval-count b))))
    #(sgen/fmap
      budget/budget*
      (sgen/tuple
       (s/gen ::domain/elapsed-time)
       (s/gen ::domain/eval-count)))))

(s/fdef criterium.budget/budget
  :args (s/cat :elapsed-time ::domain/elapsed-time
               :eval-count ::domain/eval-count)
  :ret ::budget/budget
  :fn (s/and #(= (.elapsed-time (:ret %)) (-> % :args :elapsed-time))
             #(= (.eval-count (:ret %)) (-> % :args :eval-count))))

(s/fdef budget/budget*
  :args (s/cat :arg (s/tuple ::domain/elapsed-time ::domain/eval-count))
  :ret ::budget/budget
  :fn (s/and #(= (.elapsed-time (:ret %)) (-> % :args :arg first))
             #(= (.eval-count (:ret %)) (-> % :args :arg second))))

(s/fdef budget/add
  :args (s/and (s/cat :budgets (s/+ ::budget/budget))
               #(< (reduce + (map
                              (fn [b] (double (budget/elapsed-time b)))
                              (:budgets %)))
                   Long/MAX_VALUE)
               #(< (reduce + (map
                              (fn [b] (double (budget/eval-count b)))
                              (:budgets %)))
                   Long/MAX_VALUE))
  :ret ::budget/budget
  :fn (s/and #(= (.elapsed-time (:ret %))
                 (reduce + (map
                            (fn [b] (budget/elapsed-time b))
                            (-> % :args :budgets))))
             #(= (.eval-count (:ret %))
                 (reduce + (map
                            (fn [b] (budget/eval-count b))
                            (-> % :args :budgets))))))

(s/fdef budget/subtract
  :args (s/and (s/cat :budgets (s/+ ::budget/budget))
               #(>= (reduce - (map
                               (fn [b] (double (budget/elapsed-time b)))
                               (:budgets %)))
                    0)
               #(>= (reduce - (map
                               (fn [b] (double (budget/eval-count b)))
                               (:budgets %)))
                    0))
  :ret ::budget/budget
  :fn (s/and #(= (.elapsed-time (:ret %))
                 (reduce - (map
                            (fn [b] (budget/elapsed-time b))
                            (-> % :args :budgets))))
             #(= (.eval-count (:ret %))
                 (reduce - (map
                            (fn [b] (budget/eval-count b))
                            (-> % :args :budgets))))))
