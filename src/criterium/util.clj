(ns criterium.util)

(defn- merge-fn [op]
  (fn merge-fn-inner [a b]
    (if (or (map? a) (map? b))
      (merge-with merge-fn-inner a b)
      (op a b))))


(defn diff [finish start]
  (merge-with (merge-fn -) finish start))


(defn sum
  ([] {})
  ([a b]
   (merge-with (merge-fn +) a b)))
