(ns criterium.implementation.array
  "Use an array to store sample results"
  (:use criterium.core))

;;; Array based storage of sample results
(def ^:const max-obj-array-size 4)

(defn execute-expr-core-timed-part-with-array
  [n f ret-vals-arr]
  (let [^objects arr ret-vals-arr
        arr-size-1 (long (dec (count arr)))
        init-j (rem (dec n) max-obj-array-size)]
    (time-expr
     (loop [i (long (dec n))
            j (long init-j)
            v (f)]
       (aset arr j v)
       (if (pos? i)
         (recur (unchecked-dec i)
                (if (zero? j) arr-size-1 (unchecked-dec j))
                (f)))))))


(defn replace-ret-val-in-time-expr-result
  [[elapsed-time _] new-ret-val]
  [elapsed-time new-ret-val])


(defn execute-expr-core-with-array
  [n f reduce-with]
  (let [arr-size (int (min max-obj-array-size n))
        arr-size-1 (int (dec arr-size))
        ret-vals-arr (object-array arr-size)
        time-and-ret (execute-expr-core-timed-part-with-array n f ret-vals-arr)]
    (loop [i (int arr-size-1)
           v (aget ret-vals-arr i)]
      (if (pos? i)
        (recur (dec i) (reduce-with v (aget ret-vals-arr (dec i))))
        (replace-ret-val-in-time-expr-result time-and-ret v)))))

(defn with-array [f]
  (with-redefs [criterium.core/execute-expr execute-expr-core-with-array]
    (f)))
