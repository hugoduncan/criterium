(ns criterium.implementation.type
  "Use a deftype to store sample results"
  (:use
   [criterium.core :only [time-expr]]))


(defprotocol MutablePlace
  "Provides a mutable place"
  (set-mutable-place [_ v] "Set mutable field to value")
  (get-mutable-place [_] "Get mutable field"))

;;; Use a volatile field
(deftype Volatile [^{:volatile-mutable true :tag Object} v]
  MutablePlace
  (set-mutable-place [_ value] (set! v value))
  (get-mutable-place [_] v))

(def volatile-place (Volatile. nil))

(defn execute-expr-core-timed-part-volatile
  [n f]
  (time-expr
   (loop [i (long (dec n))
          v (f)]
     (set-mutable-place volatile-place v)
     (if (pos? i)
       (recur (unchecked-dec i) (f))))))

(defn execute-expr-core-volatile
  [n f reduce-with]
  (let [time-and-ret (execute-expr-core-timed-part-volatile n f)]
    (get-mutable-place volatile-place) ;; just for good measure
    time-and-ret))

(defn with-volatile [f]
  (with-redefs [criterium.core/execute-expr execute-expr-core-volatile]
    (f)))


;;; Use a plain unsynchronized field
(deftype Unsynchronized [^{:unsynchronized-mutable true :tag Object} v]
  MutablePlace
  (set-mutable-place [_ value] (set! v value))
  (get-mutable-place [_] v))

(def unsynchronized-place (Unsynchronized. nil))

(defn execute-expr-core-timed-part-unsynchronized
  [n f]
  (time-expr
   (loop [i (long (dec n))
          v (f)]
     (set-mutable-place unsynchronized-place v)
     (if (pos? i)
       (recur (unchecked-dec i) (f))))))

(defn execute-expr-core-unsynchronized
  [n f reduce-with]
  (let [time-and-ret (execute-expr-core-timed-part-unsynchronized n f)]
    (get-mutable-place unsynchronized-place) ;; just for good measure
    time-and-ret))

(defn with-unsynchronized [f]
  (with-redefs [criterium.core/execute-expr execute-expr-core-unsynchronized]
    (f)))
