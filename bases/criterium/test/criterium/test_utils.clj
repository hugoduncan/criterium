(ns criterium.test-utils
  (:require
   [clojure.string :as str]
   [clojure.test :refer [is]]
   [clojure.test.check.generators :as gen]))

(defn abs-error
  ^double [^double expected ^double actual]
  (Math/abs (- expected actual)))

(defmacro test-max-error
  ([expected actual max-error]
   `(is (< (abs-error ~expected ~actual) ~max-error)))
  ([expected actual max-error msg]
   `(is (< (abs-error ~expected ~actual) ~max-error) ~msg)))

(defn gen-bounded
  "Generates a long in the range from min-val to max-val inclusive.
  Unlike gen/choose, this is bounded by the generator's `size` parameter,
  starting at min-val."
  [^long min-val ^long max-val]
  (let [r (- max-val min-val)]
    (gen/fmap
     (fn [^long x] (+ min-val x))
     (gen/sized
      (fn [^long size]
        (let [s (min size r)]
          (gen/choose 0 s)))))))

(defn trimmed-lines
  [s]
  (->> s
       str/split-lines
       (mapv str/trim)))
