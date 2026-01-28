(ns criterium.viewer.completeness-test
  "Regression test ensuring all viewers implement all view multimethods.
   Prevents issues where new multimethods are added but not implemented
   for all viewers."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.view]
   [criterium.viewer.kindly]
   [criterium.viewer.portal]
   [criterium.viewer.pprint]
   [criterium.viewer.print]))

(def ^:private view-multimethods
  "All view multimethods from criterium.view namespace.
   These are defmulti vars whose names end in '*'."
  (->> (ns-publics 'criterium.view)
       (filter (fn [[sym var]]
                 (and (str/ends-with? (name sym) "*")
                      (instance? clojure.lang.MultiFn @var))))
       (map (fn [[sym var]] {:name sym :multimethod @var}))
       (sort-by :name)))

(def ^:private required-viewers
  "Viewers that must implement all view multimethods."
  [:print :pprint :portal :kindly])

(defn- get-implemented-methods
  "Returns set of dispatch values that have implementations for a multimethod."
  [^clojure.lang.MultiFn multimethod]
  (->> (.getMethodTable multimethod)
       keys
       set))

(defn- missing-implementations
  "Returns a map of viewer -> seq of missing multimethod names."
  []
  (reduce
   (fn [acc {:keys [name multimethod]}]
     (let [implemented (get-implemented-methods multimethod)]
       (reduce
        (fn [acc viewer]
          (if (or (contains? implemented viewer)
                  (contains? implemented :default))
            acc
            (update acc viewer (fnil conj []) name)))
        acc
        required-viewers)))
   {}
   view-multimethods))

(deftest all-viewers-implement-all-multimethods
  ;; Verifies that all view multimethods have implementations for all
  ;; required viewers. This prevents regressions where new multimethods
  ;; are added to criterium.view but implementations are forgotten.
  (testing "viewer completeness"
    (testing "all required viewers implement all view multimethods"
      (let [missing (missing-implementations)]
        (is (empty? missing)
            (str "Missing viewer implementations:\n"
                 (str/join
                  "\n"
                  (for [[viewer methods] (sort-by key missing)]
                    (str "  " viewer ": " (str/join ", " methods))))))))))
