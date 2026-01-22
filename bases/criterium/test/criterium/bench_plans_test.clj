(ns criterium.bench-plans-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.bench-plans :as bench-plans]))

;; Tests verify that autocorrelation analysis is integrated into bench plans.
;; Autocorrelation detects sample non-independence and adjusts confidence intervals
;; via the :acf-id parameter in bootstrap-stats.

(defn- find-analyse-entry
  "Find entry in :analyse vector matching the given key."
  [plan entry-key]
  (->> (:analyse plan)
       (filter #(or (= entry-key %)
                    (and (vector? %) (= entry-key (first %)))))
       first))

(defn- find-view-entry
  "Find entry in :view vector matching the given key."
  [plan entry-key]
  (->> (:view plan)
       (filter #(or (= entry-key %)
                    (and (vector? %) (= entry-key (first %)))))
       first))

(defn- analyse-position
  "Return the position of entry-key in :analyse vector."
  [plan entry-key]
  (->> (:analyse plan)
       (map-indexed (fn [i entry]
                      (when (or (= entry-key entry)
                                (and (vector? entry) (= entry-key (first entry))))
                        i)))
       (remove nil?)
       first))

(deftest default-with-warmup-test
  ;; Tests verify autocorrelation analysis position and configuration
  ;; in the default-with-warmup bench plan.
  (testing "default-with-warmup"
    (let [plan bench-plans/default-with-warmup]

      (testing "includes :autocorrelation in analyse"
        (is (some? (find-analyse-entry plan :autocorrelation))))

      (testing "positions :autocorrelation after :transform-log"
        (is (< (analyse-position plan :transform-log)
               (analyse-position plan :autocorrelation))))

      (testing "positions :autocorrelation before :quantiles"
        (is (< (analyse-position plan :autocorrelation)
               (analyse-position plan :quantiles))))

      (testing "configures bootstrap-stats with :acf-id"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (vector? bs-entry))
          (is (= :autocorrelation (:acf-id (second bs-entry))))))

      (testing "includes :autocorrelation view"
        (is (some? (find-view-entry plan :autocorrelation)))))))

(deftest log-histogram-test
  (testing "log-histogram"
    (let [plan bench-plans/log-histogram]

      (testing "includes :autocorrelation in analyse"
        (is (some? (find-analyse-entry plan :autocorrelation))))

      (testing "configures bootstrap-stats with :acf-id"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation (:acf-id (second bs-entry))))))

      (testing "includes :autocorrelation view"
        (is (some? (find-view-entry plan :autocorrelation)))))))

(deftest knuth-histogram-test
  (testing "knuth-histogram"
    (let [plan bench-plans/knuth-histogram]

      (testing "includes :autocorrelation in analyse"
        (is (some? (find-analyse-entry plan :autocorrelation))))

      (testing "configures bootstrap-stats with :acf-id"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation (:acf-id (second bs-entry))))))

      (testing "includes :autocorrelation view"
        (is (some? (find-view-entry plan :autocorrelation)))))))

(deftest kde-histogram-test
  (testing "kde-histogram"
    (let [plan bench-plans/kde-histogram]

      (testing "includes :autocorrelation in analyse"
        (is (some? (find-analyse-entry plan :autocorrelation))))

      (testing "configures bootstrap-stats with :acf-id"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation (:acf-id (second bs-entry))))))

      (testing "includes :autocorrelation view"
        (is (some? (find-view-entry plan :autocorrelation)))))))

(deftest kde-modes-test
  (testing "kde-modes"
    (let [plan bench-plans/kde-modes]

      (testing "includes :autocorrelation in analyse"
        (is (some? (find-analyse-entry plan :autocorrelation))))

      (testing "configures bootstrap-stats with :acf-id"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation (:acf-id (second bs-entry))))))

      (testing "includes :autocorrelation view"
        (is (some? (find-view-entry plan :autocorrelation)))))))

(deftest distribution-analysis-test
  (testing "distribution-analysis"
    (let [plan bench-plans/distribution-analysis]

      (testing "includes :autocorrelation in analyse"
        (is (some? (find-analyse-entry plan :autocorrelation))))

      (testing "configures bootstrap-stats with :acf-id"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation (:acf-id (second bs-entry))))))

      (testing "includes :autocorrelation view"
        (is (some? (find-view-entry plan :autocorrelation)))))))

(deftest tail-analysis-test
  (testing "tail-analysis"
    (let [plan bench-plans/tail-analysis]

      (testing "includes :autocorrelation in analyse"
        (is (some? (find-analyse-entry plan :autocorrelation))))

      (testing "configures bootstrap-stats with :acf-id"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation (:acf-id (second bs-entry))))))

      (testing "includes :autocorrelation view"
        (is (some? (find-view-entry plan :autocorrelation)))))))

(deftest default-one-shot-test
  ;; Tests verify that default-one-shot does NOT include autocorrelation.
  ;; One-shot benchmarks don't have enough samples for meaningful autocorrelation.
  (testing "default-one-shot"
    (let [plan bench-plans/default-one-shot]

      (testing "does NOT include :autocorrelation in analyse"
        (is (nil? (find-analyse-entry plan :autocorrelation))))

      (testing "does NOT include :autocorrelation view"
        (is (nil? (find-view-entry plan :autocorrelation)))))))
