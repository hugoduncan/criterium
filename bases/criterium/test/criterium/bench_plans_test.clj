(ns criterium.bench-plans-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.bench-plans :as bench-plans]))

;; Tests verify that autocorrelation analysis is integrated into bench plans.
;; Autocorrelation detects sample non-independence and adjusts confidence intervals
;; via the :acf-id parameter in bootstrap-stats.
;;
;; Plans with outlier filtering have two autocorrelation analyses:
;; - :autocorrelation-raw - for pattern detection on unfiltered data
;; - :autocorrelation-filtered - for effective sample size on filtered data
;;
;; Plans without outlier filtering (e.g., tail-analysis) have only :autocorrelation-raw.

(defn- find-analyse-entry
  "Find entry in :analyse vector matching the given key."
  [plan entry-key]
  (->> (:analyse plan)
       (filter #(or (= entry-key %)
                    (and (vector? %) (= entry-key (first %)))))
       first))

(defn- find-analyse-entry-by-id
  "Find entry in :analyse vector with matching :id in options."
  [plan entry-id]
  (->> (:analyse plan)
       (filter #(and (vector? %)
                     (= entry-id (:id (second %)))))
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

(defn- analyse-position-by-id
  "Return the position of entry with :id in :analyse vector."
  [plan entry-id]
  (->> (:analyse plan)
       (map-indexed (fn [i entry]
                      (when (and (vector? entry)
                                 (= entry-id (:id (second entry))))
                        i)))
       (remove nil?)
       first))

(deftest default-with-warmup-test
  ;; Tests verify dual autocorrelation analysis in default-with-warmup:
  ;; - autocorrelation-raw for pattern detection
  ;; - autocorrelation-filtered for effective sample size
  (testing "default-with-warmup"
    (let [plan bench-plans/default-with-warmup]

      (testing "includes :autocorrelation-raw in analyse"
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-raw))))

      (testing "includes :autocorrelation-filtered in analyse"
        (let [entry (find-analyse-entry-by-id plan :autocorrelation-filtered)]
          (is (some? entry))
          (is (= :outliers (:outlier-id (second entry))))))

      (testing "positions :autocorrelation-raw after :transform-log"
        (is (< (analyse-position plan :transform-log)
               (analyse-position-by-id plan :autocorrelation-raw))))

      (testing "positions :autocorrelation-raw before :quantiles"
        (is (< (analyse-position-by-id plan :autocorrelation-raw)
               (analyse-position plan :quantiles))))

      (testing "positions :autocorrelation-filtered after :outliers"
        (is (< (analyse-position plan :outliers)
               (analyse-position-by-id plan :autocorrelation-filtered))))

      (testing "configures bootstrap-stats with :acf-id :autocorrelation-filtered"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (vector? bs-entry))
          (is (= :autocorrelation-filtered (:acf-id (second bs-entry))))))

      (testing "includes :autocorrelation-classification view with raw id"
        (let [entry (find-view-entry plan :autocorrelation-classification)]
          (is (some? entry))
          (is (= :autocorrelation-raw (:autocorrelation-id (second entry))))))

      (testing "includes :effective-sample-size view with filtered id"
        (let [entry (find-view-entry plan :effective-sample-size)]
          (is (some? entry))
          (is (= :autocorrelation-filtered (:autocorrelation-id (second entry)))))))))

(deftest log-histogram-test
  (testing "log-histogram"
    (let [plan bench-plans/log-histogram]

      (testing "includes both autocorrelation analyses"
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-raw)))
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-filtered))))

      (testing "configures bootstrap-stats with :acf-id :autocorrelation-filtered"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation-filtered (:acf-id (second bs-entry))))))

      (testing "includes separate views"
        (is (some? (find-view-entry plan :autocorrelation-classification)))
        (is (some? (find-view-entry plan :effective-sample-size)))))))

(deftest knuth-histogram-test
  (testing "knuth-histogram"
    (let [plan bench-plans/knuth-histogram]

      (testing "includes both autocorrelation analyses"
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-raw)))
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-filtered))))

      (testing "configures bootstrap-stats with :acf-id :autocorrelation-filtered"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation-filtered (:acf-id (second bs-entry))))))

      (testing "includes separate views"
        (is (some? (find-view-entry plan :autocorrelation-classification)))
        (is (some? (find-view-entry plan :effective-sample-size)))))))

(deftest kde-histogram-test
  (testing "kde-histogram"
    (let [plan bench-plans/kde-histogram]

      (testing "includes both autocorrelation analyses"
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-raw)))
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-filtered))))

      (testing "configures bootstrap-stats with :acf-id :autocorrelation-filtered"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation-filtered (:acf-id (second bs-entry))))))

      (testing "includes separate views"
        (is (some? (find-view-entry plan :autocorrelation-classification)))
        (is (some? (find-view-entry plan :effective-sample-size)))))))

(deftest kde-modes-test
  (testing "kde-modes"
    (let [plan bench-plans/kde-modes]

      (testing "includes both autocorrelation analyses"
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-raw)))
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-filtered))))

      (testing "configures bootstrap-stats with :acf-id :autocorrelation-filtered"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation-filtered (:acf-id (second bs-entry))))))

      (testing "includes separate views"
        (is (some? (find-view-entry plan :autocorrelation-classification)))
        (is (some? (find-view-entry plan :effective-sample-size)))))))

(deftest distribution-analysis-test
  (testing "distribution-analysis"
    (let [plan bench-plans/distribution-analysis]

      (testing "includes both autocorrelation analyses"
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-raw)))
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-filtered))))

      (testing "configures bootstrap-stats with :acf-id :autocorrelation-filtered"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation-filtered (:acf-id (second bs-entry))))))

      (testing "includes separate views"
        (is (some? (find-view-entry plan :autocorrelation-classification)))
        (is (some? (find-view-entry plan :effective-sample-size)))))))

(deftest tail-analysis-test
  ;; tail-analysis uses raw samples WITHOUT outlier filtering (extreme values
  ;; ARE the tail). It has only one autocorrelation analysis on raw samples.
  (testing "tail-analysis"
    (let [plan bench-plans/tail-analysis]

      (testing "includes only :autocorrelation-raw (no outlier filtering)"
        (is (some? (find-analyse-entry-by-id plan :autocorrelation-raw)))
        (is (nil? (find-analyse-entry-by-id plan :autocorrelation-filtered))))

      (testing "configures bootstrap-stats with :acf-id :autocorrelation-raw"
        (let [bs-entry (find-analyse-entry plan :bootstrap-stats)]
          (is (= :autocorrelation-raw (:acf-id (second bs-entry))))))

      (testing "includes separate views referencing :autocorrelation-raw"
        (let [class-entry (find-view-entry plan :autocorrelation-classification)
              ess-entry (find-view-entry plan :effective-sample-size)]
          (is (some? class-entry))
          (is (= :autocorrelation-raw (:autocorrelation-id (second class-entry))))
          (is (some? ess-entry))
          (is (= :autocorrelation-raw (:autocorrelation-id (second ess-entry)))))))))

(deftest default-one-shot-test
  ;; Tests verify that default-one-shot does NOT include autocorrelation.
  ;; One-shot benchmarks don't have enough samples for meaningful autocorrelation.
  (testing "default-one-shot"
    (let [plan bench-plans/default-one-shot]

      (testing "does NOT include :autocorrelation in analyse"
        (is (nil? (find-analyse-entry plan :autocorrelation)))
        (is (nil? (find-analyse-entry-by-id plan :autocorrelation-raw)))
        (is (nil? (find-analyse-entry-by-id plan :autocorrelation-filtered))))

      (testing "does NOT include autocorrelation views"
        (is (nil? (find-view-entry plan :autocorrelation)))
        (is (nil? (find-view-entry plan :autocorrelation-classification)))
        (is (nil? (find-view-entry plan :effective-sample-size)))))))
