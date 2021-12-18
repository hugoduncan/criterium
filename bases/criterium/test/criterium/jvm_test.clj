(ns criterium.jvm-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.jvm :as jvm]))

(deftest elapsed-timestamp-test
  (let [ts (jvm/timestamp)]
    (is (zero? (jvm/elapsed-time ts ts)))
    (is (= 1 (jvm/elapsed-time ts (inc ts))))
    (is (= -1 (jvm/elapsed-time (inc ts) ts)))))

(def warmup-count 10)

(deftest zero-garbage-test
  (testing "Sampling is zero garbage"
    (testing "for class-loader-counts"
      ;; run once for function initialisation
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/class-loader-counts)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/class-loader-counts))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes))))
    (testing "for compilation"
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/compilation-sample)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/compilation-sample))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes)
            (->> allocations
                 (filterv (agent/allocation-on-thread?))
                 (filterv agent/allocation-freed?)))))
    (testing "for garbage-collectpr"
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/garbage-collector-sample)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/garbage-collector-sample))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes)
            (->> allocations
                 (filterv (agent/allocation-on-thread?))
                 (filterv agent/allocation-freed?)))))
    (testing "for memory"
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/memory-sample)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/memory-sample))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes)
            (->> allocations
                 (filterv (agent/allocation-on-thread?))
                 (filterv agent/allocation-freed?)))))
    (testing "for memory-pools"
      (dotimes [_ warmup-count]
        (agent/with-allocation-tracing (jvm/memory-pools-sample)))
      (let [[allocations _res]    (agent/with-allocation-tracing
                                    (jvm/memory-pools-sample))
            {:keys [freed-bytes]} (->> allocations
                                       (filterv (agent/allocation-on-thread?))
                                       agent/allocations-summary)]
        (is (zero? freed-bytes)
            (->> allocations
                 (filterv (agent/allocation-on-thread?))
                 (filterv agent/allocation-freed?)))))
    ;; unfortunately this can not be made garbage free
    #_(testing "for thread"
        (let [[allocations _res]    (agent/with-allocation-tracing
                                      (jvm/thread-sample))
              {:keys [freed-bytes]} (agent/allocations-summary
                                     (filterv (agent/allocation-on-thread?) allocations))]
          (is (zero? freed-bytes)
              (mapv agent/allocation-freed?
                    (filterv (agent/allocation-on-thread?) allocations)))))))

;; (deftest transient-persistent-overhead-test
;;   (is (= 48
;;          (let [x (transient {})]
;;            (jvm/allocated-bytes (persistent! x))))))

;; (deftest allocation-test
;;   (testing "compilation-sample does not create garbage"
;;     (let [fc  (jvm/compilation-sample)
;;           _   (jvm/compilation-diff! fc)
;;           fc  (jvm/compilation!)
;;           x   (jvm/current-thread-allocated-bytes)
;;           fcd (jvm/compilation-diff! fc)
;;           d   (- (jvm/current-thread-allocated-bytes) x)]
;;       (is (= 0 d))
;;       (is fcd "keep a refernce until after diff test")))
;;   (testing "finalization! and finalization-diff! do not create garbage"
;;     (let [fc  (jvm/finalization!)
;;           _   (jvm/finalization-diff! fc)
;;           fc  (jvm/finalization!)
;;           x   (jvm/current-thread-allocated-bytes)
;;           fcd (jvm/finalization-diff! fc)
;;           d   (- (jvm/current-thread-allocated-bytes) x)]
;;       (is (= 0 d))
;;       (is fcd "keep a refernce until after diff test")))
;;   (testing "garbage-collector-raw functions do not create garbage"
;;     (let [fc  (jvm/garbage-collector-raw)
;;           x   (jvm/current-thread-allocated-bytes)
;;           _   (agent/with-allocation-tracing
;;                 (jvm/garbage-collector-diff-raw fc))
;;           d   (- (jvm/current-thread-allocated-bytes) x)
;;           res (jvm/garbage-collector-map-raw fc)]
;;       (is (map? res))
;;       (is (= 0 d) "no garbage object genetation")))
;;   ;; (testing "garbage-collector! and garbage-collector-diff! do not create garbage"
;;   ;;   (agent/with-allocation-tracing
;;   ;;     (let [fc  (jvm/garbage-collector!)
;;   ;;           _   (jvm/garbage-collector-diff! fc)
;;   ;;           fc  (jvm/garbage-collector!)
;;   ;;           x   (jvm/current-thread-allocated-bytes)
;;   ;;           fcd (jvm/garbage-collector-diff! fc)
;;   ;;           d   (- (jvm/current-thread-allocated-bytes) x)]
;;   ;;       (is (= 0 d))
;;   ;;       (is fcd "keep a refernce until after diff test"))))
;;   (testing "memory! and memory-diff! do not create garbage"
;;     (let [fc  (jvm/memory!)
;;           x   (jvm/current-thread-allocated-bytes)
;;           _   (agent/with-allocation-tracing
;;                 (jvm/memory-diff! fc))
;;           d   (- (jvm/current-thread-allocated-bytes) x)
;;           res (jvm/memory-diff-map fc)]
;;       (is (map? res))
;;       (is (= 0 d))))
;;   ;; (testing "memory! and memory-diff! do not create garbage"
;;   ;;   (let [fc  (jvm/memory!)
;;   ;;         _   (jvm/memory-diff! fc)
;;   ;;         fc  (jvm/memory!)
;;   ;;         x   (jvm/current-thread-allocated-bytes)
;;   ;;         fcd (jvm/memory-diff! fc)
;;   ;;         d   (- (jvm/current-thread-allocated-bytes) x)]
;;   ;;     (is (= 0 d))
;;   ;;     (is fcd "keep a refernce until after diff test")))
;;   )
