(ns criterium.jvm.impl-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [criterium.jvm.impl :as impl]))

;; Fixtures

(def ^:private original-thread-state (atom nil))

(defn save-thread-monitoring-state [f]
  (let [bean (.. java.lang.management.ManagementFactory getThreadMXBean)]
    (reset! original-thread-state
            {:contention-monitoring (.isThreadContentionMonitoringEnabled bean)
             :cpu-timing           (.isThreadCpuTimeEnabled bean)}))
  (f)
  (let [{:keys [contention-monitoring cpu-timing]} @original-thread-state]
    (impl/set-thread-contention-monitoring-enabled contention-monitoring)
    (impl/set-thread-cpu-time-enabled cpu-timing)))

(use-fixtures :each save-thread-monitoring-state)

;; Helper Functions

(defn thread-allocated-bytes-overhead
  ^long []
  (unchecked-subtract
   (impl/thread-allocated-bytes (impl/current-thread-id))
   (impl/thread-allocated-bytes (impl/current-thread-id))))

;; ClassLoading Tests

(deftest class-loading-test
  (testing "class loader counts returns expected structure"
    (let [counts (impl/class-loader-counts)]
      (is (contains? counts :loaded-count))
      (is (contains? counts :unloaded-count))
      (is (number? (:loaded-count counts)))
      (is (number? (:unloaded-count counts)))))

  (testing "class loader counts change calculation"
    (let [first-sample {:loaded-count 100 :unloaded-count 10}
          last-sample {:loaded-count 120 :unloaded-count 15}
          change (impl/class-loader-counts-change first-sample last-sample)]
      (is (= {:loaded-count 20 :unloaded-count 5} change)))))

;; Memory Tests

(deftest memory-test
  (testing "memory sample contains expected structure"
    (let [sample (impl/memory-sample)]
      (is (some? (:heap sample)))
      (is (some? (:non-heap sample)))))

  (testing "memory usage calculation"
    (let [sample (impl/memory-sample)
          usage (impl/memory sample)]
      (is (contains? usage :heap))
      (is (contains? usage :non-heap))
      (is (contains? usage :total))
      (doseq [section [:heap :non-heap :total]]
        (let [section-data (get usage section)]
          (is (contains? section-data :committed))
          (is (contains? section-data :init))
          (is (contains? section-data :max))
          (is (contains? section-data :used))))))

  (testing "finalization tracking"
    (let [sample (impl/finalization-sample)
          result (impl/finalization sample)]
      (is (contains? result :pending))
      (is (number? (:pending result))))))

;; Thread Tests

(deftest thread-monitoring-test
  (testing "thread-allocated-bytes has no memory overhead"
    (is (= 0 (thread-allocated-bytes-overhead))))

  (testing "thread info structure"
    (let [id     (impl/current-thread-id)
          sample (impl/thread-sample id)]
      (is (contains? sample :thread-info))
      (is (contains? sample :cpu-time))
      (is (contains? sample :user-time))
      (is (contains? sample :allocated))

      (let [info (impl/thread sample)]
        (is (contains? (:thread-info info) :blocked-count))
        (is (contains? (:thread-info info) :blocked-time-ms))
        (is (contains? (:thread-info info) :waited-count))
        (is (contains? (:thread-info info) :waited-time-ms)))))

  (testing "thread monitoring can be enabled/disabled"
    (impl/set-thread-contention-monitoring-enabled true)
    (impl/set-thread-cpu-time-enabled true)
    (let [id     (impl/current-thread-id)
          sample (impl/thread-sample id)]
      (is (number? (:cpu-time sample)))
      (is (number? (:user-time sample))))))

;; Compilation Tests

(def ^:private compilation-changes-data
  [{:name "basic increase"
    :first {:time-ms 100}
    :last {:time-ms 150}
    :expected {:time-ms 50}}
   {:name "no change"
    :first {:time-ms 100}
    :last {:time-ms 100}
    :expected {:time-ms 0}}
   {:name "decrease (unusual but possible)"
    :first {:time-ms 150}
    :last {:time-ms 100}
    :expected {:time-ms -50}}])

(deftest compilation-test
  (testing "JIT compiler name"
    (let [name (impl/jit-name)]
      (is (string? name))
      (is (not (str/blank? name)))))

  (testing "compilation time monitoring"
    (let [sample (impl/compilation-sample)]
      ;; Should either be -1 (unsupported) or a non-negative number
      (is (or (= sample -1)
              (and (number? sample)
                   (>= sample 0))))))

  (testing "compilation sample wrapper"
    (let [sample 100
          result (impl/compilation sample)]
      (is (= {:time-ms 100} result))))

  (testing "compilation change calculations"
    (doseq [{:keys [name first last expected]} compilation-changes-data]
      (testing name
        (is (= expected
               (impl/compilation-change
                (:time-ms first)
                (:time-ms last))))))))

;; Memory Pool Tests

(def ^:private memory-usage-keys
  #{:committed :init :max :used})

(deftest memory-pool-test
  (testing "memory pool names"
    (let [names (impl/memory-pool-names)]
      (is (vector? names))
      (is (pos? (count names)))
      (is (every? string? names))
      (is (every? (complement str/blank?) names))))

  (testing "memory pool keywords"
    (let [keywords impl/memory-pool-keywords]
      (is (vector? keywords))
      (is (= (count (impl/memory-pool-names)) (count keywords)))
      (is (every? keyword? keywords))))

  (testing "memory pools sample structure"
    (let [sample (impl/memory-pools-sample)]
      (is (vector? sample))
      (is (pos? (count sample)))
      (is (every? #(instance? java.lang.management.MemoryUsage %) sample))))

  (testing "memory pools usage calculation"
    (let [sample (impl/memory-pools-sample)
          pools  (impl/memory-pools sample)]
      ;; Check total is present
      (is (contains? pools :total))

      ;; All values should be maps with standard memory usage keys
      (doseq [[_pool-name usage] pools]
        (is (map? usage))
        (is (= memory-usage-keys (set (keys usage))))
        (doseq [v (vals usage)]
          (is (number? v)))))

    ;; Verify total is sum of other pools
    (let [sample         (impl/memory-pools-sample)
          pools          (impl/memory-pools sample)
          pool-vals      (dissoc pools :total)
          computed-total (reduce #'impl/val-sum (vals pool-vals))]
      (is (= computed-total (:total pools)))))

  (testing "memory pools change calculation"
    (let [sample         (impl/memory-pools-sample)
          ;; Create artificial change by doubling values
          changed-sample (mapv #(let [usage ^java.lang.management.MemoryUsage %]
                                  (java.lang.management.MemoryUsage.
                                   (* 2 (.getInit usage))
                                   (* 2 (.getUsed usage))
                                   (* 2 (.getCommitted usage))
                                   (if (pos? (.getMax usage))
                                     (* 2 (.getMax usage))
                                     (.getMax usage))))
                               sample)
          change         (impl/memory-pools-change sample changed-sample)]
      ;; Structure tests
      (is (contains? change :total))
      (is (= (count impl/memory-pool-keywords)
             (count (dissoc change :total)))))))

;; Garbage Collector Tests

(def ^:private gc-sample-structure-keys
  #{:count :time-ms})

(def ^:private gc-change-test-data
  [{:name "Basic collection occurred"
    :first {:count 10 :time-ms 100}
    :last  {:count 11 :time-ms 150}
    :expected {:count 1 :time-ms 50}}
   {:name "Multiple collections"
    :first {:count 10 :time-ms 100}
    :last  {:count 15 :time-ms 200}
    :expected {:count 5 :time-ms 100}}
   {:name "No collections occurred"
    :first {:count 10 :time-ms 100}
    :last  {:count 10 :time-ms 100}
    :expected {:count 0 :time-ms 0}}])

(deftest garbage-collector-test
  (testing "GC bean names"
    (let [names (impl/garbage-collector-names)]
      (is (vector? names))
      (is (pos? (count names)))
      (is (every? string? names))
      (is (every? (complement str/blank?) names))))

  (testing "GC keywords generation"
    (let [keywords impl/garbage-collector-keywords]
      (is (vector? keywords))
      (is (= (count (impl/garbage-collector-names)) (count keywords)))
      (is (every? keyword? keywords))))

  (testing "GC sample structure"
    (let [sample (impl/garbage-collector-sample)]
      (is (vector? sample))
      (is (pos? (count sample)))
      (doseq [collector sample]
        (is (map? collector))
        (is (= gc-sample-structure-keys
               (set (keys collector))))
        (is (nat-int? (:count collector)))
        (is (nat-int? (:time-ms collector))))))

  (testing "GC metrics aggregation"
    (let [sample  (impl/garbage-collector-sample)
          metrics (impl/garbage-collector sample)]
      ;; Check structure
      (is (contains? metrics :total))
      (is (= (count impl/garbage-collector-keywords)
             (count (dissoc metrics :total))))

      ;; Verify all collectors present
      (doseq [k impl/garbage-collector-keywords]
        (is (contains? metrics k)))

      ;; Check data types
      (doseq [[_name data] metrics]
        (is (map? data))
        (is (= gc-sample-structure-keys (set (keys data))))
        (is (nat-int? (:count data)))
        (is (nat-int? (:time-ms data))))

      ;; Verify total is sum of all collectors
      (let [collectors-data (vals (dissoc metrics :total))
            computed-total  (reduce #'impl/val-sum collectors-data)]
        (is (= computed-total (:total metrics))))))

  (testing "GC change calculations"
    (doseq [{:keys [name first last expected]} gc-change-test-data]
      (testing name
        (is (= expected
               (#'impl/val-diff last first)))))

    (testing "changes with actual GC data"
      (let [first-sample (impl/garbage-collector-sample)
            ;; Force some GC activity
            _
            (System/gc)
            last-sample
            (impl/garbage-collector-sample)
            changes
            (impl/garbage-collector-change first-sample last-sample)]
        ;; Structure tests
        (is (contains? changes :total))
        (is (= (count impl/garbage-collector-keywords)
               (count (dissoc changes :total))))

        ;; Data validation
        (doseq [[_name change-data] changes]
          (is (map? change-data))
          (is (= gc-sample-structure-keys
                 (set (keys change-data))))
          ;; Changes can be negative if GC runs between samples
          (is (integer? (:count change-data)))
          (is (integer? (:time-ms change-data))))))))

;; Runtime Info Tests

(deftest runtime-info-test
  (testing "runtime details contains expected fields"
    (let [details (impl/runtime-details)]
      (is (contains? details :name))
      (is (contains? details :spec-name))
      (is (contains? details :spec-vendor))
      (is (contains? details :vm-name))
      (is (contains? details :vm-vendor))
      (is (contains? details :vm-version))
      (is (contains? details :java-version))
      (is (contains? details :clojure-version))))

  (testing "system properties are accessible"
    (let [props (impl/system-properties)]
      (is (instance? java.util.HashMap props) (str (type props)))
      (is (contains? props "java.version"))
      (is (string? (get props "java.version"))))))
