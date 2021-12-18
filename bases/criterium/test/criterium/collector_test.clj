(ns criterium.collector-test
  (:require
   [clojure.set :as set]
   [clojure.spec.test.alpha]
   [clojure.test :refer [deftest is testing]]
   [criterium.collector :as collector]
   [criterium.collector.impl :as collector-impl]
   [criterium.measured :as measured]))

(def m-value 12345)
(def m (measured/measured
        (fn args-f [] ::args)
        (fn measured-f [args _n]
          [m-value [args args]])
        nil))

(def base-keys #{:expr-value :elapsed-time})

(defn run-pipeline
  [collector-config measured]
  (let [pipeline (collector/collector collector-config)
        sample   (collector/collect-array
                  pipeline
                  measured
                  (measured/args measured)
                  1)]
    (is (= (:length pipeline) (alength sample)))
    (collector/transform pipeline sample)))

(deftest execute-test
  (testing "Execute a measured with time-metric"
    (let [res (run-pipeline {:stages [] :terminator :elapsed-time} m)]
      (is (map? res) res)
      (testing "Has the measured time on the :elapsed-time key"
        (is (= m-value (:elapsed-time res))))
      (testing "Has the evaluation count on the :eval-count key"
        (is (= m-value (:elapsed-time res))))
      (testing "Has the measured expression value on the :expr-value key"
        (is (= m-value (:elapsed-time res)))))))

(deftest with-measured-args-test
  (testing "Execute a measured with measured-args"
    (let [res (run-pipeline
               {:stages [:measured-args] :terminator :elapsed-time}
               m)]
      (testing "Has the measured state on the :state key"
        (is (= ::args (:args res)))
        (is (= m-value (:elapsed-time res)))))))

(defn- all-stages []
  (->> 'criterium.collector.fns
       ns-publics
       vals
       (mapv var-get)
       (remove collector/terminal?)
       (filterv collector/stage?)))

(defn- all-terminators []
  (->> 'criterium.collector.fns
       ns-publics
       vals
       (mapv var-get)
       (filterv collector/terminal?)))

(deftest pipeline-fns-test
  (doseq [stage (all-stages)]
    (testing (str "Pipeline function " (:id stage))
      (let [res (run-pipeline {:stages [stage] :terminator :elapsed-time} m)
            ks  (set (keys res))]
        (is (= base-keys (set/intersection base-keys ks)))))))

(deftest pipeline*-test
  (testing "pipeline-fn*"
    (testing "builds a pipeline"
      (is (fn? (collector-impl/pipeline-sample-fn
                {:stages     (all-stages)
                 :terminator (first (all-terminators))}))))
    (testing "throws if passed a non keyword"
      (is (thrown? clojure.lang.ExceptionInfo
                   (collector-impl/pipeline-sample-fn
                    {:stages     [::unknown]
                     :terminator (first (all-terminators))}))))
    (testing "throws if passed an unknown terminal function"
      (is (thrown? clojure.lang.ExceptionInfo
                   (collector-impl/pipeline-sample-fn
                    {:stages     (all-stages)
                     :terminator ::unknown}))))))

(deftest pipeline-test
  (testing "pipeline"
    (testing "builds a pipeline"
      (let [pipeline (collector/collector
                      {:stages     (all-stages)
                       :terminator :elapsed-time})]
        (is (map? pipeline))
        (is (contains? pipeline :metrics-configs))
        (is (fn? (-> pipeline :f)))
        (is (fn? (-> pipeline :x)))))
    (testing "throws if passed a non keyword"
      (is (thrown? clojure.lang.ExceptionInfo
                   {:config
                    {:collector-config
                     (collector/collector
                      {:config
                       {:collector-config
                        {:stages     [::unknown]
                         :terminator :elapsed-time}}})}})))
    (testing "throws if passed an unknown terminal function"
      (is (thrown? clojure.lang.ExceptionInfo
                   {:config
                    {:collector-config
                     (collector/collector
                      {:config
                       {:collector-config
                        {:stages     (all-stages)
                         :terminator ::unknown}}})}})))))
