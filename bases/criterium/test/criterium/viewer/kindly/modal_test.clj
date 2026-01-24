(ns criterium.viewer.kindly.modal-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.view :as view]
   [criterium.viewer.kindly.core :as kindly.core]
   [criterium.viewer.kindly.modal]))

;; Tests multimethod registration, data processing, and output formatting
;; for multimodal warning views in kindly viewer.
;;
;; Multimodal warnings alert users when distributions have multiple peaks,
;; which may indicate unstable benchmarks.

(deftest multimodal-warning-kindly-test
  (testing "multimodal-warning*"
    (testing "produces output when n-modes > 1"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 100.0
                                          :density 0.3
                                          :significant? true}
                                         {:location 200.0
                                          :density 0.25
                                          :significant? true}]
                                 :n-modes 2}}}]
        (view/multimodal-warning* :kindly {} {:modes modes-data})
        (let [acc @kindly.core/accumulated]
          (is (= 4 (count acc)))
          ;; First: heading
          (is (= :kind/md (:kindly/kind (meta (first acc)))))
          (is (re-find #"WARNING.*Multimodal.*Elapsed Time" (first (first acc))))
          ;; Second: mode count table
          (is (= :kind/table (:kindly/kind (meta (second acc)))))
          (is (= [{:metric "Mode count" :value 2}] (second acc)))
          ;; Third: mode locations label
          (is (= :kind/md (:kindly/kind (meta (nth acc 2)))))
          ;; Fourth: modes table
          (is (= :kind/table (:kindly/kind (meta (nth acc 3)))))
          (let [mode-rows (nth acc 3)]
            (is (= 2 (count mode-rows)))
            (is (every? #(contains? % :location) mode-rows))
            (is (every? #(contains? % :density) mode-rows))))))

    (testing "does not produce output when n-modes = 1"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 100.0
                                          :density 0.5}]
                                 :n-modes 1}}}]
        (view/multimodal-warning* :kindly {} {:modes modes-data})
        (is (empty? @kindly.core/accumulated))))

    (testing "handles missing modes data gracefully"
      (reset! kindly.core/accumulated [])
      (is (nil? (view/multimodal-warning* :kindly {} {})))
      (is (empty? @kindly.core/accumulated)))

    (testing "uses custom modes-id"
      (reset! kindly.core/accumulated [])
      (let [metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 50.0
                                          :density 0.4}
                                         {:location 150.0
                                          :density 0.35}]
                                 :n-modes 2}}}]
        (view/multimodal-warning* :kindly {:modes-id :my-modes}
                                  {:my-modes modes-data})
        (let [acc @kindly.core/accumulated]
          (is (= 4 (count acc)))
          (is (re-find #"WARNING.*Multimodal.*Elapsed Time" (first (first acc)))))))))
