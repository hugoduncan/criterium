(ns criterium.viewer.portal.modal-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collect-plan :as collect-plan]
   [criterium.collector.metrics :as metrics]
   [criterium.view :as view]
   [criterium.viewer.portal.core :as portal.core]
   [criterium.viewer.portal.modal]))

;; Tests multimethod registration, data processing, and output formatting
;; for multimodal warning views in portal viewer.
;;
;; Multimodal warnings alert users when distributions have multiple peaks,
;; which may indicate unstable benchmarks.

(deftest multimodal-warning-portal-test
  (testing "multimodal-warning*"
    (testing "produces output when n-modes > 1"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
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
        (with-redefs [portal.core/heading
                      #(swap! tapped conj {:heading %})
                      portal.core/portal-heading
                      #(swap! tapped conj {:portal-heading %})
                      portal.core/portal-table
                      #(swap! tapped conj {:table %})]
          (view/multimodal-warning* :portal {} {:modes modes-data}))
        (is (= 4 (count @tapped)))
        (is (= {:heading "WARNING: Multimodal distribution - Elapsed Time"}
               (first @tapped)))
        (is (= {:table [{:metric "Mode count" :value 2}]}
               (second @tapped)))
        (is (= {:portal-heading [:em "Mode locations:"]}
               (nth @tapped 2)))
        (let [mode-rows (:table (nth @tapped 3))]
          (is (= 2 (count mode-rows)))
          (is (every? #(contains? % :location) mode-rows))
          (is (every? #(contains? % :density) mode-rows)))))

    (testing "does not produce output when n-modes = 1"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 100.0
                                          :density 0.5}]
                                 :n-modes 1}}}]
        (with-redefs [portal.core/heading
                      #(swap! tapped conj {:heading %})
                      portal.core/portal-heading
                      #(swap! tapped conj {:portal-heading %})
                      portal.core/portal-table
                      #(swap! tapped conj {:table %})]
          (view/multimodal-warning* :portal {} {:modes modes-data}))
        (is (empty? @tapped))))

    (testing "handles missing modes data gracefully"
      (let [tapped (atom [])]
        (with-redefs [portal.core/heading
                      #(swap! tapped conj {:heading %})
                      portal.core/portal-heading
                      #(swap! tapped conj {:portal-heading %})
                      portal.core/portal-table
                      #(swap! tapped conj {:table %})]
          (is (nil? (view/multimodal-warning* :portal {} {}))))
        (is (empty? @tapped))))

    (testing "uses custom modes-id"
      (let [tapped (atom [])
            metrics-defs (select-keys (metrics/metrics) [:elapsed-time])
            modes-data {:type :criterium/modes
                        :metrics-defs metrics-defs
                        :transform collect-plan/identity-transforms
                        :modes {[:elapsed-time]
                                {:modes [{:location 50.0
                                          :density 0.4}
                                         {:location 150.0
                                          :density 0.35}]
                                 :n-modes 2}}}]
        (with-redefs [portal.core/heading
                      #(swap! tapped conj {:heading %})
                      portal.core/portal-heading
                      #(swap! tapped conj {:portal-heading %})
                      portal.core/portal-table
                      #(swap! tapped conj {:table %})]
          (view/multimodal-warning* :portal {:modes-id :my-modes}
                                    {:my-modes modes-data}))
        (is (= 4 (count @tapped)))
        (is (= {:heading "WARNING: Multimodal distribution - Elapsed Time"}
               (first @tapped)))))))
