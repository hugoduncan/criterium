(ns criterium.viewer.portal-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.collector.metrics :as metrics]
   [criterium.view :as view]
   [criterium.viewer.portal]))

(defmacro with-tap-out [& body]
  `(let [v# (volatile! [])
         f# (fn [x#] (vswap! v# conj x#))]
     (try
       (add-tap f#)
       ~@body
       (loop []
         (when-not (.isEmpty @#'clojure.core/tapq)
           (recur)))
       (loop []
         (when (empty? @v#)
           (recur)))
       @v#
       (finally
         (remove-tap f#)))))

(deftest print-outlier-significance-test
  (testing "print-outlier-significance"
    (testing "prints via view"
      (is (= [[{:effect :moderate :significance 0.25}]]
             (with-tap-out
               ((view/outlier-significance)
                {:metrics-configs      (select-keys
                                        (metrics/metrics)
                                        [:elapsed-time])
                 :num-samples          1
                 :outlier-significance {:elapsed-time
                                        {:effect       :moderate
                                         :significance 0.25}}
                 :viewer               :portal})))))))
