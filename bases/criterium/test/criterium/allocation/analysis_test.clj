(ns criterium.allocation.analysis-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.allocation.analysis :as analysis]))

;; Tests for criterium.allocation.analysis namespace.
;; Validates analysis functions for allocation traces: summary, hotspots,
;; and by-type grouping.

(def sample-trace
  "Sample allocation trace for testing."
  {:type :criterium/allocation-trace
   :records [{:object-type "Ljava/lang/String;"
              :object_size 48
              :call-class "my.ns$fn"
              :call-method "invoke"
              :call-file "my_ns.clj"
              :call-line 42
              :thread 1
              :freed true}
             {:object-type "Ljava/lang/String;"
              :object_size 64
              :call-class "my.ns$fn"
              :call-method "invoke"
              :call-file "my_ns.clj"
              :call-line 42
              :thread 1
              :freed false}
             {:object-type "[J"
              :object_size 1024
              :call-class "other.ns$bar"
              :call-method "invoke"
              :call-file "other_ns.clj"
              :call-line 10
              :thread 1
              :freed true}
             {:object-type "Ljava/lang/Long;"
              :object_size 24
              :call-class "my.ns$fn"
              :call-method "invoke"
              :call-file "my_ns.clj"
              :call-line 42
              :thread 2
              :freed false}]
   :thread-id 1
   :eval-count 100
   :elapsed-time 1.5e9})

(def empty-trace
  "Empty allocation trace for edge case testing."
  {:type :criterium/allocation-trace
   :records []
   :thread-id 1
   :eval-count 1
   :elapsed-time 1e6})

(deftest summary-fn-test
  (testing "summary-fn"
    (testing "computes totals from allocation trace"
      (let [analyse (analysis/summary-fn)
            result (analyse {:allocation-trace sample-trace})
            summary (:allocation-summary result)]
        (is (= :criterium/allocation-summary (:type summary)))
        (is (= 4 (:num-allocations summary)))
        (is (= 2 (:num-freed summary)))
        (is (= (+ 48 64 1024 24) (:total-allocated summary)))
        (is (= (+ 48 1024) (:total-freed summary)))))

    (testing "returns zeros for empty trace"
      (let [analyse (analysis/summary-fn)
            result (analyse {:allocation-trace empty-trace})
            summary (:allocation-summary result)]
        (is (= 0 (:num-allocations summary)))
        (is (= 0 (:num-freed summary)))
        (is (= 0 (:total-allocated summary)))
        (is (= 0 (:total-freed summary)))))

    (testing "uses custom :id option"
      (let [analyse (analysis/summary-fn {:id :my-summary})
            result (analyse {:allocation-trace sample-trace})]
        (is (contains? result :my-summary))
        (is (not (contains? result :allocation-summary)))))

    (testing "uses custom :trace-id option"
      (let [analyse (analysis/summary-fn {:trace-id :my-trace})
            result (analyse {:my-trace sample-trace})]
        (is (= 4 (get-in result [:allocation-summary :num-allocations])))))

    (testing "preserves other data-map entries"
      (let [analyse (analysis/summary-fn)
            result (analyse {:allocation-trace sample-trace
                             :other-data :preserved})]
        (is (= :preserved (:other-data result)))))))

(deftest hotspots-fn-test
  (testing "hotspots-fn"
    (testing "groups allocations by call-site"
      (let [analyse (analysis/hotspots-fn)
            result (analyse {:allocation-trace sample-trace})
            hotspots (get-in result [:allocation-hotspots :hotspots])]
        (is (= :criterium/allocation-hotspots
               (get-in result [:allocation-hotspots :type])))
        (is (= 2 (count hotspots)))
        ;; First hotspot should be the one with most bytes
        (let [first-hotspot (first hotspots)]
          (is (= "other.ns$bar"
                 (get-in first-hotspot [:call-site :call-class])))
          (is (= 1024 (:bytes first-hotspot)))
          (is (= 1 (:count first-hotspot))))))

    (testing "aggregates stats for same call-site"
      (let [analyse (analysis/hotspots-fn)
            result (analyse {:allocation-trace sample-trace})
            hotspots (get-in result [:allocation-hotspots :hotspots])
            my-ns-hotspot (first (filter #(= "my.ns$fn"
                                             (get-in % [:call-site :call-class]))
                                         hotspots))]
        (is (= 3 (:count my-ns-hotspot)))
        (is (= (+ 48 64 24) (:bytes my-ns-hotspot)))
        (is (= 1 (:freed-count my-ns-hotspot)))
        (is (= 48 (:freed-bytes my-ns-hotspot)))))

    (testing "respects :limit option"
      (let [analyse (analysis/hotspots-fn {:limit 1})
            result (analyse {:allocation-trace sample-trace})
            hotspots (get-in result [:allocation-hotspots :hotspots])]
        (is (= 1 (count hotspots)))))

    (testing "sorts by :count when specified"
      (let [analyse (analysis/hotspots-fn {:order-by :count})
            result (analyse {:allocation-trace sample-trace})
            hotspots (get-in result [:allocation-hotspots :hotspots])]
        ;; my.ns$fn has 3 allocations, other.ns$bar has 1
        (is (= "my.ns$fn"
               (get-in (first hotspots) [:call-site :call-class])))))

    (testing "returns empty vector for empty trace"
      (let [analyse (analysis/hotspots-fn)
            result (analyse {:allocation-trace empty-trace})
            hotspots (get-in result [:allocation-hotspots :hotspots])]
        (is (= [] hotspots))))

    (testing "uses custom :id option"
      (let [analyse (analysis/hotspots-fn {:id :my-hotspots})
            result (analyse {:allocation-trace sample-trace})]
        (is (contains? result :my-hotspots))))))

(deftest by-type-fn-test
  (testing "by-type-fn"
    (testing "groups allocations by object type"
      (let [analyse (analysis/by-type-fn)
            result (analyse {:allocation-trace sample-trace})
            by-type (get-in result [:allocation-by-type :by-type])]
        (is (= :criterium/allocation-by-type
               (get-in result [:allocation-by-type :type])))
        (is (= 3 (count by-type)))
        (is (contains? by-type "Ljava/lang/String;"))
        (is (contains? by-type "[J"))
        (is (contains? by-type "Ljava/lang/Long;"))))

    (testing "computes per-type statistics"
      (let [analyse (analysis/by-type-fn)
            result (analyse {:allocation-trace sample-trace})
            string-stats (get-in result [:allocation-by-type
                                         :by-type
                                         "Ljava/lang/String;"])]
        (is (= 2 (:count string-stats)))
        (is (= (+ 48 64) (:bytes string-stats)))
        (is (= 1 (:freed-count string-stats)))
        (is (= 48 (:freed-bytes string-stats)))))

    (testing "returns empty map for empty trace"
      (let [analyse (analysis/by-type-fn)
            result (analyse {:allocation-trace empty-trace})
            by-type (get-in result [:allocation-by-type :by-type])]
        (is (= {} by-type))))

    (testing "uses custom :id option"
      (let [analyse (analysis/by-type-fn {:id :my-by-type})
            result (analyse {:allocation-trace sample-trace})]
        (is (contains? result :my-by-type))))))

(deftest ->allocation-analyse-test
  (testing "->allocation-analyse"
    (testing "composes multiple analysis functions"
      (let [analyse (analysis/->allocation-analyse
                     [[:summary-fn {}]
                      [:hotspots-fn {:limit 5}]
                      [:by-type-fn {}]])
            result (analyse {:allocation-trace sample-trace})]
        (is (contains? result :allocation-summary))
        (is (contains? result :allocation-hotspots))
        (is (contains? result :allocation-by-type))))

    (testing "functions are applied in order"
      (let [analyse (analysis/->allocation-analyse
                     [[:summary-fn {:id :first}]
                      [:summary-fn {:id :second}]])
            result (analyse {:allocation-trace sample-trace})]
        (is (contains? result :first))
        (is (contains? result :second))))

    (testing "handles empty plan"
      (let [analyse (analysis/->allocation-analyse [])
            result (analyse {:allocation-trace sample-trace})]
        (is (= sample-trace (:allocation-trace result)))))

    (testing "handles nil plan"
      (let [analyse (analysis/->allocation-analyse nil)
            result (analyse {:allocation-trace sample-trace})]
        (is (= sample-trace (:allocation-trace result)))))

    (testing "throws on invalid plan type"
      (is (thrown? clojure.lang.ExceptionInfo
                   (analysis/->allocation-analyse :invalid))))))
