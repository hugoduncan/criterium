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
            result (analyse {:samples {:allocation-trace sample-trace}})
            summary (:allocation-summary result)]
        (is (= :criterium/allocation-summary (:type summary)))
        (is (= 4 (:num-allocations summary)))
        (is (= 2 (:num-freed summary)))
        (is (= (+ 48 64 1024 24) (:total-allocated summary)))
        (is (= (+ 48 1024) (:total-freed summary)))))

    (testing "returns zeros for empty trace"
      (let [analyse (analysis/summary-fn)
            result (analyse {:samples {:allocation-trace empty-trace}})
            summary (:allocation-summary result)]
        (is (= 0 (:num-allocations summary)))
        (is (= 0 (:num-freed summary)))
        (is (= 0 (:total-allocated summary)))
        (is (= 0 (:total-freed summary)))))

    (testing "uses custom :id option"
      (let [analyse (analysis/summary-fn {:id :my-summary})
            result (analyse {:samples {:allocation-trace sample-trace}})]
        (is (contains? result :my-summary))
        (is (not (contains? result :allocation-summary)))))

    (testing "uses custom :trace-id option"
      (let [analyse (analysis/summary-fn {:trace-id [:my-trace]})
            result (analyse {:my-trace sample-trace})]
        (is (= 4 (get-in result [:allocation-summary :num-allocations])))))

    (testing "preserves other data-map entries"
      (let [analyse (analysis/summary-fn)
            result (analyse {:samples {:allocation-trace sample-trace}
                             :other-data :preserved})]
        (is (= :preserved (:other-data result)))))))

(deftest hotspots-fn-test
  ;; Tests hotspots-fn which groups allocations by (call-site, object-type) pairs.
  ;; Verifies aggregation, sorting, limiting, and fallback behavior.
  (testing "hotspots-fn"
    (testing "groups allocations by call-site and object-type"
      (let [analyse (analysis/hotspots-fn)
            result (analyse {:samples {:allocation-trace sample-trace}})
            hotspots (get-in result [:allocation-hotspots :hotspots])]
        (is (= :criterium/allocation-hotspots
               (get-in result [:allocation-hotspots :type])))
        ;; With (call-site, object-type) grouping:
        ;; - other.ns$bar + [J: 1024 bytes (1 alloc)
        ;; - my.ns$fn + String: 112 bytes (2 allocs)
        ;; - my.ns$fn + Long: 24 bytes (1 alloc)
        (is (= 3 (count hotspots)))
        ;; First hotspot should be the one with most bytes
        (let [first-hotspot (first hotspots)]
          (is (= "other.ns$bar"
                 (get-in first-hotspot [:call-site :call-class])))
          (is (= "[J" (:object-type first-hotspot)))
          (is (= 1024 (:bytes first-hotspot)))
          (is (= 1 (:count first-hotspot))))))

    (testing "aggregates stats for same call-site and object-type"
      (let [analyse (analysis/hotspots-fn)
            result (analyse {:samples {:allocation-trace sample-trace}})
            hotspots (get-in result [:allocation-hotspots :hotspots])
            string-hotspot (first (filter #(and (= "my.ns$fn"
                                                   (get-in % [:call-site :call-class]))
                                                (= "Ljava/lang/String;"
                                                   (:object-type %)))
                                          hotspots))]
        ;; Two String allocations from my.ns$fn: 48 + 64 = 112 bytes
        (is (= 2 (:count string-hotspot)))
        (is (= (+ 48 64) (:bytes string-hotspot)))
        (is (= 1 (:freed-count string-hotspot)))
        (is (= 48 (:freed-bytes string-hotspot)))
        (is (= "Ljava/lang/String;" (:object-type string-hotspot)))))

    (testing "respects :limit option"
      (let [analyse (analysis/hotspots-fn {:limit 1})
            result (analyse {:samples {:allocation-trace sample-trace}})
            hotspots (get-in result [:allocation-hotspots :hotspots])]
        (is (= 1 (count hotspots)))))

    (testing "sorts by :count when specified"
      (let [analyse (analysis/hotspots-fn {:order-by :count})
            result (analyse {:samples {:allocation-trace sample-trace}})
            hotspots (get-in result [:allocation-hotspots :hotspots])]
        ;; my.ns$fn + String has 2 allocations (highest count)
        (is (= "my.ns$fn"
               (get-in (first hotspots) [:call-site :call-class])))
        (is (= "Ljava/lang/String;" (:object-type (first hotspots))))))

    (testing "returns empty vector for empty trace"
      (let [analyse (analysis/hotspots-fn)
            result (analyse {:samples {:allocation-trace empty-trace}})
            hotspots (get-in result [:allocation-hotspots :hotspots])]
        (is (= [] hotspots))))

    (testing "uses custom :id option"
      (let [analyse (analysis/hotspots-fn {:id :my-hotspots})
            result (analyse {:samples {:allocation-trace sample-trace}})]
        (is (contains? result :my-hotspots))))

    (testing "falls back to alloc-* fields when call-* are empty"
      (let [trace-with-fallback {:type :criterium/allocation-trace
                                 :records [{:object-type "Ljava/lang/Object;"
                                            :object_size 16
                                            :call-class ""
                                            :call-method ""
                                            :call-file ""
                                            :call-line -1
                                            :alloc-class "java.lang.Object"
                                            :alloc-method "<init>"
                                            :alloc-file "Object.java"
                                            :alloc-line 50
                                            :thread 1
                                            :freed false}]
                                 :thread-id 1
                                 :eval-count 1
                                 :elapsed-time 1e6}
            analyse (analysis/hotspots-fn)
            result (analyse {:samples {:allocation-trace trace-with-fallback}})
            hotspots (get-in result [:allocation-hotspots :hotspots])
            hotspot (first hotspots)]
        (is (= "java.lang.Object" (get-in hotspot [:call-site :call-class])))
        (is (= "<init>" (get-in hotspot [:call-site :call-method])))
        (is (= "Object.java" (get-in hotspot [:call-site :call-file])))
        (is (= 50 (get-in hotspot [:call-site :call-line])))
        (is (= "Ljava/lang/Object;" (:object-type hotspot)))))))

(deftest by-type-fn-test
  (testing "by-type-fn"
    (testing "groups allocations by object type"
      (let [analyse (analysis/by-type-fn)
            result (analyse {:samples {:allocation-trace sample-trace}})
            by-type (get-in result [:allocation-by-type :by-type])]
        (is (= :criterium/allocation-by-type
               (get-in result [:allocation-by-type :type])))
        (is (= 3 (count by-type)))
        (is (contains? by-type "Ljava/lang/String;"))
        (is (contains? by-type "[J"))
        (is (contains? by-type "Ljava/lang/Long;"))))

    (testing "computes per-type statistics"
      (let [analyse (analysis/by-type-fn)
            result (analyse {:samples {:allocation-trace sample-trace}})
            string-stats (get-in result [:allocation-by-type
                                         :by-type
                                         "Ljava/lang/String;"])]
        (is (= 2 (:count string-stats)))
        (is (= (+ 48 64) (:bytes string-stats)))
        (is (= 1 (:freed-count string-stats)))
        (is (= 48 (:freed-bytes string-stats)))))

    (testing "returns empty map for empty trace"
      (let [analyse (analysis/by-type-fn)
            result (analyse {:samples {:allocation-trace empty-trace}})
            by-type (get-in result [:allocation-by-type :by-type])]
        (is (= {} by-type))))

    (testing "uses custom :id option"
      (let [analyse (analysis/by-type-fn {:id :my-by-type})
            result (analyse {:samples {:allocation-trace sample-trace}})]
        (is (contains? result :my-by-type))))))

;; Tests for treemap-fn.
;; Validates hierarchical treemap generation with various grouping, sizing,
;; and filtering options.
(deftest treemap-fn-test
  (testing "treemap-fn"
    (testing "with default options produces correct hierarchy"
      ;; Default: :class→line→type grouping, :bytes sizing, :all filtering
      (let [analyse (analysis/treemap-fn)
            result (analyse {:samples {:allocation-trace sample-trace}})
            treemap (:allocation-treemap result)
            root (:root treemap)]
        (is (= :criterium/allocation-treemap (:type treemap)))
        (is (= :class→line→type (:group-by treemap)))
        (is (= :bytes (:size-by treemap)))
        (is (= "allocations" (:name root)))
        ;; Total bytes: 48 + 64 + 1024 + 24 = 1160
        (is (= 1160 (:value root)))
        ;; Should have children for each call-class
        (is (contains? root :children))
        (is (= 2 (count (:children root))))))

    (testing "with :type→class→line grouping"
      (let [analyse (analysis/treemap-fn {:group-by :type→class→line})
            result (analyse {:samples {:allocation-trace sample-trace}})
            treemap (:allocation-treemap result)
            root (:root treemap)]
        (is (= :type→class→line (:group-by treemap)))
        ;; Should have children for each object-type (String, [J, Long)
        (is (= 3 (count (:children root))))))

    (testing "with :size-by :count"
      (let [analyse (analysis/treemap-fn {:size-by :count})
            result (analyse {:samples {:allocation-trace sample-trace}})
            root (get-in result [:allocation-treemap :root])]
        ;; Total count: 4 allocations
        (is (= 4 (:value root)))))

    (testing "with :size-by :bytes-per-allocation"
      ;; Total bytes = 1160, total count = 4
      ;; Average = 290.0
      (let [analyse (analysis/treemap-fn {:size-by :bytes-per-allocation})
            result (analyse {:samples {:allocation-trace sample-trace}})
            root (get-in result [:allocation-treemap :root])]
        ;; Value should be sum of children's bytes-per-allocation values
        (is (number? (:value root)))
        (is (pos? (:value root)))))

    (testing "with :filter-by :freed"
      ;; Only freed records: String (48 bytes) + [J (1024 bytes)
      (let [analyse (analysis/treemap-fn {:filter-by :freed})
            result (analyse {:samples {:allocation-trace sample-trace}})
            root (get-in result [:allocation-treemap :root])]
        (is (= (+ 48 1024) (:value root)))))

    (testing "with :filter-by :not-freed"
      ;; Only not-freed records: String (64 bytes) + Long (24 bytes)
      (let [analyse (analysis/treemap-fn {:filter-by :not-freed})
            result (analyse {:samples {:allocation-trace sample-trace}})
            root (get-in result [:allocation-treemap :root])]
        (is (= (+ 64 24) (:value root)))))

    (testing "returns unchanged data-map when trace absent"
      (let [analyse (analysis/treemap-fn)
            input {:other :data}
            result (analyse input)]
        (is (= input result))
        (is (not (contains? result :allocation-treemap)))))

    (testing "hierarchical values sum correctly"
      (let [analyse (analysis/treemap-fn)
            result (analyse {:samples {:allocation-trace sample-trace}})
            root (get-in result [:allocation-treemap :root])]
        ;; Sum of all children values should equal root value
        (let [children-sum (reduce + 0 (map :value (:children root)))]
          (is (= (:value root) children-sum)))
        ;; Check one level deeper - my.ns$fn class
        (let [my-ns-node (first (filter #(= "my.ns$fn" (:name %))
                                        (:children root)))]
          (when my-ns-node
            (let [children-sum (reduce + 0 (map :value (:children my-ns-node)))]
              (is (= (:value my-ns-node) children-sum)))))))

    (testing "uses custom :id option"
      (let [analyse (analysis/treemap-fn {:id :my-treemap})
            result (analyse {:samples {:allocation-trace sample-trace}})]
        (is (contains? result :my-treemap))
        (is (not (contains? result :allocation-treemap)))))

    (testing "uses custom :trace-id option"
      (let [analyse (analysis/treemap-fn {:trace-id [:custom :path]})
            result (analyse {:custom {:path sample-trace}})]
        (is (contains? result :allocation-treemap))))

    (testing "returns empty root for empty trace"
      (let [analyse (analysis/treemap-fn)
            result (analyse {:samples {:allocation-trace empty-trace}})
            root (get-in result [:allocation-treemap :root])]
        (is (= "allocations" (:name root)))
        (is (= 0 (:value root)))
        (is (not (contains? root :children)))))))

(deftest ->allocation-analyse-test
  (testing "->allocation-analyse"
    (testing "composes multiple analysis functions"
      (let [analyse (analysis/->allocation-analyse
                     [[:summary-fn {}]
                      [:hotspots-fn {:limit 5}]
                      [:by-type-fn {}]])
            result (analyse {:samples {:allocation-trace sample-trace}})]
        (is (contains? result :allocation-summary))
        (is (contains? result :allocation-hotspots))
        (is (contains? result :allocation-by-type))))

    (testing "functions are applied in order"
      (let [analyse (analysis/->allocation-analyse
                     [[:summary-fn {:id :first}]
                      [:summary-fn {:id :second}]])
            result (analyse {:samples {:allocation-trace sample-trace}})]
        (is (contains? result :first))
        (is (contains? result :second))))

    (testing "handles empty plan"
      (let [analyse (analysis/->allocation-analyse [])
            result (analyse {:samples {:allocation-trace sample-trace}})]
        (is (= sample-trace (get-in result [:samples :allocation-trace])))))

    (testing "handles nil plan"
      (let [analyse (analysis/->allocation-analyse nil)
            result (analyse {:samples {:allocation-trace sample-trace}})]
        (is (= sample-trace (get-in result [:samples :allocation-trace])))))

    (testing "throws on invalid plan type"
      (is (thrown? clojure.lang.ExceptionInfo
                   (analysis/->allocation-analyse :invalid))))))
