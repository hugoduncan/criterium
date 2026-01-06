(ns criterium.analyse.call-graph-test
  ;; Tests the most-called analysis function for call graphs.
  ;; Verifies:
  ;; - Flattening call trees into aggregated method lists
  ;; - Sorting by total call count
  ;; - Respecting limit option
  ;; - Handling empty/nil call trees
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.analyse :as analyse]))

(def sample-call-tree
  "A sample call tree for testing.
  Structure:
    root (1 call)
    ├── child-a (10 calls)
    │   └── grandchild-a1 (5 calls)
    └── child-b (3 calls)
        ├── grandchild-b1 (2 calls)
        └── grandchild-b1 (7 calls) - same method, different location"
  {:class "com.example.Root"
   :method "main"
   :file "Root.java"
   :line 10
   :call-count 1
   :children [{:class "com.example.ChildA"
               :method "process"
               :file "ChildA.java"
               :line 20
               :call-count 10
               :children [{:class "com.example.Grandchild"
                           :method "compute"
                           :file "Grandchild.java"
                           :line 30
                           :call-count 5}]}
              {:class "com.example.ChildB"
               :method "handle"
               :file "ChildB.java"
               :line 40
               :call-count 3
               :children [{:class "com.example.Grandchild"
                           :method "compute"
                           :file "Grandchild.java"
                           :line 30
                           :call-count 2}
                          {:class "com.example.Grandchild"
                           :method "compute"
                           :file "Other.java"
                           :line 50
                           :call-count 7}]}]})

(deftest most-called-basic-test
  (testing "most-called"
    (testing "aggregates methods by class+method"
      (let [data-map {:call-tree sample-call-tree}
            result ((analyse/most-called) data-map)
            most-called (:most-called (:most-called result))]
        (is (= 4 (count most-called))
            "should have 4 unique class+method combinations")
        (is (= 14 (:total-calls (first most-called)))
            "Grandchild.compute should have 5+2+7=14 total calls")
        (is (= "com.example.Grandchild" (:class (first most-called))))
        (is (= "compute" (:method (first most-called))))))

    (testing "sorts by total call count descending"
      (let [data-map {:call-tree sample-call-tree}
            result ((analyse/most-called) data-map)
            calls (mapv :total-calls (:most-called (:most-called result)))]
        (is (= calls (reverse (sort calls)))
            "should be sorted descending")))

    (testing "includes file and line from first occurrence"
      (let [data-map {:call-tree sample-call-tree}
            result ((analyse/most-called) data-map)
            grandchild (first (:most-called (:most-called result)))]
        (is (= "Grandchild.java" (:file grandchild)))
        (is (= 30 (:line grandchild)))))

    (testing "sets correct result type"
      (let [data-map {:call-tree sample-call-tree}
            result ((analyse/most-called) data-map)]
        (is (= :criterium/most-called (:type (:most-called result))))
        (is (= :call-tree (:source-id (:most-called result))))))))

(deftest most-called-limit-test
  (testing "most-called with limit"
    (testing "respects :limit option"
      (let [data-map {:call-tree sample-call-tree}
            result ((analyse/most-called {:limit 2}) data-map)
            most-called (:most-called (:most-called result))]
        (is (= 2 (count most-called)))
        (is (= 2 (:limit (:most-called result))))))

    (testing "returns all when limit exceeds count"
      (let [data-map {:call-tree sample-call-tree}
            result ((analyse/most-called {:limit 100}) data-map)
            most-called (:most-called (:most-called result))]
        (is (= 4 (count most-called)))))))

(deftest most-called-empty-test
  (testing "most-called with empty/nil input"
    (testing "returns unchanged data-map when call-tree is nil"
      (let [data-map {:call-tree nil}
            result ((analyse/most-called) data-map)]
        (is (= data-map result))))

    (testing "returns unchanged data-map when call-tree is missing"
      (let [data-map {:other-key "value"}
            result ((analyse/most-called) data-map)]
        (is (= data-map result))))))

(deftest most-called-custom-id-test
  (testing "most-called with custom IDs"
    (testing "uses custom :id"
      (let [data-map {:call-tree sample-call-tree}
            result ((analyse/most-called {:id :custom-most-called}) data-map)]
        (is (contains? result :custom-most-called))
        (is (not (contains? result :most-called)))))

    (testing "uses custom :call-tree-id"
      (let [data-map {:my-tree sample-call-tree}
            result ((analyse/most-called {:call-tree-id :my-tree}) data-map)]
        (is (= :my-tree (:source-id (:most-called result))))
        (is (seq (:most-called (:most-called result))))))))
