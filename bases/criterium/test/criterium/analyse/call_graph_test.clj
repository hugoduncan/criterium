(ns criterium.analyse.call-graph-test
  ;; Tests analysis functions for call graphs.
  ;; Verifies:
  ;; - most-called: aggregation, sorting, limit, empty handling
  ;; - filter-calls: exclude-packages, stop-at-packages, max-depth
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

;;; filter-calls tests

(def filter-test-tree
  "A call tree with various package prefixes for filter testing.
  Structure:
    myapp.Main (1 call)
    ├── java.util.List (5 calls)
    │   └── java.lang.Object (3 calls)
    ├── myapp.Service (10 calls)
    │   └── clojure.core.fn (8 calls)
    └── sun.misc.Unsafe (2 calls)"
  {:class "myapp.Main"
   :method "run"
   :file "Main.java"
   :line 10
   :call-count 1
   :children [{:class "java.util.List"
               :method "add"
               :file "List.java"
               :line 20
               :call-count 5
               :children [{:class "java.lang.Object"
                           :method "hashCode"
                           :file "Object.java"
                           :line 30
                           :call-count 3}]}
              {:class "myapp.Service"
               :method "process"
               :file "Service.java"
               :line 40
               :call-count 10
               :children [{:class "clojure.core"
                           :method "fn"
                           :file "core.clj"
                           :line 50
                           :call-count 8}]}
              {:class "sun.misc.Unsafe"
               :method "allocate"
               :file "Unsafe.java"
               :line 60
               :call-count 2}]})

(deftest filter-calls-basic-test
  (testing "filter-calls"
    (testing "stores filtered tree under default :filtered key"
      (let [data-map {:call-tree filter-test-tree}
            result ((analyse/filter-calls) data-map)]
        (is (contains? result :filtered))
        (is (= :criterium/filtered-call-tree
               (:type (:filtered result))))
        (is (= :call-tree (:source-id (:filtered result))))))

    (testing "preserves original call-tree"
      (let [data-map {:call-tree filter-test-tree}
            result ((analyse/filter-calls) data-map)]
        (is (= filter-test-tree (:call-tree data-map)))
        (is (contains? result :call-tree))))))

(deftest filter-calls-exclude-packages-test
  (testing "filter-calls with :exclude-packages"
    (testing "removes nodes matching excluded packages"
      (let [data-map {:call-tree filter-test-tree}
            result ((analyse/filter-calls
                     {:exclude-packages #{"java." "sun."}}) data-map)
            filtered (:call-tree (:filtered result))
            child-classes (set (map :class (:children filtered)))]
        (is (= "myapp.Main" (:class filtered)))
        (is (not (contains? child-classes "java.util.List")))
        (is (not (contains? child-classes "sun.misc.Unsafe")))
        (is (contains? child-classes "myapp.Service"))))

    (testing "stores filter options in result"
      (let [data-map {:call-tree filter-test-tree}
            opts {:exclude-packages #{"java."}}
            result ((analyse/filter-calls opts) data-map)]
        (is
         (=
          #{"java."}
          (get-in result [:filtered :filter-opts :exclude-packages])))))))

(deftest filter-calls-stop-at-packages-test
  (testing "filter-calls with :stop-at-packages"
    (testing "truncates children at matching packages"
      (let [data-map {:call-tree filter-test-tree}
            result ((analyse/filter-calls
                     {:stop-at-packages #{"clojure."}}) data-map)
            filtered (:call-tree (:filtered result))
            service-node (->> (:children filtered)
                              (filter #(= "myapp.Service" (:class %)))
                              first)
            clojure-node (->> (:children service-node)
                              (filter #(= "clojure.core" (:class %)))
                              first)]
        (is (some? clojure-node) "clojure.core node should exist")
        (is (empty? (:children clojure-node))
            "clojure.core children should be truncated")))))

(deftest filter-calls-max-depth-test
  (testing "filter-calls with :max-depth"
    (testing "limits tree depth to specified level"
      (let [data-map {:call-tree filter-test-tree}
            result ((analyse/filter-calls {:max-depth 2}) data-map)
            filtered (:call-tree (:filtered result))]
        (is (some? filtered) "root at depth 1 should exist")
        (is (seq (:children filtered)) "depth 2 children should exist")
        (doseq [child (:children filtered)]
          (is (empty? (:children child))
              "depth 3 grandchildren should be excluded"))))

    (testing "returns nil tree when max-depth excludes root"
      (let [data-map {:call-tree filter-test-tree}
            result ((analyse/filter-calls {:max-depth 0}) data-map)]
        (is (nil? (:call-tree (:filtered result))))))))

(deftest filter-calls-empty-test
  (testing "filter-calls with empty/nil input"
    (testing "returns unchanged data-map when call-tree is nil"
      (let [data-map {:call-tree nil}
            result ((analyse/filter-calls) data-map)]
        (is (= data-map result))))

    (testing "returns unchanged data-map when call-tree is missing"
      (let [data-map {:other-key "value"}
            result ((analyse/filter-calls) data-map)]
        (is (= data-map result))))))

(deftest filter-calls-custom-id-test
  (testing "filter-calls with custom IDs"
    (testing "uses custom :id"
      (let [data-map {:call-tree filter-test-tree}
            result ((analyse/filter-calls {:id :my-filtered}) data-map)]
        (is (contains? result :my-filtered))
        (is (not (contains? result :filtered)))))

    (testing "uses custom :call-tree-id"
      (let [data-map {:my-tree filter-test-tree}
            result ((analyse/filter-calls {:call-tree-id :my-tree}) data-map)]
        (is (= :my-tree (:source-id (:filtered result))))
        (is (some? (:call-tree (:filtered result))))))))
