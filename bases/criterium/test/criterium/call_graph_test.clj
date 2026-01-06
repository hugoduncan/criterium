(ns criterium.call-graph-test
  ;; Tests for criterium.call-graph namespace.
  ;; Verifies the bench macro, last-bench storage, filter re-exports,
  ;; and viewer integration.
  (:require
   [clojure.test :refer [deftest is testing]]
   [criterium.agent :as agent]
   [criterium.call-graph :as cg]))

(deftest default-viewer-test
  (testing "default-viewer"
    (testing "returns the current default viewer"
      (is (= :print (cg/default-viewer))))

    (testing "set-default-viewer! changes the default"
      (let [original (cg/default-viewer)]
        (try
          (cg/set-default-viewer! :pprint)
          (is (= :pprint (cg/default-viewer)))
          (finally
            (cg/set-default-viewer! original)))))))

(deftest options->call-graph-plan-test
  (testing "options->call-graph-plan"
    (testing "returns default plan when no options provided"
      (let [plan (cg/options->call-graph-plan)]
        (is (= :print (:viewer plan)))
        (is (= [] (:analyse plan)))
        (is (= [:call-tree :call-flame] (:view plan)))))

    (testing "respects explicit viewer option"
      (let [plan (cg/options->call-graph-plan :viewer :portal)]
        (is (= :portal (:viewer plan)))))

    (testing "respects explicit view option"
      (let [plan (cg/options->call-graph-plan :view [:call-tree])]
        (is (= [:call-tree] (:view plan)))))

    (testing "respects explicit analyse option"
      (let [plan (cg/options->call-graph-plan :analyse [:some-analysis])]
        (is (= [:some-analysis] (:analyse plan)))))))

(deftest filter-re-exports-test
  (testing "filter-call-tree"
    (testing "is re-exported from criterium.agent"
      (is (= agent/filter-call-tree cg/filter-call-tree))))

  (testing "jdk-filter"
    (testing "is re-exported from criterium.agent"
      (is (= agent/jdk-filter cg/jdk-filter))))

  (testing "clojure-core-boundary-filter"
    (testing "is re-exported from criterium.agent"
      (is (= agent/clojure-core-boundary-filter cg/clojure-core-boundary-filter)))))

(deftest bench-macro-test
  ;; Tests bench macro functionality.
  ;; When agent is attached, verifies call tree is captured.
  ;; When agent is not attached, verifies graceful degradation.
  (testing "bench"
    (testing "returns the expression value"
      (with-out-str (cg/bench (+ 1 2))) ; suppress output
      (is (= 3 (cg/bench (+ 1 2) :view []))))

    (testing "stores results in last-bench"
      (cg/bench (+ 1 2) :view [])
      (let [results (cg/last-bench)]
        (is (some? results))
        (is (contains? results :data))
        (is (contains? results :viewer))
        (is (contains? results :analyse))
        (is (contains? results :view))))

    (testing "handles local bindings"
      (let [x 10
            y 20]
        (is (= 30 (cg/bench (+ x y) :view [])))))

    (if (agent/attached?)
      (testing "captures call tree when agent attached"
        (cg/bench (reduce + (range 5)) :view [])
        (let [call-tree (get-in (cg/last-bench) [:data :call-tree])]
          (is (some? call-tree) "call-tree should be present")
          (is (map? call-tree) "call-tree should be a map")
          (is (contains? call-tree :class) "call-tree has :class")
          (is (contains? call-tree :method) "call-tree has :method")
          (is (contains? call-tree :call-count) "call-tree has :call-count")))

      (testing "returns nil call-tree when agent not attached"
        (cg/bench (+ 1 2) :view [])
        (is (nil? (get-in (cg/last-bench) [:data :call-tree])))))))

(deftest bench-viewer-integration-test
  ;; Integration tests for viewer output.
  ;; Verifies that different viewers produce expected output.
  (testing ":print viewer"
    (testing "outputs call tree to stdout"
      (when (agent/attached?)
        (let [out (with-out-str (cg/bench (reduce + (range 5))))]
          (is (re-find #"Call Tree" out) "should print call tree header")
          (is (re-find #"total calls" out) "should show total calls")))))

  (testing ":view [:call-tree] option"
    (testing "shows only tree, not flame chart"
      (when (agent/attached?)
        (let [out (with-out-str (cg/bench (reduce + (range 5)) :view [:call-tree]))]
          (is (re-find #"Call Tree" out) "should print call tree")
          (is (not (re-find #"Flame Chart" out)) "should not print flame chart")))))

  (testing ":view [] option"
    (testing "suppresses all output"
      (let [out (with-out-str (cg/bench (+ 1 2) :view []))]
        (is (= "" out) "should produce no output")))))

(deftest filter-call-tree-integration-test
  ;; Tests filter functions work correctly with call tree data.
  ;; These tests require the agent to be attached; they are skipped otherwise.
  (testing "filter-call-tree"
    (if (agent/attached?)
      (do
        (cg/bench (reduce + (range 5)) :view [])
        (let [call-tree (get-in (cg/last-bench) [:data :call-tree])]
          (is (some? call-tree) "call-tree should be present when agent attached")
          (testing "with jdk-filter excludes java.* classes"
            (let [filtered (cg/filter-call-tree call-tree cg/jdk-filter)]
              (is (some? filtered) "filtered tree should not be nil")
              (is (not (re-find #"^java\." (or (:class filtered) "")))
                  "root should not be java.* class")))

          (testing "with max-depth limits depth"
            (let [filtered (cg/filter-call-tree call-tree {:max-depth 2})]
              (is (some? filtered))
              ;; Depth 2 means root + children, no grandchildren
              (doseq [child (:children filtered)]
                (is (empty? (:children child))
                    "children should have no children at depth 2"))))))
      ;; Agent not attached - verify filter works with nil input
      (testing "handles nil call-tree gracefully"
        (is (nil? (cg/filter-call-tree nil cg/jdk-filter)))))))

(deftest last-bench-test
  (testing "last-bench"
    (testing "returns nil or map"
      ;; Note: This test may return a map if other tests run first
      (is (or (nil? (cg/last-bench))
              (map? (cg/last-bench)))))

    (testing "stores results after bench"
      (cg/bench (+ 1 1) :view [])
      (let [result (cg/last-bench)]
        (is (map? result) "should return a map")
        (is (contains? result :data) "should contain :data key")
        (is (contains? result :viewer) "should contain :viewer key")))

    (testing "updates with different call-trees when agent attached"
      ;; When agent is attached, different expressions produce different call trees
      (when (agent/attached?)
        (cg/bench (reduce + (range 5)) :view [])
        (let [first-tree (get-in (cg/last-bench) [:data :call-tree])]
          (cg/bench (reduce * (range 1 5)) :view [])
          (let [second-tree (get-in (cg/last-bench) [:data :call-tree])]
            (is (some? first-tree) "first call-tree should be present")
            (is (some? second-tree) "second call-tree should be present")
            ;; Trees should have different structures due to different operations
            (is (not= first-tree second-tree)
                "different expressions should produce different call trees")))))))
