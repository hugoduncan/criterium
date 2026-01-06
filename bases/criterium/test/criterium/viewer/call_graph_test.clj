(ns criterium.viewer.call-graph-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.test-utils :refer [trimmed-lines]]
   [criterium.view :as view]
   [criterium.viewer.call-graph :as call-graph]
   [criterium.viewer.common-charts :as charts]
   [criterium.viewer.kindly :as kindly]
   [criterium.viewer.portal :as portal])
  (:import
   [java.util Queue]))

;;; Test Data

(def simple-call-tree
  "A simple call tree with one child."
  {:class "myapp.Core"
   :method "main"
   :file "Core.java"
   :line 10
   :call-count 1
   :children
   [{:class "myapp.Service"
     :method "process"
     :file "Service.java"
     :line 20
     :call-count 5
     :children []}]})

(def nested-call-tree
  "A nested call tree for testing tree rendering."
  {:class "myapp.Core"
   :method "main"
   :file "Core.java"
   :line 10
   :call-count 1
   :children
   [{:class "myapp.Service"
     :method "process"
     :file "Service.java"
     :line 20
     :call-count 10
     :children
     [{:class "myapp.Helper"
       :method "compute"
       :file "Helper.java"
       :line 30
       :call-count 100
       :children []}
      {:class "myapp.Util"
       :method "format"
       :file "Util.java"
       :line 40
       :call-count 50
       :children []}]}
    {:class "myapp.Logger"
     :method "log"
     :file "Logger.java"
     :line 50
     :call-count 5
     :children []}]})

(def single-node-tree
  "A call tree with no children."
  {:class "myapp.Main"
   :method "run"
   :file "Main.java"
   :line 1
   :call-count 1
   :children []})

;;; Unit Tests

(deftest total-call-count-test
  ;; Tests that total-call-count correctly sums all call counts in a tree.
  (testing "total-call-count"
    (testing "returns 0 for nil"
      (is (= 0 (call-graph/total-call-count nil))))

    (testing "returns call-count for single node"
      (is (= 1 (call-graph/total-call-count single-node-tree))))

    (testing "sums all calls in simple tree"
      ;; 1 (root) + 5 (Service.process) = 6
      (is (= 6 (call-graph/total-call-count simple-call-tree))))

    (testing "sums all calls in nested tree"
      ;; 1 + 10 + 100 + 50 + 5 = 166
      (is (= 166 (call-graph/total-call-count nested-call-tree))))))

(deftest render-call-tree-test
  ;; Tests ASCII tree rendering with box-drawing characters.
  (testing "render-call-tree"
    (testing "returns nil for nil input"
      (is (nil? (call-graph/render-call-tree nil))))

    (testing "renders single node without tree characters"
      (let [result (call-graph/render-call-tree single-node-tree)]
        (is (= ["myapp.Main.run (1 call, 100.0%)"]
               (trimmed-lines result)))))

    (testing "renders simple tree with box characters"
      (let [result (call-graph/render-call-tree simple-call-tree)]
        (is (= ["myapp.Core.main (1 call, 16.7%)"
                "└── myapp.Service.process (5 calls, 83.3%)"]
               (trimmed-lines result)))))

    (testing "renders nested tree with proper indentation"
      (let [result (call-graph/render-call-tree nested-call-tree)
            lines (trimmed-lines result)]
        ;; Check structure
        (is (= 5 (count lines)))
        ;; Root
        (is (= "myapp.Core.main (1 call, 0.6%)" (nth lines 0)))
        ;; First child with ├──
        (is (= "├── myapp.Service.process (10 calls, 6.0%)" (nth lines 1)))
        ;; Grandchildren with │ prefix
        (is (= "│   ├── myapp.Helper.compute (100 calls, 60.2%)" (nth lines 2)))
        (is (= "│   └── myapp.Util.format (50 calls, 30.1%)" (nth lines 3)))
        ;; Last child with └──
        (is (= "└── myapp.Logger.log (5 calls, 3.0%)" (nth lines 4)))))

    (testing "uses 'call' singular for count of 1"
      (let [result (call-graph/render-call-tree single-node-tree)]
        (is (re-find #"\(1 call," result))))

    (testing "uses 'calls' plural for count > 1"
      (let [result (call-graph/render-call-tree simple-call-tree)]
        (is (re-find #"\(5 calls," result))))))

(deftest render-call-tree-edge-cases-test
  ;; Tests edge cases in tree rendering.
  (testing "render-call-tree edge cases"
    (testing "handles missing class name"
      (let [tree {:class nil
                  :method "unknown"
                  :call-count 1
                  :children []}
            result (call-graph/render-call-tree tree)]
        (is (= ["<unknown>.unknown (1 call, 100.0%)"]
               (trimmed-lines result)))))

    (testing "handles missing method name"
      (let [tree {:class "myapp.Test"
                  :method nil
                  :call-count 1
                  :children []}
            result (call-graph/render-call-tree tree)]
        (is (= ["myapp.Test.<unknown> (1 call, 100.0%)"]
               (trimmed-lines result)))))

    (testing "handles zero call count"
      (let [tree {:class "myapp.Test"
                  :method "test"
                  :call-count 0
                  :children []}
            result (call-graph/render-call-tree tree)]
        (is (= ["myapp.Test.test (0 calls, 0.0%)"]
               (trimmed-lines result)))))

    (testing "handles deeply nested tree"
      (let [deep-tree {:class "L1"
                       :method "m"
                       :call-count 1
                       :children
                       [{:class "L2"
                         :method "m"
                         :call-count 1
                         :children
                         [{:class "L3"
                           :method "m"
                           :call-count 1
                           :children
                           [{:class "L4"
                             :method "m"
                             :call-count 1
                             :children []}]}]}]}
            result (call-graph/render-call-tree deep-tree)
            lines (trimmed-lines result)]
        (is (= 4 (count lines)))
        ;; Check indentation increases correctly
        (is (= "L1.m (1 call, 25.0%)" (nth lines 0)))
        (is (= "└── L2.m (1 call, 25.0%)" (nth lines 1)))
        (is (= "└── L3.m (1 call, 25.0%)" (nth lines 2)))
        (is (= "└── L4.m (1 call, 25.0%)" (nth lines 3)))))))

;;; View Integration Tests

(deftest call-tree-view-print-test
  ;; Tests the :print viewer integration for call-tree.
  (testing "call-tree*"
    (testing "prints tree with header"
      (let [output (with-out-str
                     (view/call-tree*
                      :print
                      {}
                      {:call-tree simple-call-tree}))
            lines (trimmed-lines output)]
        (is (= "Call Tree (6 total calls)" (first lines)))
        (is (= "myapp.Core.main (1 call, 16.7%)" (second lines)))))

    (testing "uses custom call-tree-id"
      (let [output (with-out-str
                     (view/call-tree*
                      :print
                      {:call-tree-id :my-tree}
                      {:my-tree single-node-tree}))
            lines (trimmed-lines output)]
        (is (= "Call Tree (1 total calls)" (first lines)))))

    (testing "handles missing call-tree gracefully"
      (let [output (with-out-str
                     (view/call-tree*
                      :print
                      {}
                      {}))]
        (is (= "" output))))

    (testing "handles nil call-tree gracefully"
      (let [output (with-out-str
                     (view/call-tree*
                      :print
                      {}
                      {:call-tree nil}))]
        (is (= "" output))))))

(deftest call-flame-view-print-test
  ;; Tests the :print viewer integration for call-flame.
  (testing "call-flame* :print"
    (testing "prints message directing to visual viewers"
      (let [output (with-out-str
                     (view/call-flame*
                      :print
                      {}
                      {:call-tree simple-call-tree}))]
        (is (str/includes? output "Flame Chart"))
        (is (str/includes? output ":portal"))))

    (testing "handles missing call-tree gracefully"
      (let [output (with-out-str
                     (view/call-flame*
                      :print
                      {}
                      {}))]
        (is (= "" output))))

    (testing "handles nil call-tree gracefully"
      (let [output (with-out-str
                     (view/call-flame*
                      :print
                      {}
                      {:call-tree nil}))]
        (is (= "" output))))))

(deftest call-tree-view-none-test
  ;; Tests the :none viewer does nothing.
  (testing "call-tree* :none"
    (testing "produces no output"
      (let [output (with-out-str
                     (view/call-tree*
                      :none
                      {}
                      {:call-tree nested-call-tree}))]
        (is (= "" output))))))

;;; Chart Generation Tests

(deftest call-tree-tree-vega-spec-test
  ;; Tests the Vega spec generation for tree diagram.
  (testing "call-tree-tree-vega-spec"
    (testing "generates valid Vega spec for nested tree"
      (let [spec (charts/call-tree-tree-vega-spec nested-call-tree {})]
        (is (str/includes? (:$schema spec) "vega/v5.json"))
        (is (= 700 (:width spec)))
        (is (= 500 (:height spec)))
        (is (contains? spec :data))
        (is (contains? spec :marks))
        (is (contains? spec :scales))))

    (testing "respects width/height options"
      (let [spec (charts/call-tree-tree-vega-spec nested-call-tree
                                                  {:width 800 :height 600})]
        (is (= 800 (:width spec)))
        (is (= 600 (:height spec)))))

    (testing "handles nil call-tree"
      (let [spec (charts/call-tree-tree-vega-spec nil {})]
        (is (map? spec))
        (is (= [] (get-in spec [:data 0 :values])))))))

(deftest call-tree-flame-vega-spec-test
  ;; Tests the Vega spec generation for flame chart.
  (testing "call-tree-flame-vega-spec"
    (testing "generates valid Vega spec for nested tree"
      (let [total-calls (call-graph/total-call-count nested-call-tree)
            spec (charts/call-tree-flame-vega-spec nested-call-tree total-calls {})]
        (is (str/includes? (:$schema spec) "vega/v5.json"))
        (is (= 700 (:width spec)))
        (is (contains? spec :data))
        (is (contains? spec :marks))
        (is (contains? spec :scales))
        ;; Check flame data was computed
        (let [flame-data (get-in spec [:data 0 :values])]
          (is (vector? flame-data))
          (is (= 5 (count flame-data)))
          ;; Check first node (root) - uses short class name for readability
          (let [root (first flame-data)]
            (is (= "Core.main" (:name root)))
            (is (= 0 (:x0 root)))
            (is (= 0 (:depth root)))))))

    (testing "respects width option"
      (let [total-calls (call-graph/total-call-count simple-call-tree)
            spec (charts/call-tree-flame-vega-spec simple-call-tree total-calls
                                                   {:width 500})]
        (is (= 500 (:width spec)))))

    (testing "handles nil call-tree"
      (let [spec (charts/call-tree-flame-vega-spec nil 0 {})]
        (is (map? spec))
        (is (= [] (get-in spec [:data 0 :values])))))))

;;; Portal Tests

(defmacro with-tap-out
  "Capture tapped values during body execution."
  [& body]
  `(let [v# (volatile! [])
         f# (fn [x#]
              (when-not (= ::portal/_ x#)
                (vswap! v# conj x#)))]
     (try
       (add-tap f#)
       ~@body
       (loop []
         (when-not (.isEmpty ^Queue @#'clojure.core/tapq)
           (recur)))
       (loop []
         (when (empty? @v#)
           (recur)))
       (portal/flush)
       @v#
       (finally
         (remove-tap f#)))))

(deftest call-tree-view-portal-test
  ;; Tests the :portal viewer integration for call-tree.
  (testing "call-tree* :portal"
    (testing "outputs heading and tree diagram"
      (let [outputs (with-tap-out
                      (view/call-tree*
                       :portal
                       {}
                       {:call-tree nested-call-tree}))]
        (is (= 2 (count outputs))
            "Expected 2 outputs: heading and tree")
        (let [[heading tree-spec] outputs]
          (is (= :b (first heading)))
          (is (str/includes? (second heading) "Call Tree"))
          (is (str/includes? (second heading) "166"))
          ;; Tree spec
          (is (str/includes? (:$schema tree-spec) "vega"))
          (is (= :portal.viewer/vega (:portal.viewer/default (meta tree-spec)))))))

    (testing "uses custom call-tree-id"
      (let [outputs (with-tap-out
                      (view/call-tree*
                       :portal
                       {:call-tree-id :my-tree}
                       {:my-tree simple-call-tree}))]
        (is (= 2 (count outputs)))
        (let [[heading _] outputs]
          (is (str/includes? (second heading) "6")))))

    (testing "handles missing call-tree gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/call-tree* :portal {} {})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))

    (testing "handles nil call-tree gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/call-tree* :portal {} {:call-tree nil})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

(deftest call-flame-view-portal-test
  ;; Tests the :portal viewer integration for call-flame.
  (testing "call-flame* :portal"
    (testing "outputs heading and flame chart"
      (let [outputs (with-tap-out
                      (view/call-flame*
                       :portal
                       {}
                       {:call-tree nested-call-tree}))]
        (is (= 2 (count outputs))
            "Expected 2 outputs: heading and flame chart")
        (let [[heading flame-spec] outputs]
          (is (= :b (first heading)))
          (is (str/includes? (second heading) "Flame"))
          ;; Flame spec
          (is (str/includes? (:$schema flame-spec) "vega"))
          (is (= :portal.viewer/vega (:portal.viewer/default (meta flame-spec)))))))

    (testing "handles missing call-tree gracefully"
      (let [v (volatile! [])
            f (fn [x] (when-not (= ::portal/_ x) (vswap! v conj x)))]
        (try
          (add-tap f)
          (view/call-flame* :portal {} {})
          (portal/flush)
          (is (empty? @v))
          (finally
            (remove-tap f)))))))

;;; Kindly Tests

(deftest call-tree-view-kindly-test
  ;; Tests the :kindly viewer integration for call-tree.
  (testing "call-tree* :kindly"
    (testing "outputs heading and tree diagram"
      (reset! kindly/accumulated [])
      (view/call-tree*
       :kindly
       {}
       {:call-tree nested-call-tree})
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected 2 elements: heading and tree")
        (let [[heading tree-spec] result]
          ;; Heading
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (str/includes? (first heading) "Call Tree"))
          (is (str/includes? (first heading) "166"))
          ;; Tree spec
          (is (= :kind/vega (:kindly/kind (meta tree-spec))))
          (is (str/includes? (:$schema tree-spec) "vega")))))

    (testing "uses custom call-tree-id"
      (reset! kindly/accumulated [])
      (view/call-tree*
       :kindly
       {:call-tree-id :my-tree}
       {:my-tree simple-call-tree})
      (let [result (kindly/flush)
            [heading _] result]
        (is (str/includes? (first heading) "6"))))

    (testing "handles missing call-tree gracefully"
      (reset! kindly/accumulated [])
      (view/call-tree* :kindly {} {})
      (is (nil? (kindly/flush))))

    (testing "handles nil call-tree gracefully"
      (reset! kindly/accumulated [])
      (view/call-tree* :kindly {} {:call-tree nil})
      (is (nil? (kindly/flush))))))

(deftest call-flame-view-kindly-test
  ;; Tests the :kindly viewer integration for call-flame.
  (testing "call-flame* :kindly"
    (testing "outputs heading and flame chart"
      (reset! kindly/accumulated [])
      (view/call-flame*
       :kindly
       {}
       {:call-tree nested-call-tree})
      (let [result (kindly/flush)]
        (is (= :kind/fragment (:kindly/kind (meta result))))
        (is (= 2 (count result))
            "Expected 2 elements: heading and flame")
        (let [[heading flame-spec] result]
          ;; Heading
          (is (= :kind/md (:kindly/kind (meta heading))))
          (is (str/includes? (first heading) "Flame"))
          ;; Flame spec
          (is (= :kind/vega (:kindly/kind (meta flame-spec))))
          (is (str/includes? (:$schema flame-spec) "vega")))))

    (testing "handles missing call-tree gracefully"
      (reset! kindly/accumulated [])
      (view/call-flame* :kindly {} {})
      (is (nil? (kindly/flush))))

    (testing "handles nil call-tree gracefully"
      (reset! kindly/accumulated [])
      (view/call-flame* :kindly {} {:call-tree nil})
      (is (nil? (kindly/flush))))))
