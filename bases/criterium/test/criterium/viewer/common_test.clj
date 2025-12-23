(ns criterium.viewer.common-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.common :as common]))

;; Tests for ASCII treemap rendering functions.
;; Verifies ascii-bar generates proportional bars and render-ascii-treemap
;; produces correct tree structure with proper formatting, filtering, and depth limits.

(def sample-treemap
  "Sample allocation treemap for testing."
  {:type :criterium/allocation-treemap
   :group-by :class→line→type
   :size-by :bytes
   :root {:name "allocations"
          :value 1000000
          :children [{:name "MyClass"
                      :value 800000
                      :children [{:name "L42"
                                  :value 600000
                                  :children [{:name "java.lang.String"
                                              :value 400000}
                                             {:name "clojure.lang.Keyword"
                                              :value 200000}]}
                                 {:name "L58"
                                  :value 200000
                                  :children [{:name "java.lang.Long"
                                              :value 200000}]}]}
                     {:name "OtherClass"
                      :value 200000
                      :children [{:name "L10"
                                  :value 200000
                                  :children [{:name "java.util.HashMap"
                                              :value 200000}]}]}]}})

(deftest ascii-bar-test
  (testing "ascii-bar"
    (testing "produces correct proportional bars"
      (is (= "████████████████████" (common/ascii-bar 100.0 100.0 20)))
      (is (= "██████████" (common/ascii-bar 50.0 100.0 20)))
      (is (= "█████" (common/ascii-bar 25.0 100.0 20)))
      (is (= "██" (common/ascii-bar 10.0 100.0 20))))

    (testing "handles zero and negative values"
      (is (= "" (common/ascii-bar 0.0 100.0 20)))
      (is (= "" (common/ascii-bar -10.0 100.0 20)))
      (is (= "" (common/ascii-bar 100.0 0.0 20)))
      (is (= "" (common/ascii-bar 100.0 -10.0 20))))

    (testing "respects width parameter"
      (is (= "██████████" (common/ascii-bar 100.0 100.0 10)))
      (is (= "█████" (common/ascii-bar 100.0 100.0 5)))
      (is (= "██████████████████████████████" (common/ascii-bar 100.0 100.0 30))))

    (testing "caps at max width when value exceeds max"
      (is (= "████████████████████" (common/ascii-bar 150.0 100.0 20))))))

(deftest render-ascii-treemap-test
  (testing "render-ascii-treemap"
    (testing "produces correct tree structure"
      (let [result (common/render-ascii-treemap sample-treemap)
            lines (str/split-lines result)]
        (is (string? result))
        (is (str/starts-with? (first lines) "Allocation Treemap"))
        (is (str/includes? (first lines) "bytes"))
        (is (str/includes? (first lines) "class→line→type"))
        (is (str/includes? (second lines) "allocations/"))
        (is (some #(str/includes? % "├──") lines))
        (is (some #(str/includes? % "└──") lines))
        (is (some #(str/includes? % "│") lines))))

    (testing "shows non-leaf nodes with trailing slash"
      (let [result (common/render-ascii-treemap sample-treemap)]
        (is (str/includes? result "allocations/"))
        (is (str/includes? result "MyClass/"))
        (is (str/includes? result "L42/"))))

    (testing "shows leaf nodes without trailing slash"
      (let [result (common/render-ascii-treemap sample-treemap)]
        (is (str/includes? result "java.lang.String "))
        (is (not (str/includes? result "java.lang.String/")))))

    (testing "shows bars only on leaf nodes"
      (let [result (common/render-ascii-treemap sample-treemap)
            lines (str/split-lines result)]
        (doseq [line lines]
          (when (str/includes? line "█")
            (is (not (str/ends-with? (first (str/split line #"\[")) "/"))
                (str "Bar found on non-leaf: " line))))))

    (testing "respects depth-limit option"
      (let [result (common/render-ascii-treemap
                    sample-treemap
                    {:depth-limit 1})]
        (is (not (str/includes? result "L42")))
        (is (not (str/includes? result "java.lang.String")))
        (is (str/includes? result "MyClass/"))))

    (testing "respects depth-limit 2"
      (let [result (common/render-ascii-treemap
                    sample-treemap
                    {:depth-limit 2})]
        (is (str/includes? result "L42/"))
        (is (not (str/includes? result "java.lang.String")))))

    (testing "filters by min-percent"
      (let [result (common/render-ascii-treemap sample-treemap {:min-percent 25})]
        (is (str/includes? result "MyClass/"))
        (is (str/includes? result "L42/"))
        (is (not (str/includes? result "OtherClass")))
        (is (not (str/includes? result "L58")))))

    (testing "handles empty children"
      (let [empty-treemap {:type :criterium/allocation-treemap
                           :group-by :class→line→type
                           :size-by :bytes
                           :root {:name "allocations" :value 0}}
            result (common/render-ascii-treemap empty-treemap)]
        (is (string? result))
        (is (str/includes? result "Allocation Treemap"))
        (is (str/includes? result "allocations/"))))

    (testing "handles nil root"
      (let [nil-treemap {:type :criterium/allocation-treemap :root nil}
            result (common/render-ascii-treemap nil-treemap)]
        (is (= "" result))))

    (testing "formats sizes correctly"
      (let [result (common/render-ascii-treemap sample-treemap)]
        (is (or (str/includes? result "Kb")
                (str/includes? result "Mb")
                (str/includes? result "bytes")))))

    (testing "respects name-width option"
      (let [result-wide (common/render-ascii-treemap sample-treemap {:name-width 60})
            result-narrow (common/render-ascii-treemap sample-treemap {:name-width 30})
            lines-wide (str/split-lines result-wide)
            lines-narrow (str/split-lines result-narrow)]
        (is (> (count (second lines-wide)) (count (second lines-narrow))))))

    (testing "respects bar-width option"
      (let [result-wide (common/render-ascii-treemap sample-treemap {:bar-width 30})
            result-narrow (common/render-ascii-treemap sample-treemap {:bar-width 10})]
        (is (> (count (filter #(= % \█) result-wide))
               (count (filter #(= % \█) result-narrow))))))

    (testing "shows correct header for different size-by options"
      (is (str/includes?
           (common/render-ascii-treemap (assoc sample-treemap :size-by :count))
           "by count"))
      (is (str/includes?
           (common/render-ascii-treemap (assoc sample-treemap :size-by :bytes-per-allocation))
           "by bytes/alloc")))

    (testing "shows correct header for different group-by options"
      (is (str/includes?
           (common/render-ascii-treemap (assoc sample-treemap :group-by :type→class→line))
           "type→class→line")))))

(def deep-long-names-treemap
  "Treemap with deep nesting and long class names for alignment testing."
  {:type :criterium/allocation-treemap
   :group-by :class→line→type
   :size-by :bytes
   :root {:name "allocations"
          :value 248
          :children
          [{:name "clojure.lang.PersistentVector$TransientVector"
            :value 112
            :children [{:name "L767"
                        :value 72
                        :children [{:name "clojure.lang.PersistentVector$Node"
                                    :value 72}]}
                       {:name "L720"
                        :value 40
                        :children [{:name "clojure.lang.PersistentVector"
                                    :value 40}]}]}
           {:name "clojure.lang.PersistentVector"
            :value 72
            :children [{:name "L69"
                        :value 72
                        :children
                        [{:name "clojure.lang.PersistentVector$TransientVector"
                          :value 32}
                         {:name "clojure.lang.PersistentVector$Node"
                          :value 24}
                         {:name "java.util.concurrent.atomic.AtomicReference"
                          :value 16}]}]}
           {:name "clojure.lang.Compiler"
            :value 64
            :children [{:name "L7757"
                        :value 64
                        :children [{:name "clojure.lang.PersistentVector"
                                    :value 40}
                                   {:name "java.lang.Long"
                                    :value 24}]}]}]}})

(deftest treemap-alignment-test
  ;; Tests that treemap leaf bars start in the same column and tree structure
  ;; is preserved even with deep nesting and long class names that require truncation.
  (testing "treemap-alignment"
    (testing "aligns leaf bars in same column"
      (let [result (common/render-ascii-treemap deep-long-names-treemap)
            lines (str/split-lines result)
            leaf-lines (filter #(str/includes? % "█") lines)
            ;; Extract the position where the bar starts (first █ character)
            bar-positions (map #(.indexOf ^String % "█") leaf-lines)]
        ;; All bars should start at the same position
        (is (apply = bar-positions)
            (str "Bar positions differ: " (vec bar-positions)
                 "\nLines:\n" (str/join "\n" leaf-lines)))))

    (testing "preserves tree connectors without corruption"
      (let [result (common/render-ascii-treemap deep-long-names-treemap)]
        ;; Should not contain replacement characters
        (is (not (str/includes? result "�"))
            "Found replacement character in output")
        ;; All tree connectors should be intact
        (is (or (str/includes? result "├── ")
                (str/includes? result "├──"))
            "Missing branch connector")
        (is (or (str/includes? result "└── ")
                (str/includes? result "└──"))
            "Missing last-child connector")))

    (testing "truncates long names from left with ellipsis"
      (let [result (common/render-ascii-treemap deep-long-names-treemap
                                                {:name-width 40})]
        ;; Long names should be truncated with ellipsis prefix
        (is (str/includes? result "…")
            "Expected ellipsis for truncated names")))

    (testing "maintains fixed column width for all lines"
      (let [result (common/render-ascii-treemap deep-long-names-treemap
                                                {:name-width 40})
            lines (str/split-lines result)
            ;; Skip header line, check data lines
            data-lines (rest lines)
            ;; Find position of first [ in each line (start of size)
            bracket-positions (map #(.indexOf ^String % "[") data-lines)]
        ;; All size brackets should start at same position (column 41, 0-indexed 40)
        (is (apply = bracket-positions)
            (str "Size column positions differ: " (vec bracket-positions)
                 "\nLines:\n" (str/join "\n" data-lines)))))

    (testing "handles last-child at multiple nesting levels"
      ;; This specifically tests the case where continuation prefixes
      ;; are "    " (spaces) from multiple last-child ancestors
      (let [result (common/render-ascii-treemap deep-long-names-treemap)
            lines (str/split-lines result)]
        ;; The Compiler branch is last at level 1, L7757 is last at level 2
        ;; Their children should have proper tree structure
        (is (some #(and (str/includes? % "java.lang.Long")
                        (str/includes? % "└──"))
                  lines)
            "Expected java.lang.Long as last child with └── connector")))))
