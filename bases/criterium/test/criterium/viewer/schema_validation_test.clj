(ns criterium.viewer.schema-validation-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [criterium.viewer.schema-validation :as sv]))

;; Tests for schema validation utilities.
;; Verifies schema fetching, caching, and validation of Vega/Vega-Lite specs.

(deftest fetch-and-cache-schema-test
  (testing "fetch-and-cache-schema"
    (testing "caches schema to disk on first fetch"
      (let [url "https://vega.github.io/schema/vega/v5.json"
            path (sv/fetch-and-cache-schema url)
            cache-file (io/file path)]
        (is (string? path))
        (is (.exists cache-file))
        (is (pos? (.length cache-file)))))

    (testing "returns cached path on subsequent calls"
      (let [url "https://vega.github.io/schema/vega/v5.json"
            path1 (sv/fetch-and-cache-schema url)
            path2 (sv/fetch-and-cache-schema url)]
        (is (= path1 path2))))))

(deftest validate-vega-lite-spec-test
  (testing "validate-vega-lite-spec"
    (testing "returns valid for minimal valid spec"
      (let [spec {:data {:values []}
                  :mark "point"}
            result (sv/validate-vega-lite-spec spec)]
        (is (:valid? result))))

    (testing "returns valid for layered spec"
      (let [spec {:data {:values [{:x 1 :y 2}]}
                  :layer [{:mark "point"
                           :encoding {:x {:field "x" :type "quantitative"}
                                      :y {:field "y" :type "quantitative"}}}]}
            result (sv/validate-vega-lite-spec spec)]
        (is (:valid? result))))

    (testing "returns invalid with errors for invalid encoding type"
      (let [spec {:data {:values [{:x 1}]}
                  :mark "point"
                  :encoding {:x {:field "x" :type "invalid-type"}}}
            result (sv/validate-vega-lite-spec spec)]
        (is (not (:valid? result))
            (str "Expected invalid for bad encoding type, got: " result))
        (is (some? (:errors result))
            "Expected errors for invalid encoding type")))))

(deftest validate-vega-spec-test
  (testing "validate-vega-spec"
    (testing "returns valid for minimal valid spec"
      (let [spec {:$schema "https://vega.github.io/schema/vega/v5.json"
                  :width 400
                  :height 200
                  :data []
                  :marks []}
            result (sv/validate-vega-spec spec)]
        (is (:valid? result))))

    (testing "returns valid for treemap-style spec"
      (let [spec {:$schema "https://vega.github.io/schema/vega/v5.json"
                  :width 700
                  :height 400
                  :data [{:name "tree"
                          :values []
                          :transform [{:type "stratify"
                                       :key "id"
                                       :parentKey "parent"}
                                      {:type "treemap"
                                       :field "value"
                                       :method "squarify"
                                       :size [{:signal "width"}
                                              {:signal "height"}]}]}]
                  :scales [{:name "color"
                            :type "ordinal"
                            :range {:scheme "tableau10"}}]
                  :marks [{:type "rect"
                           :from {:data "tree"}
                           :encode {:enter {:fill {:value "blue"}}
                                    :update {:x {:field "x0"}
                                             :y {:field "y0"}}}}]}
            result (sv/validate-vega-spec spec)]
        (is (:valid? result))))

    (testing "returns invalid with errors for wrong root type"
      ;; Vega schema is permissive about object contents but requires root to be object
      (let [spec [1 2 3]  ; array instead of object
            result (sv/validate-vega-spec spec)]
        (is (not (:valid? result))
            (str "Expected invalid for array root, got: " result))
        (is (some? (:errors result))
            "Expected errors for wrong root type")))))
