(ns build.defaults-test
  (:require
   [build.defaults :as sut]
   [clojure.test :refer [deftest is testing]]
   [malli.core :as m]))

(deftest project-coordinate-schema-test
  ;; Verify project-coordinate-schema correctly validates coordinate maps
  (testing "project-coordinate-schema"
    (testing "validates valid coordinates"
      (is (m/validate sut/project-coordinate-schema
                      {:lib 'criterium/criterium
                       :name "criterium/criterium"
                       :version "0.5.246-ALPHA"}))
      (is (m/validate sut/project-coordinate-schema
                      {:lib 'criterium/agent
                       :name "criterium/agent"
                       :version "1.0.0"
                       :manifest {"Agent-Class" "criterium.agent"}})))

    (testing "rejects coordinates missing required keys"
      (is (not (m/validate sut/project-coordinate-schema
                           {:lib 'criterium/criterium
                            :name "criterium/criterium"})))
      (is (not (m/validate sut/project-coordinate-schema
                           {:name "criterium/criterium"
                            :version "0.5.0"}))))

    (testing "rejects coordinates with wrong value types"
      (is (not (m/validate sut/project-coordinate-schema
                           {:lib "not-a-symbol"
                            :name "criterium/criterium"
                            :version "0.5.0"})))
      (is (not (m/validate sut/project-coordinate-schema
                           {:lib 'criterium/criterium
                            :name :not-a-string
                            :version "0.5.0"})))
      (is (not (m/validate sut/project-coordinate-schema
                           {:lib 'criterium/criterium
                            :name "criterium/criterium"
                            :version 123}))))))

(deftest project-coordinates-test
  ;; Verify that all entries in project-coordinates conform to the schema
  (testing "all project-coordinates entries are valid"
    (doseq [[project coords] sut/project-coordinates]
      (testing (str "project " project)
        (is
         (m/validate sut/project-coordinate-schema coords)
         (str
          "Coordinates for "
          project
          " should validate against schema"))))))

(deftest project-data-test
  ;; Verify project-data function behavior
  (testing "project-data"
    (testing "returns coordinates merged with params for valid project"
      (let [result (sut/project-data {:project :criterium :extra "value"})]
        (is (= 'org.hugoduncan/criterium (:lib result)))
        (is (= "org.hugoduncan/criterium" (:name result)))
        (is (string? (:version result)))
        (is (= "value" (:extra result)))))

    (testing "throws for unknown project"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Unknown project"
                            (sut/project-data {:project :unknown}))))

    (testing "validates coordinates against schema"
      (with-redefs [sut/project-coordinates
                    {:invalid {:lib "not-a-symbol"
                               :name "test"
                               :version "1.0.0"}}]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Invalid project coordinates"
                              (sut/project-data {:project :invalid})))))))
