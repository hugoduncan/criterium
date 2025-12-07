(ns build.tasks.javac-test
    (:require
     [babashka.fs :as fs]
     [build.tasks.javac :as javac]
     [clojure.test :refer [deftest is testing]]
     [clojure.tools.build.api :as b]))

(deftest javac-with-explicit-src-dirs-test
  ;; Test javac compilation with explicitly specified source directories
  ;; Verifies that Java sources compile to .class files correctly
         (testing "javac"
                  (testing "compiles Java sources with explicit src-dirs"
                           (let [temp-dir (fs/create-temp-dir {:prefix "javac-test"})
                                 class-dir (fs/path temp-dir "classes")
                                 src-dir (fs/path temp-dir "src")]
                                (try
                                 (fs/create-dirs class-dir)
                                 (fs/create-dirs (fs/path src-dir "com" "example"))

                                 (spit (fs/file src-dir "com" "example" "Hello.java")
                                       "package com.example;\npublic class Hello {\n  public static String greet() { return \"Hello\"; }\n}")

                                 (let [opts {:src-dirs [(str src-dir)]
                                             :class-dir (str class-dir)}
                                       result (javac/javac opts)]

                                      (testing "returns opts with populated fields"
                                               (is (= [(str src-dir)] (:src-dirs result)))
                                               (is (some? (:basis result)))
                                               (is (= ["--release" "11"] (:javac-opts result))))

                                      (testing "creates .class files"
                                               (is (fs/exists? (fs/path class-dir "com" "example" "Hello.class")))))

                                 (finally
                                  (fs/delete-tree temp-dir)))))))

(deftest javac-with-project-test
  ;; Test javac with project parameter
  ;; Verifies that :project :agent correctly resolves src-dirs
         (testing "javac"
                  (testing "resolves src-dirs for agent project"
                           (let [temp-dir (fs/create-temp-dir {:prefix "javac-test"})
                                 class-dir (fs/path temp-dir "classes")
                                 src-dir (fs/path temp-dir "src" "java")]
                                (try
                                 (fs/create-dirs class-dir)
                                 (fs/create-dirs (fs/path src-dir "criterium" "agent"))

                                 (spit (fs/file src-dir "criterium" "agent" "TestClass.java")
                                       "package criterium.agent;\npublic class TestClass {}")

                                 (let [opts {:project :agent
                                             :src-dirs [(str src-dir)]
                                             :class-dir (str class-dir)}
                                       result (javac/javac opts)]

                                      (testing "preserves provided src-dirs when given"
                                               (is (= [(str src-dir)] (:src-dirs result))))

                                      (testing "creates TestClass.class"
                                               (is (fs/exists? (fs/path class-dir "criterium" "agent" "TestClass.class")))))

                                 (finally
                                  (fs/delete-tree temp-dir)))))))

(deftest javac-with-custom-opts-test
  ;; Test javac with custom compiler options
  ;; Verifies that custom javac-opts are respected
         (testing "javac"
                  (testing "uses custom javac-opts when provided"
                           (let [temp-dir (fs/create-temp-dir {:prefix "javac-test"})
                                 class-dir (fs/path temp-dir "classes")
                                 src-dir (fs/path temp-dir "src")]
                                (try
                                 (fs/create-dirs class-dir)
                                 (fs/create-dirs (fs/path src-dir "com" "example"))

                                 (spit (fs/file src-dir "com" "example" "Simple.java")
                                       "package com.example;\npublic class Simple {}")

                                 (let [custom-opts ["-Xlint:unchecked"]
                                       opts {:src-dirs [(str src-dir)]
                                             :class-dir (str class-dir)
                                             :javac-opts custom-opts}
                                       result (javac/javac opts)]

                                      (testing "preserves custom javac-opts"
                                               (is (= custom-opts (:javac-opts result))))

                                      (testing "compiles successfully"
                                               (is (fs/exists? (fs/path class-dir "com" "example" "Simple.class")))))

                                 (finally
                                  (fs/delete-tree temp-dir)))))))

(deftest javac-without-basis-test
  ;; Test javac creates basis when not provided
  ;; Verifies automatic basis creation functionality
         (testing "javac"
                  (testing "creates basis when not provided"
                           (let [temp-dir (fs/create-temp-dir {:prefix "javac-test"})
                                 class-dir (fs/path temp-dir "classes")
                                 src-dir (fs/path temp-dir "src")]
                                (try
                                 (fs/create-dirs class-dir)
                                 (fs/create-dirs (fs/path src-dir "test"))

                                 (spit (fs/file src-dir "test" "Minimal.java")
                                       "package test;\npublic class Minimal {}")

                                 (let [opts {:src-dirs [(str src-dir)]
                                             :class-dir (str class-dir)}
                                       result (javac/javac opts)]

                                      (testing "creates basis automatically"
                                               (is (some? (:basis result)))))

                                 (finally
                                  (fs/delete-tree temp-dir)))))))

(deftest javac-default-src-dirs-test
  ;; Test javac default src-dir resolution from project
  ;; Verifies that :project without explicit :src-dirs resolves correctly
         (testing "javac"
                  (testing "sets default src-dirs based on project"
                           (let [temp-dir (fs/create-temp-dir {:prefix "javac-test"})
                                 class-dir (fs/path temp-dir "classes")]
                                (try
                                 (fs/create-dirs class-dir)

                                 (let [opts {:project :agent
                                             :class-dir (str class-dir)}
                                       result (javac/javac opts)]

                                      (testing "sets agent default src-dirs"
                                               (is (= ["bases/agent/src/java"] (:src-dirs result)))))

                                 (finally
                                  (fs/delete-tree temp-dir)))))))
