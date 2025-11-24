(ns build.tasks.smoke-test
    (:require
     [babashka.fs :as fs]
     [clojure.java.io :as io]
     [clojure.java.shell :as shell]
     [clojure.test :refer [deftest is testing]]
     [clojure.xml :as xml])
    (:import
     [java.io ByteArrayInputStream]
     [java.util.jar JarFile Manifest]))

;;; JAR validation

(defn- jar-exists?
       "Check if JAR file exists at the expected location."
       [project-root jar-name]
       (let [jar-path (fs/path project-root "target" jar-name)]
            (fs/exists? jar-path)))

(defn- read-jar-manifest
       "Read the manifest from a JAR file."
       [jar-path]
       (with-open [jar (JarFile. (str jar-path))]
                  (when-let [manifest (.getManifest jar)]
                            (let [attrs (.getMainAttributes manifest)]
                                 (into {} (map (fn [[k v]] [(str k) (str v)]) attrs))))))

(defn- list-jar-entries
       "List all entries in a JAR file."
       [jar-path]
       (with-open [jar (JarFile. (str jar-path))]
                  (vec (map #(.getName ^java.util.jar.JarEntry %) (enumeration-seq (.entries jar))))))

(defn- jar-contains-pom?
       "Check if JAR contains a POM file."
       [jar-path]
       (let [entries (list-jar-entries jar-path)]
            (some #(and (.startsWith ^String % "META-INF/maven/")
                        (.endsWith ^String % "/pom.xml"))
                  entries)))

(defn- read-pom-from-jar
       "Extract and parse POM XML from JAR."
       [jar-path]
       (with-open [jar (JarFile. (str jar-path))]
                  (let [entries (enumeration-seq (.entries jar))
                        pom-entry (first (filter #(and (.startsWith (.getName ^java.util.jar.JarEntry %) "META-INF/maven/")
                                                       (.endsWith (.getName ^java.util.jar.JarEntry %) "/pom.xml"))
                                                 entries))]
                       (when pom-entry
                             (with-open [is (.getInputStream jar pom-entry)]
                                        (xml/parse is))))))

;;; POM validation

(defn- find-element
       "Find first child element with given tag."
       [xml-node tag]
       (->> (:content xml-node)
            (filter map?)
            (filter #(= (:tag %) tag))
            first))

(defn- element-text
       "Extract text content from XML element."
       [element]
       (when element
             (first (filter string? (:content element)))))

(defn- verify-pom-required-fields
       "Verify POM contains all required Maven Central/Clojars fields."
       [pom-xml]
       (let [group-id (element-text (find-element pom-xml :groupId))
             artifact-id (element-text (find-element pom-xml :artifactId))
             version (element-text (find-element pom-xml :version))
             description (element-text (find-element pom-xml :description))
             url (element-text (find-element pom-xml :url))
             licenses (find-element pom-xml :licenses)
             scm (find-element pom-xml :scm)
             developers (find-element pom-xml :developers)]
            {:group-id group-id
             :artifact-id artifact-id
             :version version
             :description description
             :url url
             :has-license? (some? licenses)
             :has-scm? (some? scm)
             :has-developers? (some? developers)
             :all-required-present? (and group-id artifact-id version description
                                         url licenses scm developers)}))

;;; Smoke tests

(deftest criterium-jar-smoke-test
  ;; Test validates the criterium JAR artifact produced by the build system
         (testing "criterium JAR"
                  (testing "exists in target directory"
                           (is (jar-exists? "projects/criterium" "criterium.jar")))

                  (let [jar-path (fs/path "projects/criterium/target/criterium.jar")]
                       (testing "contains POM file"
                                (is (jar-contains-pom? jar-path)))

                       (testing "POM has all required metadata"
                                (let [pom-xml (read-pom-from-jar jar-path)
                                      validation (verify-pom-required-fields pom-xml)]
                                     (is (= "criterium" (:group-id validation)))
                                     (is (= "criterium" (:artifact-id validation)))
                                     (is (some? (:version validation)))
                                     (is (= "Benchmarking library for Clojure with statistical rigor"
                                            (:description validation)))
                                     (is (= "https://github.com/hugoduncan/criterium"
                                            (:url validation)))
                                     (is (:has-license? validation))
                                     (is (:has-scm? validation))
                                     (is (:has-developers? validation))
                                     (is (:all-required-present? validation))))

                       (testing "contains expected entries"
                                (let [entries (list-jar-entries jar-path)]
                                     (is (some #(.startsWith ^String % "criterium/") entries)
                                         "JAR should contain criterium namespace files"))))))

(deftest agent-jar-smoke-test
  ;; Test validates the agent JAR artifact including Java compilation and manifest
         (testing "agent JAR"
                  (testing "exists in target directory"
                           (is (jar-exists? "projects/agent" "agent.jar")))

                  (let [jar-path (fs/path "projects/agent/target/agent.jar")]
                       (testing "contains POM file"
                                (is (jar-contains-pom? jar-path)))

                       (testing "POM has all required metadata"
                                (let [pom-xml (read-pom-from-jar jar-path)
                                      validation (verify-pom-required-fields pom-xml)]
                                     (is (= "criterium" (:group-id validation)))
                                     (is (= "criterium.agent" (:artifact-id validation)))
                                     (is (some? (:version validation)))
                                     (is (= "JVM agent for allocation tracking in Criterium benchmarks"
                                            (:description validation)))
                                     (is (= "https://github.com/hugoduncan/criterium"
                                            (:url validation)))
                                     (is (:has-license? validation))
                                     (is (:has-scm? validation))
                                     (is (:has-developers? validation))
                                     (is (:all-required-present? validation))))

                       (testing "manifest contains Agent-Class"
                                (let [manifest (read-jar-manifest jar-path)]
                                     (is (= "criterium.agent" (get manifest "Agent-Class")))))

                       (testing "contains compiled Java classes"
                                (let [entries (list-jar-entries jar-path)]
                                     (is (some #(= "criterium/agent/Agent.class" %) entries))
                                     (is (some #(= "criterium/agent/Allocation.class" %) entries)))))))

(deftest deps-prep-agent-test
  ;; Test validates that deps prep works for agent project
  ;; Ensures no regression of the truss assertion error
         (testing "deps prep for agent project"
                  (let [agent-project-dir "projects/agent"
                        class-dir (fs/path "bases" "agent" "target" "classes")]
                       (try
                        (when (fs/exists? class-dir)
                          (fs/delete-tree class-dir))

                        (testing "runs without truss assertion error"
                                 (let [result (shell/sh "clojure" "-X:deps" "prep"
                                                         :dir agent-project-dir)]
                                      (is (zero? (:exit result))
                                          (str "deps prep failed with exit code " (:exit result)
                                               "\nstdout: " (:out result)
                                               "\nstderr: " (:err result)))
                                      (is (not (re-find #"Invariant failed.*:project.*params"
                                                        (:err result)))
                                          "Should not have truss assertion error about missing :project param")))

                        (testing "creates .class files"
                                 (is (fs/exists? (fs/path class-dir "criterium" "agent" "Agent.class"))
                                     "Agent.class should exist")
                                 (is (fs/exists? (fs/path class-dir "criterium" "agent" "Allocation.class"))
                                     "Allocation.class should exist"))

                        (finally
                         (when (fs/exists? class-dir)
                           (fs/delete-tree class-dir)))))))
