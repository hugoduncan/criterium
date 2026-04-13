(ns build.tasks.pom-test
  (:require
   [babashka.fs :as fs]
   [build.tasks.pom :as pom]
   [clojure.test :refer [deftest is testing]]
   [clojure.tools.build.api :as b]
   [clojure.xml :as xml]))

(defn- parse-pom-xml
  "Parse POM XML file and return parsed structure."
  [pom-path]
  (with-open [in (java.io.FileInputStream. (fs/file pom-path))]
    (xml/parse in)))

(defn- find-element
  "Find first element with given tag in XML structure."
  [xml-node tag]
  (some #(when (= (:tag %) tag) %)
        (:content xml-node)))

(defn- element-text
  "Get text content of XML element."
  [element]
  (when element
    (first (:content element))))

(defn- verify-pom-metadata
  "Verify that POM contains all required Maven Central/Clojars metadata."
  [pom-xml _project-name]
  (testing "has groupId"
    (is (= "org.hugoduncan" (element-text (find-element pom-xml :groupId)))))

  (testing "has artifactId"
    (let [artifact-id (element-text (find-element pom-xml :artifactId))]
      (is (contains? #{"criterium" "criterium.agent"} artifact-id))))

  (testing "has version"
    (let [version (element-text (find-element pom-xml :version))]
      (is (some? version))
      (is (re-matches #"\d+\.\d+\.\d+-ALPHA" version))))

  (testing "has description"
    (is (some? (element-text (find-element pom-xml :description)))))

  (testing "has url"
    (is (= "https://github.com/hugoduncan/criterium"
           (element-text (find-element pom-xml :url)))))

  (testing "has licenses"
    (let [licenses (find-element pom-xml :licenses)
          license (find-element licenses :license)]
      (is (some? license))
      (is (= "Eclipse Public License"
             (element-text (find-element license :name))))
      (is (= "http://www.eclipse.org/legal/epl-v10.html"
             (element-text (find-element license :url))))))

  (testing "has SCM information"
    (let [scm (find-element pom-xml :scm)]
      (is (some? scm))
      (is (= "https://github.com/hugoduncan/criterium"
             (element-text (find-element scm :url))))
      (is (= "scm:git:git://github.com/hugoduncan/criterium.git"
             (element-text (find-element scm :connection))))
      (is (= "scm:git:ssh://git@github.com/hugoduncan/criterium.git"
             (element-text (find-element scm :developerConnection))))))

  (testing "has developers"
    (let [developers (find-element pom-xml :developers)
          developer (find-element developers :developer)]
      (is (some? developer))
      (is (= "Hugo Duncan"
             (element-text (find-element developer :name)))))))

(deftest write-pom-criterium-test
  (testing "write-pom"
    (testing "generates valid POM for criterium project"
      (let [temp-dir (fs/create-temp-dir {:prefix "pom-test"})
            class-dir (str temp-dir)
            opts {:project :criterium
                  :class-dir class-dir
                  :name "org.hugoduncan/criterium"
                  :version "0.5.246-ALPHA"
                  :basis (b/create-basis)}
            result (pom/write-pom opts)
            pom-path (:pom-file result)]
        (try
          (testing "returns pom-file path"
            (is (some? pom-path))
            (is (fs/exists? pom-path)))

          (testing "POM is at correct location"
            (is
             (=
              (str
               (fs/path
                class-dir
                "META-INF"
                "maven"
                "org.hugoduncan"
                "criterium"
                "pom.xml"))
              pom-path)))

          (let [pom-xml (parse-pom-xml pom-path)]
            (verify-pom-metadata pom-xml :criterium))
          (finally
            (fs/delete-tree temp-dir)))))))

(deftest write-pom-agent-test
  (testing "write-pom"
    (testing "generates valid POM for agent project"
      (let [temp-dir (fs/create-temp-dir {:prefix "pom-test"})
            class-dir (str temp-dir)
            opts {:project :agent
                  :class-dir class-dir
                  :name "org.hugoduncan/criterium.agent"
                  :version "0.5.246-ALPHA"
                  :basis (b/create-basis)}
            result (pom/write-pom opts)
            pom-path (:pom-file result)]
        (try
          (testing "returns pom-file path"
            (is (some? pom-path))
            (is (fs/exists? pom-path)))

          (testing "POM is at correct location"
            (is
             (=
              (str
               (fs/path
                class-dir
                "META-INF"
                "maven"
                "org.hugoduncan"
                "criterium.agent"
                "pom.xml"))
              pom-path)))

          (let [pom-xml (parse-pom-xml pom-path)]
            (verify-pom-metadata pom-xml :agent))
          (finally
            (fs/delete-tree temp-dir)))))))

(deftest pom-wrapper-test
  (testing "pom"
    (testing "wraps write-pom correctly"
      (let [temp-dir (fs/create-temp-dir {:prefix "pom-test"})
            class-dir (str temp-dir)
            opts {:project :criterium
                  :class-dir class-dir
                  :name "org.hugoduncan/criterium"
                  :version "0.5.246-ALPHA"
                  :basis (b/create-basis)}
            result (pom/pom opts)]
        (try
          (is (some? (:pom-file result)))
          (is (fs/exists? (:pom-file result)))
          (finally
            (fs/delete-tree temp-dir)))))))
