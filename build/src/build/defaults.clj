(ns build.defaults
  "Default configuration and project coordinates for build tasks.
  
  ## Project Coordinates Schema

  The `project-coordinates` map holds configuration for each artifact.
  Each project entry follows this schema:

      [:map
       [:lib symbol?]                        ; Maven coordinate symbol
       [:name string?]                       ; Project name string
       [:version string?]                    ; Version, optional template vars
       [:manifest {:optional true}           ; Optional JAR manifest attributes
        [:map-of string? string?]]]

  Example:

      {:criterium {:lib 'org.hugoduncan/criterium
                   :name \"org.hugoduncan/criterium\"
                   :version \"0.5.{{git-rev-count}}-ALPHA\"}
       :agent {:lib 'org.hugoduncan/criterium.agent
               :name \"org.hugoduncan/criterium.agent\"
               :version \"0.5.{{git-rev-count}}-ALPHA\"
               :manifest {\"Agent-Class\" \"criterium.agent\"}}}"
  (:require
   [babashka.fs :as fs]
   [malli.core :as m]
   #_[build.project-data :as project-data]))

(defn target-path
  "Return the target directory."
  [params]
  (:target params (str (fs/path (:dir params ".") "target"))))

(def project-coordinate-schema
  "Malli schema for a single project coordinate entry."
  [:map
   [:lib symbol?]
   [:name string?]
   [:version string?]
   [:manifest {:optional true}
    [:map-of string? string?]]
   [:pom-deps {:optional true}
    [:map-of symbol? [:map [:mvn/version string?]]]]
   [:validate-fn {:optional true}
    symbol?]])

(def project-coordinates
  "Project coordinates for all artifacts"
  {:criterium {:lib 'org.hugoduncan/criterium
               :name "org.hugoduncan/criterium"
               :version "0.5.{{git-rev-count}}-ALPHA"}
   :agent {:lib 'org.hugoduncan/criterium.agent
           :name "org.hugoduncan/criterium.agent"
           :version "0.5.{{git-rev-count}}-ALPHA"
           :manifest {"Agent-Class" "criterium.agent"}
           :validate-fn 'build.agent/validate-agent-binaries!}
   :blackhole {:lib 'org.hugoduncan/criterium.blackhole
               :name "org.hugoduncan/criterium.blackhole"
               :version "0.5.{{git-rev-count}}-ALPHA"}
   :arg-gen {:lib 'org.hugoduncan/criterium.arg-gen
             :name "org.hugoduncan/criterium.arg-gen"
             :version "0.5.{{git-rev-count}}-ALPHA"
             ;; Dependencies to include in the POM (replacing local/root deps)
             :pom-deps
             {'org.hugoduncan/criterium
              {:mvn/version "0.5.{{git-rev-count}}-ALPHA"}
              'org.clojure/test.check {:mvn/version "1.1.1"}}}})

(defn project-data
  "Return the params with default project coordinates.
   
   Expects :project key to be one of :criterium or :agent.
   
   Validates the coordinates against the project-coordinate-schema."
  [{:keys [project] :as params}]
  (let [coords (get project-coordinates project)]
    (when-not coords
      (throw
       (ex-info
        "Unknown project"
        {:project project :available (keys project-coordinates)})))
    (when-not (m/validate project-coordinate-schema coords)
      (throw (ex-info "Invalid project coordinates"
                      {:project project
                       :coords coords
                       :errors (m/explain project-coordinate-schema coords)})))
    (merge coords params)))
