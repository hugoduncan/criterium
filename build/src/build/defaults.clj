(ns build.defaults
  "Default configuration and project coordinates for build tasks.
  
  ## Project Coordinates Schema
  
  The `project-coordinates` map contains configuration for each project artifact.
  Each project entry follows this schema:
  
      [:map
       [:lib symbol?]                        ; Maven coordinate symbol (e.g., criterium/criterium)
       [:name string?]                       ; Project name string
       [:version string?]                    ; Version string with optional template vars
       [:manifest {:optional true}           ; Optional JAR manifest attributes
        [:map-of string? string?]]]
  
  Example:
  
      {:criterium {:lib 'criterium/criterium
                   :name \"criterium/criterium\"
                   :version \"0.5.{{git-rev-count}}-ALPHA\"}
       :agent {:lib 'criterium/criterium.agent
               :name \"criterium/criterium.agent\"
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
  {:criterium {:lib 'criterium/criterium
               :name "criterium/criterium"
               :version "0.5.{{git-rev-count}}-ALPHA"}
   :agent {:lib 'criterium/criterium.agent
           :name "criterium/criterium.agent"
           :version "0.5.{{git-rev-count}}-ALPHA"
           :manifest {"Agent-Class" "criterium.agent"}
           :validate-fn 'build.agent/validate-agent-binaries!}
   :blackhole {:lib 'criterium/criterium.blackhole
               :name "criterium/criterium.blackhole"
               :version "0.5.{{git-rev-count}}-ALPHA"}
   :arg-gen {:lib 'criterium/criterium.arg-gen
             :name "criterium/criterium.arg-gen"
             :version "0.5.{{git-rev-count}}-ALPHA"
             ;; Dependencies to include in the POM (replacing local/root deps)
             :pom-deps {'criterium/criterium {:mvn/version "0.5.{{git-rev-count}}-ALPHA"}
                        'org.clojure/test.check {:mvn/version "1.1.1"}}}})

(defn project-data
  "Return the params with default project coordinates.
   
   Expects :project key to be one of :criterium or :agent.
   
   Validates the coordinates against the project-coordinate-schema."
  [{:keys [project] :as params}]
  (let [coords (get project-coordinates project)]
    (when-not coords
      (throw (ex-info "Unknown project" {:project project
                                         :available (keys project-coordinates)})))
    (when-not (m/validate project-coordinate-schema coords)
      (throw (ex-info "Invalid project coordinates"
                      {:project project
                       :coords coords
                       :errors (m/explain project-coordinate-schema coords)})))
    (merge coords params)))
