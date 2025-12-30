(ns criterium.viewer.schema-validation
  "Schema validation utilities for Vega and Vega-Lite chart specs.

  Provides functions to validate generated chart specifications against
  official JSON schemas, with disk caching to avoid repeated downloads.

  Vega (v5) specs are validated using networknt/json-schema-validator.
  Vega-Lite specs are validated using vega-lite's compile function via Node.js,
  which handles the large schema more efficiently."
  (:require
   [clj-http.client :as http]
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str])
  (:import
   [com.fasterxml.jackson.databind JsonNode ObjectMapper]
   [com.networknt.schema JsonSchema JsonSchemaFactory SpecVersion$VersionFlag]))

(def ^:private cache-dir "target/schema-cache")

(def ^:private vega-v5-url
  "https://vega.github.io/schema/vega/v5.json")

(defn- url->cache-filename
  "Derive a cache filename from a schema URL."
  [url]
  (-> url
      (str/replace #"https?://" "")
      (str/replace #"/" "_")
      (str/replace #":" "_")))

(defn fetch-and-cache-schema
  "Fetch a JSON schema from URL, caching to disk.

  Returns the path to the cached schema file. If the schema is already
  cached, returns immediately without network access. On network error,
  uses cached version if available, otherwise throws."
  [url]
  (let [cache-file (io/file cache-dir (url->cache-filename url))]
    (if (.exists cache-file)
      (.getPath cache-file)
      (do
        (.mkdirs (io/file cache-dir))
        (try
          (let [response (http/get url {:as :string
                                        :socket-timeout 30000
                                        :connection-timeout 10000})]
            (spit cache-file (:body response))
            (.getPath cache-file))
          (catch Exception e
            (if (.exists cache-file)
              (.getPath cache-file)
              (throw (ex-info (str "Failed to fetch schema and no cache exists: " url)
                              {:url url :cause (.getMessage e)})))))))))

(def ^ObjectMapper object-mapper
  "Shared Jackson ObjectMapper instance."
  (ObjectMapper.))

(defn- load-schema
  "Load schema from URL (with caching) and create JsonSchema instance."
  ^JsonSchema [url]
  (let [path (fetch-and-cache-schema url)
        schema-content (slurp path)
        factory (JsonSchemaFactory/getInstance SpecVersion$VersionFlag/V7)
        schema-node (.readTree object-mapper ^String schema-content)]
    (.getSchema factory ^JsonNode schema-node)))

(def ^:private memoized-load-schema
  "Memoized schema loader to avoid re-parsing."
  (memoize load-schema))

(defn- validation-message->map
  "Convert a ValidationMessage to a Clojure map."
  [^com.networknt.schema.ValidationMessage msg]
  {:path (str (.getInstanceLocation msg))
   :schema-path (str (.getSchemaLocation msg))
   :message (.getMessage msg)
   :type (str (.getType msg))})

(defn- validate-with-networknt
  "Validate a spec against a schema URL using networknt.

  Returns {:valid? true} on success, or
  {:valid? false :errors [...]} on failure."
  [spec schema-url]
  (let [^JsonSchema schema (memoized-load-schema schema-url)
        ^JsonNode json-node (.valueToTree object-mapper spec)
        errors (.validate schema json-node)]
    (if (empty? errors)
      {:valid? true}
      {:valid? false
       :errors (mapv validation-message->map errors)})))

;;; Vega-Lite validation via Node.js

(def ^:private validator-script-path
  "Path to the Vega-Lite validation script."
  (let [src-path (io/resource "criterium/viewer/validate-vega-lite.mjs")]
    (if src-path
      (.getPath (io/file src-path))
      ;; Fall back to path relative to this source file for dev
      (let [this-ns-path (-> (ns-publics *ns*)
                             first val meta :file)
            base-dir (when this-ns-path
                       (-> (io/file this-ns-path)
                           (.getParentFile)))]
        (if (and base-dir (.exists (io/file base-dir "validate-vega-lite.mjs")))
          (.getPath (io/file base-dir "validate-vega-lite.mjs"))
          ;; Final fallback - use test directory path
          "bases/criterium/test/criterium/viewer/validate-vega-lite.mjs")))))

(defn- ensure-vega-lite-installed
  "Ensure vega-lite npm package is available. Installs if needed."
  []
  (let [result (shell/sh "npm" "list" "vega-lite" :dir ".")]
    (when-not (zero? (:exit result))
      (println "Installing vega-lite npm package...")
      (let [install-result (shell/sh "npm" "install" "--save-dev" "vega-lite" :dir ".")]
        (when-not (zero? (:exit install-result))
          (throw (ex-info "Failed to install vega-lite"
                          {:stderr (:err install-result)})))))))

(def ^:private vega-lite-installed?
  "Atom tracking whether vega-lite has been verified/installed."
  (atom false))

(defn- validate-with-vega-lite-compile
  "Validate a Vega-Lite spec using the vega-lite compiler via Node.js.

  Returns {:valid? true} on success, or
  {:valid? false :errors [...]} on failure."
  [spec]
  (when-not @vega-lite-installed?
    (ensure-vega-lite-installed)
    (reset! vega-lite-installed? true))
  (let [spec-json (json/write-str spec)
        result (shell/sh "node" validator-script-path
                         :in spec-json
                         :dir ".")]
    (if (zero? (:exit result))
      (let [output (json/read-str (:out result) :key-fn keyword)]
        (if (:valid output)
          (if (:warnings output)
            {:valid? true :warnings (:warnings output)}
            {:valid? true})
          {:valid? false :errors (:errors output)}))
      {:valid? false
       :errors [{:message (str "Node.js validation failed: " (:err result))
                 :type "process-error"}]})))

;;; Public API

(defn validate-vega-lite-spec
  "Validate a Vega-Lite spec using the vega-lite compiler.

  Returns {:valid? true} on success, or
  {:valid? false :errors [...]} on failure. May also include :warnings
  for valid specs with non-fatal issues."
  [spec]
  (validate-with-vega-lite-compile spec))

(defn validate-vega-spec
  "Validate a Vega spec against the v5 schema.

  Returns {:valid? true} on success, or
  {:valid? false :errors [...]} on failure."
  [spec]
  (validate-with-networknt spec vega-v5-url))
