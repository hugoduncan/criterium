(ns build.version
    "Version string parsing, interpolation, and expansion utilities."
    (:require
     [clojure.edn :as edn]
     [clojure.string :as str]
     [clojure.tools.build.api :as b]))

;;; Version <-> Version Map

(defn- maybe-read
       [s]
       (try
        (edn/read-string s)
        (catch Exception _
               s)))

(defn version->version-map
      "Parse a version string into a map with :major, :minor, :incremental, and :qualifier keys.

  Example:
  (version->version-map \"1.2.3-ALPHA\")
  => {:major 1 :minor 2 :incremental 3 :qualifier \"ALPHA\"}"
      [version]
      {:pre [(string? version)
             (re-matches #"\d+\.\d+\.\d+(?:-\S+)?" version)]}
      (let [components (into [] (str/split version #"[.]"))
            [non-q q] (str/split (last components) #"-")
            components (into (pop components) [non-q q])]
           (zipmap
            [:major :minor :incremental :qualifier]
            (mapv maybe-read components))))

(defn version-map->version
      "Convert a version map back to a version string.

  Example:
  (version-map->version {:major 1 :minor 2 :incremental 3 :qualifier \"ALPHA\"})
  => \"1.2.3-ALPHA\""
      [version-map]
      {:pre [(map? version-map)]
       :post [string?]}
      (let [components (keep version-map [:major :minor :incremental])
            q (:qualifier version-map)]
           (cond-> (str/join "." components)
             q (str "-" q))))

;;; Version Interpolation

(defn- interpolate
       "Replace placeholders of form {{key}} in template string with values from map.

  Example:
  (interpolate \"version {{major}}.{{minor}}\" {:major 1 :minor 2})
  => \"version 1.2\""
       [template data]
       (reduce-kv
        (fn [s k v]
            (str/replace s (format "{{%s}}" (name k)) (str v)))
        template
        data))

(defn interpolate-version
      "Replace {{git-rev-count}} placeholder in version string with actual git revision count."
      [s]
      (interpolate
       s
       {:git-rev-count (b/git-count-revs nil)}))

;;; Version Expansion

(defmulti expand-component
          "Expand version components that require dynamic resolution."
          (fn [component] component))

(defmethod expand-component :default
           [component]
           component)

(defmethod expand-component :git-rev-count
           [_component]
           (edn/read-string (b/git-count-revs nil)))

(defn expand-version-map
      "Expand all dynamic components in a version map."
      [version-map]
      (reduce-kv
       (fn [vm k value]
           (assoc vm k (expand-component value)))
       {}
       version-map))

(defn expand-version
      "Expand version string in params map, resolving dynamic components like {{git-rev-count}}."
      [params]
      (let [version (-> (:version params)
                        interpolate-version
                        version->version-map
                        version-map->version)]
           (assoc params :version version)))
