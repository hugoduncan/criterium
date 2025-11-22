(ns build.defaults
  (:require
   [babashka.fs :as fs]
   #_[build.project-data :as project-data]))

(defn target-path
  "Return the target directory."
  [params]
  (:target params (str (fs/path (:dir params ".") "target"))))

(def group-id "criterium")

(defn project-data
  "Return the params with default project coordinates"
  [params]
  (merge
   {:group-id group-id}
   params))


#_(defn project-data
    "Return the params with project coordinates"
    [{:keys [name version] :as params}]
    (assert (or (and name version) (and (not name) (not version)))
            "Both :name and :version must be specified, or both unspecified.")
    (if name
      params
      (merge params (project-data/read params))))
