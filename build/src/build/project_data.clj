(ns build.project-data
    "Project coordinate handling and expansion."
    (:require
     [build.defaults :as defaults]
     [build.verbose :as v]
     [build.version :as version]))

(defn project-data
      "Read and expand project coordinates from build.defaults."
      [params]
      (v/println "Reading project data")
      (-> params
          defaults/project-data
          version/expand-version))
