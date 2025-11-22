(ns build.tasks.clean
  (:require
   [build.defaults :as defaults]
   [build.verbose :as verbose]
   [clojure.tools.build.api :as b]))

(defn clean
  "Remove the target directory."
  {:arglists '[[{:keys [target] :as params}]]}
  [params]
  (verbose/println params "Clean target...")
  (b/delete {:path (defaults/target-path params)})
  params)
