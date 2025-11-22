(ns build.tasks
  (:require    [babashka.fs :as fs]))

(defn clean
  "Remove all built files."
  [params]
  ((requiring-resolve 'build.tasks.clean/clean)
   params))


(defn jar
  "Build a jar file."
  [params]
  (->
   {:task :jar}
   (merge params)
   ((requiring-resolve 'build.common/project-root))
   ((requiring-resolve 'build.tasks.jar/jar))))

(defn install
  "Install a jar file."
  [params]
  (fs/with-temp-dir [class-dir {:prefix "buildjar"}]
    (->
     {:task      :install
      :class-dir (str class-dir)}
     (merge params)
     ((requiring-resolve 'build.common/project-root))
     ((requiring-resolve 'build.project-data/project-data))
     ((requiring-resolve 'build.tasks.pom/pom))
     ((requiring-resolve 'build.tasks.jar/jar))
     ((requiring-resolve 'build.tasks.install/install)))))

(defn deploy
  "Deploy a jar file."
  [params]
  (fs/with-temp-dir [class-dir {:prefix "buildjar"}]
    (->
     {:task      :deploy
      :class-dir (str class-dir)}
     (merge params)
     ((requiring-resolve 'build.common/project-root))
     ((requiring-resolve 'build.project-data/project-data))
     ((requiring-resolve 'build.tasks.pom/pom))
     ((requiring-resolve 'build.tasks.jar/jar))
     ((requiring-resolve 'build.tasks.install/install))
     ((requiring-resolve 'build.tasks.deploy/deploy)))))
