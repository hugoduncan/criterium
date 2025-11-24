(ns build.tasks
  (:require [babashka.fs :as fs]
            [build.common :as common]))

(defn clean
      "Remove all built files."
      [params]
      ((requiring-resolve 'build.tasks.clean/clean)
       params))

(defn jar
      "Build a jar file."
      [params]
      (fs/with-temp-dir [class-dir {:prefix "buildjar"}]
                        (let [params (merge {:task :jar
                                             :class-dir (str class-dir)}
                                            params)
                              params (common/resolve-project-data params)]
                          ((requiring-resolve 'build.tasks.jar/jar) params))))

(defn install
      "Install a jar file."
      [params]
      (fs/with-temp-dir [class-dir {:prefix "buildjar"}]
                        (let [params (merge {:task :install
                                             :class-dir (str class-dir)}
                                            params)
                              params (common/resolve-project-data params)]
                          (-> params
                              ((requiring-resolve 'build.tasks.pom/pom))
                              ((requiring-resolve 'build.tasks.jar/jar))
                              ((requiring-resolve 'build.tasks.install/install))))))

(defn deploy
      "Deploy a jar file."
      [params]
      (fs/with-temp-dir [class-dir {:prefix "buildjar"}]
                        (let [params (merge {:task :deploy
                                             :class-dir (str class-dir)}
                                            params)
                              params (common/resolve-project-data params)]
                          (-> params
                              ((requiring-resolve 'build.tasks.pom/pom))
                              ((requiring-resolve 'build.tasks.jar/jar))
                              ((requiring-resolve 'build.tasks.install/install))
                              ((requiring-resolve 'build.tasks.deploy/deploy))))))

(defn javac
  "Compile Java sources.

  When called with :class-dir, uses that directory (for deps prep).
  Otherwise creates a temporary directory (for regular builds)."
  [params]
  (let [compile-fn (fn [params]
                     (let [params (assoc params :task :javac)
                           params (common/resolve-project-data params)]
                       ((requiring-resolve 'build.tasks.javac/javac) params)))]
    (if (:class-dir params)
      (compile-fn params)
      (fs/with-temp-dir [class-dir {:prefix "buildjavac"}]
        (compile-fn (assoc params :class-dir (str class-dir)))))))
