(ns criterium.jvm
  "JVM data accessors"
  (:require [criterium.types :refer :all])
  (:import [java.lang.management ManagementFactory]))


(defmacro timestamp
  "Obtain a timestamp using System/nanoTime."
  [] `(System/nanoTime))


(let [bean (.. ManagementFactory getClassLoadingMXBean)]
  (defn class-loader-counts
    "Return a JvmClassLoaderState record with loaded class counts.

    Contains :loaded-count and :unloaded-count fields.

    Satisfies the StateChanged protocol, providing the state-changed?
    and state-delta methods.

    These are counts since the start of the JVM."
    []
    (->JvmClassLoaderState (. bean getLoadedClassCount)
                           (. bean getUnloadedClassCount))))


(let [bean (.. ManagementFactory getCompilationMXBean)]
  (defn compilation-time
    "Returns the total compilation time for the JVM instance.

    Returns -1 if not supported."
    []
    (->JvmCompilationState (if (. bean isCompilationTimeMonitoringSupported)
                             (. bean getTotalCompilationTime)
                             -1))))


(defn jit-name
  "Returns the name of the JIT compiler."
  []
  (let [bean (.. ManagementFactory getCompilationMXBean)]
    (. bean getName)))


(defn os-details
  "Return the operating system details as a hash."
  []
  (let [bean (.. ManagementFactory getOperatingSystemMXBean)]
    {:arch (. bean getArch)
     :available-processors (. bean getAvailableProcessors)
     :name (. bean getName)
     :version (. bean getVersion)}))


(defn runtime-details
  "Return the runtime details as a hash."
  []
  (let [bean (.. ManagementFactory getRuntimeMXBean)
        props (. bean getSystemProperties)]
    {:input-arguments (. bean getInputArguments)
     :name (. bean getName)
     :spec-name (. bean getSpecName)
     :spec-vendor (. bean getSpecVendor)
     :spec-version (. bean getSpecVersion)
     :vm-name (. bean getVmName)
     :vm-vendor (. bean getVmVendor)
     :vm-version (. bean getVmVersion)
     :java-version (get props "java.version")
     :java-runtime-version (get props "java.runtime.version")
     :sun-arch-data-model (get props "sun.arch.data.model")
     :clojure-version-string (clojure-version)
     :clojure-version *clojure-version*}))


(defn system-properties
  "Return the operating system details."
  []
  (let [bean (.. ManagementFactory getRuntimeMXBean)]
    (. bean getSystemProperties)))
