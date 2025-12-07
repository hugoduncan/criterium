(ns criterium.blackhole
  "Blackhole for preventing dead code elimination (DCE) in benchmarks.

  Two modes are supported:

  Compiler Blackhole (JVM 17+)
  Uses -XX:CompileCommand=blackhole,criterium.blackhole.Blackhole::consume
  to mark consume methods as compiler blackholes. Zero overhead.

  Runtime Blackhole (JVM < 17 or flag not present)
  Uses volatile fields and XOR-based impossible conditions. ~3ns overhead.

  Mode is detected at namespace load time. Use `mode` to check which is active."
  (:require
   [clojure.string :as str])
  (:import
   [criterium.blackhole Blackhole]
   [java.lang.management ManagementFactory]))

;;; JVM Version Detection

(defn- parse-major-version
  "Parse major version from java.version string.
  Handles formats: '17.0.1', '21', '1.8.0_292' (returns 8 for 1.x versions)."
  [version-str]
  (when version-str
    (let [parts (str/split version-str #"[.\-_]")]
      (if (= "1" (first parts))
        ;; Legacy format: 1.8.0_292 -> 8
        (some-> (second parts) parse-long)
        ;; Modern format: 17.0.1 -> 17
        (some-> (first parts) parse-long)))))

(def ^:private jvm-major-version
  "The major version of the running JVM."
  (parse-major-version (System/getProperty "java.version")))

(defn- compiler-blackhole-supported?
  "True if JVM version supports compiler blackholes (>= 17)."
  []
  (and jvm-major-version (>= (long jvm-major-version) 17)))

(defn- blackhole-flag-present?
  "Check if the CompileCommand blackhole flag is in JVM arguments."
  []
  (let [args (.getInputArguments (ManagementFactory/getRuntimeMXBean))
        pattern "CompileCommand=blackhole,criterium.blackhole.Blackhole::consume"]
    (boolean (some #(str/includes? % pattern) args))))

;;; Mode Detection

(def ^:private detected-mode
  "Blackhole mode detected at load time: :compiler or :runtime."
  (delay
    (cond
      (not (compiler-blackhole-supported?))
      :runtime

      (blackhole-flag-present?)
      :compiler

      :else
      (do
        (binding [*out* *err*]
          (println "WARNING: JVM 17+ detected but compiler blackhole flag not present.")
          (println "Add to JVM args: -XX:+UnlockExperimentalVMOptions -XX:CompileCommand=blackhole,criterium.blackhole.Blackhole::consume")
          (println "Falling back to runtime blackhole (~3ns overhead per consume)."))
        :runtime))))

;;; Singleton for Runtime Mode

(def ^:no-doc ^Blackhole runtime-instance
  "Singleton Blackhole instance for runtime mode.
  Public for macro expansion, not part of public API."
  (Blackhole.))

;;; Public API

(defn mode
  "Return the active blackhole mode: :compiler or :runtime."
  []
  @detected-mode)

(defn compiler-blackhole-available?
  "True if the JVM supports compiler blackholes (version >= 17)."
  []
  (compiler-blackhole-supported?))

(defn compiler-blackhole-enabled?
  "True if compiler blackhole mode is active."
  []
  (= :compiler @detected-mode))

(defmacro consume
  "Consume a value to prevent dead code elimination.

  In compiler mode (JVM 17+ with flag), expands to static method call with zero overhead.
  In runtime mode, expands to instance method call with ~3ns overhead.

  The value is evaluated exactly once."
  [x]
  (if (= :compiler @detected-mode)
    `(Blackhole/consume ~x)
    `(.consumeRuntime runtime-instance ~x)))

(defn evaporate
  "Clear any retained object references from the runtime blackhole.
  Call after measurement to prevent object retention.
  No-op in compiler mode."
  []
  (when (= :runtime @detected-mode)
    (.evaporate runtime-instance)))
