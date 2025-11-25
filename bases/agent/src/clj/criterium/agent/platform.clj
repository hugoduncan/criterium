(ns criterium.agent.platform
    "Platform detection for bundled native agent binaries.

  Provides functions to detect the current operating system and architecture,
  mapping them to canonical platform identifiers used for locating bundled
  native libraries.

  Supported platforms:
  - linux-x64: Linux on x86-64 architecture
  - macos-x64: macOS on x86-64 architecture
  - macos-arm64: macOS on ARM64 architecture (Apple Silicon)

  Returns nil for unsupported platforms to enable graceful degradation.")

(def ^:private os-name-mapping
     "Map Java os.name system property values to canonical OS names."
     {"Linux" "linux"
      "Mac OS X" "macos"})

(def ^:private os-arch-mapping
     "Map Java os.arch system property values to canonical architecture names."
     {"x86_64" "x64"
      "amd64" "x64"
      "aarch64" "arm64"})

(def ^:private extension-mapping
     "Map canonical OS names to native library file extensions."
     {"linux" "so"
      "macos" "dylib"})

(defn detect
      "Detect the current platform and return its canonical identifier.

  Returns a string like 'linux-x64' or 'macos-x64', or nil if the platform
  is not supported.

  The detection uses System properties:
  - os.name: Operating system name
  - os.arch: System architecture

  Example:
  ```clojure
  (detect)
  ;=> \"macos-x64\"
  ```"
      []
      (let [os-name (System/getProperty "os.name")
            os-arch (System/getProperty "os.arch")
            canonical-os (get os-name-mapping os-name)
            canonical-arch (get os-arch-mapping os-arch)]
           (when (and canonical-os canonical-arch)
                 (str canonical-os "-" canonical-arch))))

(defn extension
      "Return the native library file extension for the given platform.

  Parameters:
  - platform: A platform identifier string (e.g., 'linux-x64', 'macos-x64')

  Returns the file extension ('so' or 'dylib') or nil if the platform
  is not recognized.

  Example:
  ```clojure
  (extension \"macos-x64\")
  ;=> \"dylib\"
  ```"
      [platform]
      (when platform
            (let [os (first (clojure.string/split platform #"-"))]
                 (get extension-mapping os))))

(defn resource-path
      "Return the resource path for the agent binary on the given platform.

  Parameters:
  - platform: A platform identifier string (e.g., 'linux-x64', 'macos-x64')

  Returns a path string like 'native/macos-x64/libcriterium.dylib' or nil
  if the platform is not supported.

  Example:
  ```clojure
  (resource-path \"macos-x64\")
  ;=> \"native/macos-x64/libcriterium.dylib\"
  ```"
      [platform]
      (when-let [ext (extension platform)]
                (str "native/" platform "/libcriterium." ext)))
