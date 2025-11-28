# Criterium Agent

The Criterium agent is a native JVM agent that provides enhanced allocation tracking capabilities for more accurate benchmarking.

## Usage

Starting with version 0.5.x, the agent is **bundled in the JAR** with pre-compiled binaries for supported platforms.

### Loading the Agent

**IMPORTANT**: Due to JVMTI limitations, the agent **must be loaded at JVM startup** using `-agentpath` for allocation tracking to work. Programmatic loading via `load-agent!` will fail because the required `can_generate_sampled_object_alloc_events` capability can only be requested during VM initialization.

#### Recommended: JVM Startup Loading

```bash
# Start REPL with agent
clojure -J-agentpath:/path/to/libcriterium.dylib -M:dev

# Or use agent/jvm-opts to get the path
clojure -Sdeps '{:deps {criterium/criterium {:mvn/version "0.5.x"}}}' \
  -e '(require '"'"'[criterium.agent :as agent]) (println (first (agent/jvm-opts)))'
# Then restart with that path
```

#### Alternative: Let Criterium Configure JVM Options

For development, you can let Criterium handle the agent path:

```clojure
(require '[criterium.agent :as agent])

;; Get JVM arguments for restarting with agent
(agent/jvm-opts)
;; => ["-agentpath:/tmp/criterium-agent-macos-x64-abc123.dylib"]

;; Check if agent is loaded
(agent/loaded?)
;; => true (if started with -agentpath) or false
```

**Note**: Programmatic loading via `load-agent!` is not supported for allocation tracking. The agent must be loaded at JVM startup.

### Spawning Subprocesses

If you need to spawn a subprocess with the agent enabled, use `jvm-opts`:

```clojure
(require '[criterium.agent :as agent])

;; Get JVM arguments for subprocess
(agent/jvm-opts)
;; => ["-agentpath:/tmp/criterium-agent-linux-x64-abc123.so"] or []

;; Example: spawning a Clojure subprocess
(require '[clojure.java.shell :as shell])
(let [opts (agent/jvm-opts)]
  (apply shell/sh "clojure" (concat opts ["-e" "(+ 1 2)"])))
```

Returns an empty vector `[]` if the agent is unavailable or the platform is unsupported.

## Platform Support

### Supported Platforms

The following platforms include bundled agent binaries:

- **linux-x64** - Linux on x86-64 (Intel/AMD 64-bit)
- **macos-x64** - macOS on x86-64 (Intel Mac)
- **macos-arm64** - macOS on ARM64 (Apple Silicon M1/M2/M3)

### Future Platform Support

Additional platforms may be added in future releases:

- **linux-aarch64** - Linux on ARM64

Platform support depends on CI infrastructure availability for building native binaries.

## Graceful Degradation

When the agent is unavailable (unsupported platform or missing binary), Criterium continues to work but with reduced capabilities:

- Basic timing measurements continue to work normally
- Allocation tracking returns empty results
- A warning is logged on first attempt to use agent features
- `(agent/loaded?)` returns `false`
- `(agent/jvm-opts)` returns `[]`

**Example behavior on unsupported platforms:**

```clojure
;; On unsupported platform
(agent/loaded?)
;; => false

(agent/agent-path)
;; => nil

;; Benchmarks still work, but without allocation tracking
(bench/bench (reduce + (range 1000)))
;; => Results show timing but no allocation data
```

This ensures your benchmarking code remains portable across all platforms.

## Manual Agent Path (Advanced)

For custom builds or development, you can still use the traditional `-agentpath` JVM argument:

```bash
# Start REPL with custom-built agent
clojure -J-agentpath:/path/to/custom/libcriterium.so -M:dev
```

The agent APIs (`loaded?`, `attached?`) recognize both bundled and manually-loaded agents.

## Development Workflow

### Building the Agent Locally

For active development of the native agent:

```bash
# From the agent-cpp/ directory
make

# Build with debug symbols
make DEBUG=1
```

This produces `agent-cpp/libcriterium.dylib` (macOS) or `agent-cpp/libcriterium.so` (Linux).

### Using Local Builds

Development aliases are provided to use locally-built agents:

```bash
# macOS
clojure -M:dev:with-agent-mac

# Linux  
clojure -M:dev:with-agent-linux

# Run tests with local agent
clojure -M:kaocha:dev:test:with-agent-mac --reporter dots
```

These aliases configure `-agentpath` to point to your local build, bypassing the bundled binary.

## Contributor Guide

### Updating Bundled Binaries

When making changes to the native agent that should be released:

1. **Wait for CI to build** - The GitHub Actions workflow builds binaries for all supported platforms
2. **Download artifacts** - See [docs/contributor/building-agent.md](../../docs/contributor/building-agent.md) for detailed instructions
3. **Place in resources** - Copy to `projects/agent/resources/native/{platform}/`
4. **Update SHA256 hashes** - Generate `.sha256` files for version tracking
5. **Commit and release** - Include binaries in the release commit

Full process documentation: [Building the Agent](../../docs/contributor/building-agent.md)

### Testing

```bash
# Run all tests
clojure -M:kaocha:dev:test --reporter dots

# Run integration tests (requires agent binary)
clojure -M:kaocha:dev:test --focus :requires-agent
```

Tests gracefully skip when agent binaries are unavailable.

## Technical Details

### Resource Layout

Agent binaries are stored in the JAR at:

```
resources/native/linux-x64/libcriterium.so
resources/native/macos-x64/libcriterium.dylib
resources/native/macos-arm64/libcriterium.dylib
```

SHA256 hash files track binary versions:

```
resources/native/linux-x64/libcriterium.so.sha256
resources/native/macos-x64/libcriterium.dylib.sha256
resources/native/macos-arm64/libcriterium.dylib.sha256
```

### Extraction Strategy

On first use, the agent:

1. Detects the current platform (OS + architecture)
2. Locates the matching binary in JAR resources
3. Extracts to temp directory with versioned filename: `criterium-agent-{platform}-{hash}.{ext}`
4. Uses file locking to prevent concurrent extraction races
5. Registers shutdown hook for cleanup

Extracted agents are reused across JVM restarts if the hash matches.

### API Reference

**`criterium.agent` namespace:**

- `(loaded?)` - Returns true if agent is currently loaded via `-agentpath` JVM argument
- `(agent-path)` - Returns absolute path to extracted agent binary, or nil if unavailable
- `(jvm-opts)` - Returns vector of JVM arguments for spawning subprocesses with agent (e.g., `["-agentpath:/tmp/..."]`), or empty vector if unavailable

**Deprecated:**

- `(load-agent!)` - Programmatic loading does not work for allocation tracking due to JVMTI limitations. The `can_generate_sampled_object_alloc_events` capability must be requested at VM startup. Use `-agentpath` at JVM startup instead.

**Low-level API (`criterium.agent.core`):**

- `(attached?)` - Checks if agent was loaded via `-agentpath` JVM argument
- `(pid)` - Returns current JVM process ID

## References

- [Building the Agent](../../docs/contributor/building-agent.md) - Contributor guide for building and updating binaries
- [Root CLAUDE.md](../../.claude/CLAUDE.md) - Development commands and testing workflows
- [jattach](https://github.com/apangin/jattach) - Alternative tool for attaching agents to running JVMs
