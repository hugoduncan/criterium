# Building and Updating the C++ Agent

This document describes how to build, download, and update the bundled C++ agent binaries in Criterium.

## Overview

Criterium bundles pre-compiled native agent binaries in the JAR to enable zero-configuration usage. The agent provides allocation tracking capabilities through a JNI interface.

**IMPORTANT:** Agent binaries are NOT committed to version control. They are in `.gitignore` and must be downloaded from CI artifacts before building release JARs. This keeps the repository clean while still allowing binaries to be bundled in distributed artifacts.

**Supported Platforms:**
- `linux-x64` - 64-bit Linux (x86_64)
- `macos-x64` - 64-bit macOS (Intel)
- `macos-arm64` - macOS on Apple Silicon (M1/M2/M3)

**Future Platforms (Out of Scope):**
- `linux-aarch64` - Deferred until CI infrastructure ready

## CI Build Process

The GitHub Actions workflow `.github/workflows/agent-cpp.yml` automatically builds agent binaries for all supported platforms whenever code is pushed to the repository.

**Trigger Options:**
- Automatic: Pushes to any branch
- Automatic: Pull requests
- Manual: `workflow_dispatch` with optional DEBUG build flag

**Build Matrix:**
- `ubuntu-latest` → `agent-cpp-linux-x64`
- `macos-latest` → `agent-cpp-macos-x64` and `agent-cpp-macos-arm64`

The macOS job cross-compiles for both x86_64 and ARM64 architectures.

Each build produces an artifact containing the platform-specific shared library and its SHA256 hash:
- Linux: `libcriterium.so` and `libcriterium.so.sha256`
- macOS: `libcriterium.dylib` and `libcriterium.dylib.sha256` (for each architecture)

The SHA256 hash files are used by the runtime extraction mechanism to generate unique filenames and verify binary integrity.

## Downloading CI Artifacts

To download agent binaries from a CI run:

### 1. Find the Workflow Run

Navigate to the Actions tab in GitHub or use the CLI:

```bash
# List recent workflow runs
gh run list --workflow="agent-cpp.yml" --limit 5

# View details of a specific run
gh run view <run-id>
```

### 2. Download Platform-Specific Artifacts

```bash
# Download Linux artifact
gh run download <run-id> -n agent-cpp-linux-x64

# Download macOS artifacts (both architectures)
gh run download <run-id> -n agent-cpp-macos-x64
gh run download <run-id> -n agent-cpp-macos-arm64
```

This creates directories named `agent-cpp-linux-x64/`, `agent-cpp-macos-x64/`, and `agent-cpp-macos-arm64/` containing the respective binaries.

### 3. Place in Resources Directory

Move the downloaded binaries and hash files to the appropriate resource paths:

```bash
# Create resource directories if they don't exist
mkdir -p bases/agent/resources/native/linux-x64
mkdir -p bases/agent/resources/native/macos-x64
mkdir -p bases/agent/resources/native/macos-arm64

# Copy binaries and hash files to resource paths
cp agent-cpp-linux-x64/libcriterium.* bases/agent/resources/native/linux-x64/
cp agent-cpp-macos-x64/libcriterium.* bases/agent/resources/native/macos-x64/
cp agent-cpp-macos-arm64/libcriterium.* bases/agent/resources/native/macos-arm64/
```

**Resource Path Convention:**
```
bases/agent/resources/native/{platform}/libcriterium.{ext}
bases/agent/resources/native/{platform}/libcriterium.{ext}.sha256
```

Where:
- `{platform}` is `linux-x64`, `macos-x64`, or `macos-arm64`
- `{ext}` is `so` (Linux) or `dylib` (macOS)

## Testing Locally

After placing the binaries, test them locally:

```bash
# Run tests with the bundled agent
clojure -M:kaocha:dev:test --reporter dots

# Test specific agent functionality
clojure -M:dev
(require 'criterium.agent)
(criterium.agent/loaded?)  ; Should return true
```

## Release Process Checklist

Use this checklist when updating agent binaries for a release:

- [ ] **Trigger CI Build:**
  - Push agent-cpp changes to trigger automatic build, or
  - Manually trigger workflow via Actions tab (use DEBUG=false for release)

- [ ] **Verify Build Success:**
  - Check that both platform builds completed successfully
  - Review build logs for warnings or errors

- [ ] **Download Artifacts:**
  - Note the workflow run ID
  - Download `agent-cpp-linux-x64`, `agent-cpp-macos-x64`, and `agent-cpp-macos-arm64` artifacts

- [ ] **Place in Resources (Not Committed to Git):**
  - Copy binaries to `bases/agent/resources/native/{platform}/`
  - Verify file permissions (should be readable)
  - **IMPORTANT:** Binaries are in `.gitignore` and should NOT be committed to version control
  - Binaries must be downloaded fresh from CI before building release JARs

- [ ] **Test Locally:**
  - Run full test suite on your platform
  - Verify agent loads correctly
  - Check allocation tracking works as expected
  - Verify `git status` shows no binary files

- [ ] **Build Release JAR:**
  - Run `clojure -M:build jar` to build JAR with bundled binaries
  - Build script will validate that binaries are present
  - Verify binaries are included in the JAR using `jar tf` command

- [ ] **Tag Release:**
  - Create release tag if part of version bump
  - Push tag to trigger release workflow

- [ ] **Document Changes:**
  - Update CHANGELOG with agent changes if applicable
  - Note any platform-specific behavior changes

## Manual Local Build

For development with local agent modifications, build manually instead of using bundled binaries:

```bash
# Build agent locally (from agent-cpp/ directory)
cd agent-cpp
cmake -B build
cmake --build build

# Build with debug symbols
cmake -B build -DCMAKE_BUILD_TYPE=Debug
cmake --build build

# Use :with-agent-* aliases to load local build
clojure -M:dev:with-agent-mac      # macOS
clojure -M:dev:with-agent-linux    # Linux
```

The `:with-agent-*` aliases configure JVM to load the agent from the local `agent-cpp/` build directory rather than using bundled binaries.

## Future Automation

The current process is manual to reduce initial complexity. Future improvements may include:

- Automatic binary updates via CI bot on release branches
- Verification scripts to ensure binary compatibility
- Cross-compilation support for ARM64 platforms
- Automated smoke tests in CI for extracted binaries

## Architecture Notes

**Cross-Compilation:**
The macOS build uses cross-compilation to produce both x86_64 and ARM64 binaries from a single runner. CMake uses the `CMAKE_OSX_ARCHITECTURES` variable to specify the target architecture:

```bash
# Build for specific architecture
cmake -B build -DCMAKE_OSX_ARCHITECTURES=x86_64   # Intel Macs
cmake --build build

cmake -B build -DCMAKE_OSX_ARCHITECTURES=arm64    # Apple Silicon
cmake --build build
```

**Binary Size:**
Typical agent binary is ~200KB per platform. Total JAR size increase is acceptable for zero-configuration convenience.

**Version-Specific Paths:**
Runtime extraction uses version-specific temporary paths to prevent collisions when multiple JAR versions are on the classpath.

## Troubleshooting

**Artifact not found:**
- Ensure the workflow run completed successfully
- Check that artifact retention period hasn't expired (default 90 days)
- Verify you have read access to the repository

**Binary won't load:**
- Check file permissions (must be readable/executable)
- Verify correct platform directory structure
- Review agent logs for detailed error messages

**Test failures after update:**
- Ensure binaries match the current agent-cpp source code
- Check that JNI interface signatures match between Java and C++ code
- Run with DEBUG=1 build for more diagnostic information
