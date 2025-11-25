# Building and Updating the C++ Agent

This document describes how to build, download, and update the bundled C++ agent binaries in Criterium.

## Overview

Criterium bundles pre-compiled native agent binaries in the JAR to enable zero-configuration usage. The agent provides allocation tracking capabilities through a JNI interface.

**Supported Platforms (Phase 1):**
- `linux-x64` - 64-bit Linux
- `macos-x64` - 64-bit macOS

**Future Platforms (Out of Scope):**
- `linux-aarch64`, `macos-aarch64` - Deferred until CI infrastructure ready

## CI Build Process

The GitHub Actions workflow `.github/workflows/agent-cpp.yml` automatically builds agent binaries for all supported platforms whenever code is pushed to the repository.

**Trigger Options:**
- Automatic: Pushes to any branch
- Automatic: Pull requests
- Manual: `workflow_dispatch` with optional DEBUG build flag

**Build Matrix:**
- `ubuntu-latest` → `agent-cpp-linux-x64`
- `macOS-latest` → `agent-cpp-macos-x64`

Each build produces an artifact containing the platform-specific shared library and its SHA256 hash:
- Linux: `libcriterium.so` and `libcriterium.so.sha256`
- macOS: `libcriterium.dylib` and `libcriterium.dylib.sha256`

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

# Download macOS artifact
gh run download <run-id> -n agent-cpp-macos-x64
```

This creates directories named `agent-cpp-linux-x64/` and `agent-cpp-macos-x64/` containing the respective binaries.

### 3. Place in Resources Directory

Move the downloaded binaries and hash files to the appropriate resource paths:

```bash
# Create resource directories if they don't exist
mkdir -p projects/agent/resources/native/linux-x64
mkdir -p projects/agent/resources/native/macos-x64

# Copy binaries and hash files to resource paths
cp agent-cpp-linux-x64/libcriterium.so projects/agent/resources/native/linux-x64/
cp agent-cpp-linux-x64/libcriterium.so.sha256 projects/agent/resources/native/linux-x64/
cp agent-cpp-macos-x64/libcriterium.dylib projects/agent/resources/native/macos-x64/
cp agent-cpp-macos-x64/libcriterium.dylib.sha256 projects/agent/resources/native/macos-x64/
```

**Resource Path Convention:**
```
projects/agent/resources/native/{platform}/libcriterium.{ext}
projects/agent/resources/native/{platform}/libcriterium.{ext}.sha256
```

Where:
- `{platform}` is `linux-x64` or `macos-x64`
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
  - Download both `agent-cpp-linux-x64` and `agent-cpp-macos-x64` artifacts

- [ ] **Place in Resources:**
  - Copy binaries to `projects/agent/resources/native/{platform}/`
  - Verify file permissions (should be readable)

- [ ] **Test Locally:**
  - Run full test suite on your platform
  - Verify agent loads correctly
  - Check allocation tracking works as expected

- [ ] **Commit Binaries:**
  - Add binaries to git: `git add projects/agent/resources/`
  - Commit with message: `chore: update bundled agent binaries for v<version>`

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
make

# Build with debug symbols
make DEBUG=1

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

**Why x64 Only Initially:**
ARM64 support requires either cross-compilation or native ARM runners. Starting with x64 reduces scope while covering the majority of development and production environments.

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
