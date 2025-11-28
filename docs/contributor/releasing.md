# Releasing Criterium

This document describes the release process for Criterium.

## Branch Strategy

- `develop` - Main development branch where features are merged
- `master` - Release branch, should only contain release-ready code

## Release Workflow

Releases are automated via GitHub Actions. The workflow handles:
- Building native agents for all platforms (linux-x64, macos-x64, macos-arm64)
- Running tests and linting
- Building the JAR with bundled agents
- Smoke testing the JAR
- Generating changelog with git-cliff
- Creating git tag
- Deploying to Clojars
- Creating GitHub Release

### Prerequisites

Before releasing, ensure:

1. The `Release` environment is configured in GitHub repository settings with:
   - `CLOJARS_USERNAME` secret
   - `CLOJARS_PASSWORD` secret

2. The version in `build/src/build/version.clj` is set correctly

### Performing a Release

1. **Merge develop into master:**
   ```bash
   git checkout master
   git pull origin master
   git merge develop
   git push origin master
   ```

2. **Run dry-run release:**
   - Go to Actions → Release workflow
   - Click "Run workflow"
   - Select `master` branch
   - Keep `dry_run: true` (default)
   - Review the workflow summary for version, changelog, and smoke test results

3. **Perform actual release:**
   - If dry-run looks good, run workflow again with `dry_run: false`
   - The workflow will:
     - Create and push version tag (vX.Y.Z)
     - Deploy to Clojars
     - Create GitHub Release with changelog and JAR attachment

4. **Post-release:**
   - Update version in `build/src/build/version.clj` for next development cycle
   - Commit to develop: `chore: bump version to X.Y.Z-SNAPSHOT`

### Version Format

Tags follow semantic versioning: `vMAJOR.MINOR.PATCH`

Examples:
- `v0.5.0` - First 0.5.x release
- `v0.5.1` - Patch release

### Changelog

The changelog is generated automatically using [git-cliff](https://git-cliff.org/) with conventional commits:
- `feat:` - Features
- `fix:` - Bug Fixes
- `docs:` - Documentation
- `perf:` - Performance
- `refactor:` - Refactor
- `test:` - Testing
- Other commits go to Miscellaneous

## Troubleshooting

### Tag already exists
If the tag already exists, you need to either:
- Bump the version number, or
- Delete the existing tag (if it was created in error)

### Clojars deployment fails
Check that `CLOJARS_USERNAME` and `CLOJARS_PASSWORD` secrets are correctly configured in the `Release` environment.

### Agent build fails
Check the agent-cpp workflow for build errors. Each platform builds independently.
