# OSRM Binaries Release Handoff

## Final Goal

Rebuild the `e-kotov/osrm-binaries` releases chronologically with
immutable GitHub releases enabled, using the exact upstream OSRM tags as
release tags. The key purpose is to make every published binary release
reliable for the R package and to prevent broken archives from being
published.

The broader package goal is to move `osrm.backend` onto a
self-controlled binary supply chain:

- Build OSRM backend binaries in `e-kotov/osrm-binaries`.
- Publish one immutable GitHub release per upstream OSRM tag.
- Include archives for Linux x64, Linux arm64, macOS x64, macOS arm64,
  and Windows x64.
- Include `checksums.txt` in every release.
- Have the R package install those binaries by default through
  [`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md).
- Keep upstream `Project-OSRM/osrm-backend` binaries available only as
  an explicit alternate provider.
- Validate the full install-and-run path in R package CI, not only the
  binary build workflow.

The intended release sequence is:

``` text
v5.27.1
v6.0.0
v26.4.0
v26.4.1
v26.5.0
v26.6.0
v26.6.1
v26.6.2
v26.6.3
v26.6.4
v26.6.5
v26.7.0
v26.7.1
v26.7.2
```

## What We Changed

We hardened `.github/workflows/build.yml` in the `osrm-binaries`
repository so publishing is gated behind successful builds and runtime
sanity checks.

The workflow now follows:

``` text
build -> test -> release
```

Important safeguards added:

- `release` depends on both `build` and `test`.
- Every platform artifact is downloaded and tested before release.
- Basic executable checks run for `osrm-extract --version` and
  `osrm-routed --version`.
- The release job validates the expected artifact set and publishes
  `checksums.txt`.
- `softprops/action-gh-release` uses `fail_on_unmatched_files: true`.
- The workflow validates `version_tag` format.
- macOS packaging now bundles and relinks transitive Homebrew `.dylib`
  dependencies.
- Windows packaging keeps the broad vcpkg `boost` package because old
  OSRM tags, especially `v5.27.1`, need many header-only Boost modules.

## R Package Context

The current folder is the `osrm.backend` R package. Its installation
path already expects the custom binary repository to be the default.

Key package files:

- `DESCRIPTION`: declares
  `SystemRequirements: OSRM backend binaries (>= v5.27.0)` and says
  binaries are downloaded automatically if not found.
- `R/osrm_install.R`: implements
  [`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md),
  platform detection, release lookup, binary download, checksum
  verification, extraction, profile installation, and optional `PATH`
  setup.
- `R/osrm_which.R`: resolves the active `osrm-routed` executable and
  verifies it by running `osrm-routed --version`.
- `tests/testthat/test-osrm_install.R`: covers platform detection,
  override options, asset matching, version comparison, release
  filtering, install behavior, and error cases.
- `tests/testthat/test-live-integration.R`: performs the expensive
  end-to-end validation against real GitHub releases.
- `.github/workflows/osrm-live-tests.yml`: runs the live integration
  workflow on a schedule and on manual dispatch.

[`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)
currently supports two providers:

- `"default"`: `e-kotov/osrm-binaries`, the custom binary release
  repository.
- `"official"`: `Project-OSRM/osrm-backend`, the upstream OSRM release
  repository.

For the custom repository,
[`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)
constructs the expected asset URL directly:

``` text
https://github.com/e-kotov/osrm-binaries/releases/download/<tag>/osrm-<tag>-<os>-<arch>-Release.tar.gz
```

This means the binary repository release tags and asset names are part
of the package API. Tags such as `v5.27.1` must match the upstream OSRM
tag exactly because users pass the same version string to
`osrm_install(version = ...)`.

The package also attempts SHA-256 validation for custom binaries by
downloading `checksums.txt` from the matching release. That is why each
rebuilt release must include a correct `checksums.txt` file.

## R Package Tests and CI Contract

The normal R package checks run through
`.github/workflows/R-CMD-check.yaml` on:

- macOS release
- Windows release
- Ubuntu release/devel/oldrel variants
- Ubuntu ARM release

Those checks exercise the ordinary testthat suite but do not run the
long live OSRM integration unless `RUN_OSRM_LIVE_TESTS=true`.

The live OSRM workflow is `.github/workflows/osrm-live-tests.yml`. It
runs weekly and can be triggered manually. Its platform matrix is:

- `macos-latest`
- `windows-latest`
- `ubuntu-latest`
- `ubuntu-24.04-arm`

The live test does the important real-world validation:

1.  Calls `osrm_check_available_versions(prereleases = TRUE)`.
2.  Selects the latest `v5.x` release plus all releases `>= v6.0.0`.
3.  Installs each selected version with
    [`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md).
4.  Verifies `osrm-routed --version` reports the expected OSRM version.
5.  Runs
    [`osrm_prepare_graph()`](https://www.ekotov.pro/osrm.backend/reference/osrm_prepare_graph.md)
    on the bundled `extdata/cur.osm.pbf`.
6.  Starts a local `osrm-routed` server.
7.  Calls the OSRM HTTP route API and expects a valid route.
8.  If the `osrm` R package is available, calls
    [`osrm::osrmRoute()`](https://rdrr.io/pkg/osrm/man/osrmRoute.html)
    against the local server.
9.  Stops the server and records pass/fail status.

The live workflow also initializes and updates `test_results.rds`, then
generates per-OS badge JSON files with `tools/init_test_results.R` and
`tools/generate_badge.R`. The badge deploy step pushes those JSON files
to the `badges` branch.

This creates a two-layer reliability model:

- `osrm-binaries` CI proves each archive starts on its native platform
  before publishing.
- `osrm.backend` live CI proves the published archives can be installed,
  used to prepare graph data, start a routing server, and serve real
  route requests from R.

## What Happened During Testing

We first proved the hardened workflow on `v26.7.2`. After fixing macOS
transitive dylib packaging, the full build, test, and release path
passed.

Then all old releases and remote tags were deleted. Immutable releases
were enabled in GitHub after deletion.

We recreated `v5.27.1` first because it must match the original upstream
OSRM tag exactly for downstream version selection in the R package.

The first two `v5.27.1` attempts failed on Windows:

- First failure: missing `boost_unit_test_framework`.
- Second failure: missing header-only Boost modules such as
  `boost/format.hpp`, `boost/heap/...`, `boost/geometry.hpp`, and
  `boost/spirit/...`.

Historical successful `v5.27.1` runs showed that Windows previously used
the broad vcpkg `boost` package. We restored that dependency strategy
while keeping the new release/test gates.

## Current State

`v5.27.1` has been successfully rebuilt and published after immutable
releases were enabled.

Successful run:

``` text
https://github.com/e-kotov/osrm-binaries/actions/runs/28959165452
```

Published release:

``` text
https://github.com/e-kotov/osrm-binaries/releases/tag/v5.27.1
```

The release contains:

- `checksums.txt`
- `osrm-v5.27.1-darwin-arm64-Release.tar.gz`
- `osrm-v5.27.1-darwin-x64-Release.tar.gz`
- `osrm-v5.27.1-linux-arm64-Release.tar.gz`
- `osrm-v5.27.1-linux-x64-Release.tar.gz`
- `osrm-v5.27.1-win32-x64-Release.tar.gz`

All five build jobs, all five sanity test jobs, and `Publish Release`
completed successfully.

## Remaining Work

Continue rebuilding the remaining releases in chronological order:

``` text
v6.0.0
v26.4.0
v26.4.1
v26.5.0
v26.6.0
v26.6.1
v26.6.2
v26.6.3
v26.6.4
v26.6.5
v26.7.0
v26.7.1
v26.7.2
```

For each tag:

1.  Run the workflow with `version_tag=<tag>`.
2.  Wait for the workflow to complete.
3.  Confirm all build, test, and release jobs passed.
4.  Confirm the GitHub release exists and has five platform archives
    plus `checksums.txt`.
5.  Continue only after the previous tag is fully published, so GitHub
    release ordering stays chronological by `published_at`.

After the binary releases are rebuilt, make a small final set of commits
in the R package:

1.  Update `R/osrm_install.R` documentation and `tested_versions` so the
    package only advertises versions that have been rebuilt and
    validated through the new immutable binary workflow.
2.  Update `NEWS.md` with the user-facing change:
    [`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)
    now defaults to the self-hosted immutable `e-kotov/osrm-binaries`
    releases with checksum verification.
3.  Run or trigger `.github/workflows/osrm-live-tests.yml` after the
    rebuilt releases exist, because that is the real package-level
    acceptance test.
4.  Review the generated badge output after the live workflow finishes,
    since badge status is driven by the same discovered versions that
    users can install.
5.  If any old or newly rebuilt tag fails live CI, either fix the binary
    release workflow and recreate that tag before continuing, or remove
    that tag from the R package’s advertised/tested versions.

## Useful Commands

Trigger one release:

``` bash
gh workflow run build.yml -f version_tag=v6.0.0
```

Watch a run:

``` bash
gh run watch <run_id> --exit-status
```

Check release assets:

``` bash
gh release view v6.0.0 --json tagName,publishedAt,url,assets \
  --jq '{tagName,publishedAt,url,assets:[.assets[].name]}'
```

## Key Caution

Do not replace Windows
`vcpkg install bzip2 boost expat stxxl lua tbb libarchive libzip --triplet x64-windows`
with a narrow Boost component list unless every historical tag is
tested. Older OSRM versions include Boost headers that are not
represented by CMake link components, so a targeted list can pass
configuration but fail during compilation.

Also avoid treating a successful binary release as sufficient by itself.
The binary release workflow only checks that `osrm-extract --version`
and `osrm-routed --version` start correctly. The R package live workflow
is still required because it tests installation, graph preparation,
server startup, HTTP routing, and `osrm` package integration.
