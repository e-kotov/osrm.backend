# Session Handoff — OSRM Backend / osrm-binaries

Date: 2026-07-09 (UTC)

## Purpose

This file documents the work done in this session and the current state
so a new agent (or human) can pick up where we left off.

## High-level summary

- Fixed fragile live tests that referenced a hard-coded `v26.7.2`. Tests
  now discover available releases for the current platform and exercise
  only versions that exist on both providers (default
  `e-kotov/osrm-binaries` and `Project-OSRM/osrm-backend`) where
  reasonable.
- Opened PR `#27` (branch `fix-tests-use-available-releases`) and merged
  it to `main`.
- Verified package tests locally with live tests disabled
  (`RUN_OSRM_LIVE_TESTS=false`) — focused compatibility tests parse and
  skip cleanly.
- Ran the repo-level live integration (GitHub Actions) on the PR branch
  and on `main`. Live integration on `main` completed successfully.
- Triggered rebuild of the next binary release `v6.0.0` in
  `e-kotov/osrm-binaries` (build run in progress at the time of
  handoff).
- `v6.0.0` build completed and release published (immutable). See notes
  below.
- `v26.4.0` build run started and is currently in progress (run id
  28997079466).

## What changed (code)

- tests/testthat/setup-osrm.R
  - Stop hard-coding `v26.7.2`; try to install the first available
    release (from `osrm_check_available_versions(prereleases = TRUE)`)
    when running locally/CI.
- tests/testthat/test-compatibility.R
  - Compute `test_versions` from `e-kotov/osrm-binaries` and
    `official_versions` from `Project-OSRM/osrm-backend`.
  - Use [`intersect()`](https://rdrr.io/r/base/sets.html) to run
    cross-provider compatibility only on tags available on both
    providers.
  - Skip versions where the “official” provider cannot prepare the test
    graph (do not fail the suite).
  - For macOS Rosetta test, query available darwin-x64 versions instead
    of using `v26.7.2`.

## Commits / PR

- Branch created: `fix-tests-use-available-releases`
  - Local commit (branch): `bb7e602` (tests: prefer available OSRM
    releases; avoid hard-coded v26.7.2; cross-provider tests use
    intersection and skip when none)
- PR: <https://github.com/e-kotov/osrm.backend/pull/27> (created,
  reviewed, merged)
- Merge: fast-forward applied to `main` (new main tip `047dcf8` as
  observed when merging)

## CI / runs (key URLs & status)

- Live integration (PR branch run): success —
  <https://github.com/e-kotov/osrm.backend/actions/runs/28976799508>
- Live integration (main after merge): success —
  <https://github.com/e-kotov/osrm.backend/actions/runs/28977425237>
- Binary rebuild: `v6.0.0` published —
  <https://github.com/e-kotov/osrm-binaries/releases/tag/v6.0.0>
  - Published at: 2026-07-08T22:38:28Z
  - Immutable: true
  - Assets (6): checksums.txt + darwin-arm64, darwin-x64, linux-arm64,
    linux-x64, win32-x64 archives
- Binary rebuild triggered for `v26.4.0` in `e-kotov/osrm-binaries`: run
  in progress —
  <https://github.com/e-kotov/osrm-binaries/actions/runs/28997079466>
- Note: `v5.27.1` was previously rebuilt & published (immutable) and is
  visible to the package tests.
  - Release:
    <https://github.com/e-kotov/osrm-binaries/releases/tag/v5.27.1>
- Watch that run until completion; each platform build is a separate job
  (linux/macOS/windows/arm).

## Next work items for the new agent

1.  Monitor the v6.0.0 build run:
    <https://github.com/e-kotov/osrm-binaries/actions/runs/28977832998>
    - Success criteria for each tag:
      - All platform build jobs complete and pass.
      - A GitHub Release is created for the tag (e.g., `v6.0.0`) with
        these assets:
        - `checksums.txt`
        - `osrm-<tag>-darwin-arm64-Release.tar.gz`
        - `osrm-<tag>-darwin-x64-Release.tar.gz`
        - `osrm-<tag>-linux-arm64-Release.tar.gz`
        - `osrm-<tag>-linux-x64-Release.tar.gz`
        - `osrm-<tag>-win32-x64-Release.tar.gz`
      - The release should be immutable (check `isImmutable` / published
        as immutable release).
    - Commands:
      - Trigger workflow for a tag:
        - `gh -R e-kotov/osrm-binaries workflow run build.yml -f version_tag=vX.Y.Z`
      - Watch a run (example for v26.4.0):
        `gh -R e-kotov/osrm-binaries run view 28997079466 --json conclusion,status,jobs`
      - Watch a run:
        - `gh -R e-kotov/osrm-binaries run watch <run_id> --exit-status`
      - Check release assets:
        - `gh -R e-kotov/osrm-binaries release view vX.Y.Z --json tagName,publishedAt,isImmutable,url,assets --jq '{tagName,publishedAt,url,assets:[.assets[].name]}'`
2.  After each rebuilt release is published:
    - Wait for the `osrm-binaries` release to exist and validate
      checksums.
    - Optionally run the R package live integration (or wait for
      scheduled run) to verify install & routing.
      - Workflow: `osrm-live-tests.yml` in `e-kotov/osrm.backend`.
      - Trigger manual dispatch (if needed):
        `gh workflow run osrm-live-tests.yml --ref main`
3.  Continue rebuilding releases in chronological order. Remaining
    targets (from earlier plan):
    - v6.0.0 (in progress)
    - v26.4.0
    - v26.4.1
    - v26.5.0
    - v26.6.0
    - v26.6.1
    - v26.6.2
    - v26.6.3
    - v26.6.4
    - v26.6.5
    - v26.7.0
    - v26.7.1
    - v26.7.2
4.  When a reasonable set of releases is rebuilt (e.g., v5.27.1 +
    v6.0.0 + v26.x validated):
    - Update `R/osrm_install.R` `tested_versions` array to reflect only
      rebuilt/tested versions.
    - Update `NEWS.md` describing the change (osrm_install() defaults to
      self-hosted immutable releases and verifies checksums).
    - Open a small PR for these package metadata changes and run the
      live integration to verify.

## Troubleshooting / common failure modes

- Missing checksums.txt or mismatched checksum: release job should fail;
  do not proceed to package-level tests until checksums are present and
  correct.
- Windows builds may fail due to missing vcpkg/Boost components.
  Historical solution: include broad `vcpkg install boost ...` or keep
  the broad `boost` package rather than attempting narrow selections.
- macOS packaging may require bundling/relinking Homebrew `.dylib`
  dependencies — watch for transitive dylib failures.
- If official upstream binaries are missing or incomplete, the R package
  live tests will skip or fail for those versions. Our test changes
  avoid hard failures by skipping versions not available on both
  providers.

## Quick commands (cheat sheet)

- Trigger a binary build for tag:
  - `gh -R e-kotov/osrm-binaries workflow run build.yml -f version_tag=v6.0.0`
- Watch a run:
  - `gh run watch <run_id> --exit-status`
- View a release’s assets:
  - `gh -R e-kotov/osrm-binaries release view v6.0.0 --json tagName,publishedAt,isImmutable,assets --jq '{tagName,publishedAt,isImmutable,assets:[.assets[].name]}'`
- Re-run live package integration:
  - `gh workflow run osrm-live-tests.yml -f ref=main` (or dispatch via
    GitHub UI)

## Notes / context

- I intentionally made minimal, tightly scoped edits to tests so the
  behavior is safer for CI runs while retaining the original intent of
  the tests (cross-provider compatibility, Rosetta checks).
- Local quick verification steps performed:
  - Ran the compatibility test file with `RUN_OSRM_LIVE_TESTS=false` (so
    the live installs are skipped). That run parsed and skipped as
    intended.
  - Ran the full test suite locally (with live tests disabled) and
    observed no regressions in the non-live tests.

## If you take over

1.  Open this file and follow “Next work items”.  
2.  Start by watching the `v6.0.0` build run; when it completes, verify
    the release assets and checksums.  
3.  Rebuild additional tags in the order above. After rebuilding a
    reasonable set, prepare the package metadata update PR
    (tested_versions + NEWS.md) and run the live package CI to validate
    end-to-end behavior.

—— End of handoff.
