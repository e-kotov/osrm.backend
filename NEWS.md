# osrm.backend 0.3.1

## Major Changes

* Introduced `osrm_binaries_provider` argument to `osrm_install()` (default: `"default"`). This automatically fetches static, highly-compatible binaries built directly by this package's maintainer (`e-kotov/osrm-binaries`). These new binaries include native `linux-arm64` and `darwin-arm64` support, bundle required libraries like Intel TBB natively to avoid legacy runtime hacks, and reduce overhead by dropping unnecessary NodeJS wrappers. The original upstream binaries remain fully supported via `osrm_binaries_provider = "official"`.
* Added `download_url` and `file_path` parameters to `osrm_install()` for deterministic manual installations without relying on GitHub API metadata lookups.
* Switched binary integrity verification to use GitHub release asset digests first, with `checksums.txt` as a fallback, ensuring installation security without requiring R package updates when new binary builds are released.
* Marked all currently published immutable OSRM binary releases through `v26.7.3` as validated by `osrm.backend`.

## Bug fixes

* Heavily optimized GitHub API rate limit handling during installation. When fetching known versions from the default provider, the package now bypasses API checks entirely by directly constructing asset URLs.
* Increased stability of the live integration tests against custom routing instances and proxy-blocked networks.
* Fixes for install process of OSRM binaries v26+ on Windows.
* Added weekly cross-provider integration tests (`tests/testthat/test-compatibility.R`) to automatically verify that custom binaries produce identical routing results and are fully cross-compatible with official releases.


# osrm.backend 0.3.0

## New features

* Added `osrm_gui()`, an interactive Shiny web application for exploring and visualizing the loaded OSRM routing network. It comes with auto-centering logic to reliably detect the source OSM PBF file or retrieve the extent from the server registry or server object for auto extent.

## Improvements

* Improved `osrm_servers()` with a custom S3 print method for better interactive readability and a new `output` argument to return raw metadata as a list.

* Simplified server IDs in the registry to `osrm-{port}-{pid}` format for better readability and programmatic access.

* OSRM servers and running OSRM servers registry maintained by the package pre-save the extent info for `osrm_gui()` to use it for auto-centering.


* Added a helpful hint to `osrm_start()` error messages when a graph is detected to be incompatible with the currently installed OSRM version.

* Validated and added official support for OSRM `v26.4.0` and `v26.4.1` releases, ensuring the necessary runtime libraries (TBB and BZip2) are correctly fetched and patched across all supported platforms (macOS, Windows, and Linux).

* Weekly live tests to check for potential regressions, successful OSRM binaries installation and possible issues when new versions of OSRM backend binaries.


# osrm.backend 0.2.0

## Breaking Changes

* The `osrm.server.log_file` option no longer accepts a list for separate stdout/stderr files. 
  This feature has been removed due to potential deadlock issues. Use a single character path 
  instead: `options(osrm.server.log_file = "path/to/logfile.log")`. If a list is provided, 
  it will silently fall back to the default temporary file behavior.

* Default logging behavior changed: `osrm_start_server()` now writes logs to a temporary file 
  by default instead of using pipes. This prevents deadlocks in R's single-threaded environment 
  while preserving logs for debugging. Use `verbose = TRUE` to see output in the console.

# osrm.backend 0.1.1

* Fixes in test suite to address CRAN warnings.

# osrm.backend 0.1.0

* Initial CRAN submission.
