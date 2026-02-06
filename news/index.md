# Changelog

## osrm.backend (development version)

## osrm.backend 0.2.0

### Breaking Changes

- The `osrm.server.log_file` option no longer accepts a list for
  separate stdout/stderr files. This feature has been removed due to
  potential deadlock issues. Use a single character path instead:
  `options(osrm.server.log_file = "path/to/logfile.log")`. If a list is
  provided, it will silently fall back to the default temporary file
  behavior.

- Default logging behavior changed:
  [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md)
  now writes logs to a temporary file by default instead of using pipes.
  This prevents deadlocks in R’s single-threaded environment while
  preserving logs for debugging. Use `verbose = TRUE` to see output in
  the console.

## osrm.backend 0.1.1

CRAN release: 2025-12-07

- Fixes in test suite to address CRAN warnings.

## osrm.backend 0.1.0

CRAN release: 2025-12-03

- Initial CRAN submission.
