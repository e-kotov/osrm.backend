# osrm.backend (development version)

* Added `osrm_gui()`, an interactive Shiny web application for exploring and visualizing the loaded OSRM routing network.

* Tested with OSRM `v26.4.0` and `v26.4.1` releases, ensuring the necessary runtime libraries (TBB and BZip2) are correctly fetched and patched across all supported platforms (macOS, Windows, and Linux).

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
