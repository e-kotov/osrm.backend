# osrm.backend (development version)

* Added `osrm_gui()`, an interactive Shiny web application for exploring and visualizing the loaded OSRM routing network.
* Improved `osrm_servers()` with a custom S3 print method for better interactive readability and a new `output` argument to return raw metadata as a list.
* Simplified server IDs in the registry to `osrm-{port}-{pid}` format for better readability and programmatic access.
* Enhanced `osrm_gui()` auto-centering logic to reliably detect the source OSM PBF file and store the center point in the server registry for faster and more resilient launches.
* Added a helpful hint to `osrm_start()` error messages when a graph is detected to be incompatible with the currently installed OSRM version.
* Validated and added official support for OSRM `v26.4.0` and `v26.4.1` releases, ensuring the necessary runtime libraries (TBB and BZip2) are correctly fetched and patched across all supported platforms (macOS, Windows, and Linux).
* Updated `osrm_gui()` examples and tests to follow CRAN policies for interactive applications.

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
