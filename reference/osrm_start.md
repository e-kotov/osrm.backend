# Start an OSRM Server with Automatic Setup

A high-level, "one-shot" function to start an OSRM server that
automatically handles OSRM installation and graph preparation. This is
the recommended function for most users to get a server running quickly
with minimal steps.

## Usage

``` r
osrm_start(path, algorithm = c("mld", "ch"), verbose = FALSE, ...)
```

## Arguments

- path:

  A string. Path to the input data. Can be one of:

  - A path to an OSM file (e.g., `/path/to/data.osm.pbf`).

  - A path to a directory containing OSRM graph files or an OSM file.

  - A direct path to a final graph file (`.osrm.mldgr` or `.osrm.hsgr`).

- algorithm:

  A string specifying the routing algorithm to use for graph
  preparation, either `"mld"` (Multi-Level Dijkstra, default) or `"ch"`
  (Contraction Hierarchies). This is only used when `osrm_prepare_graph`
  is automatically called.

- verbose:

  A logical. If `FALSE` (default), suppresses detailed console output
  from backend commands. If `TRUE`, shows all output, which is useful
  for debugging.

- ...:

  Additional arguments passed on to
  [`osrm_prepare_graph()`](https://www.ekotov.pro/osrm.backend/reference/osrm_prepare_graph.md)
  (e.g., `overwrite = TRUE`) and
  [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md)
  (e.g., `port = 5001`).

## Value

A [`processx::process`](http://processx.r-lib.org/reference/process.md)
object for the running server.

## Details

This function is designed for convenience and automates the entire setup
process. By default, it is not verbose and only prints high-level status
messages.

1.  **Check for OSRM Installation:** It first verifies if the
    `osrm-routed` executable is available in the system's `PATH`. If
    not, it automatically calls
    [`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)
    to download and install the latest stable version.

2.  **Process Input Path and Prepare Graph:** The function intelligently
    handles the `path` argument to find or create the necessary graph
    files. If the graph files do not exist, it automatically runs
    [`osrm_prepare_graph()`](https://www.ekotov.pro/osrm.backend/reference/osrm_prepare_graph.md)
    to build them, which may take some time.

3.  **Start Server:** Once the graph files are located or prepared, it
    launches the `osrm-routed` server and prints a confirmation message
    with the server's PID and port.

For advanced users or for debugging, set `verbose = TRUE` to see the
detailed console output from the installation and graph preparation
steps. For full manual control, use the lower-level functions like
[`osrm_prepare_graph()`](https://www.ekotov.pro/osrm.backend/reference/osrm_prepare_graph.md)
and
[`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md)
directly.

## See also

[`osrm_stop()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop.md),
[`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md)
for manual server startup.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the path to the example OSM file included in the package
pbf_file <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")

# Create a temporary directory to work in
temp_dir <- tempdir()
file.copy(pbf_file, temp_dir)
local_pbf <- file.path(temp_dir, "cur.osm.pbf")

# Start the server with one command.
# It will quietly install OSRM and prepare the graph if needed.
osrm_process <- osrm_start(local_pbf)

# Stop the server when done.
osrm_stop()

# To see all backend logs during setup, use verbose = TRUE
osrm_process_verbose <- osrm_start(local_pbf, verbose = TRUE)
osrm_stop()
} # }
```
