# Start an OSRM MLD/CH server with `osrm-routed`

Launches an `osrm-routed` process pointing at a localized OSRM graph
(either `.osrm.mldgr` for MLD or `.osrm.hsgr` for CH/CoreCH).

## Usage

``` r
osrm_start_server(
  osrm_path,
  version = FALSE,
  help = FALSE,
  verbosity = c("INFO", "ERROR", "WARNING", "NONE", "DEBUG"),
  trial = FALSE,
  ip = "0.0.0.0",
  port = 5001L,
  threads = 8L,
  shared_memory = FALSE,
  memory_file = NULL,
  mmap = FALSE,
  dataset_name = NULL,
  algorithm = NULL,
  max_viaroute_size = 500L,
  max_trip_size = 100L,
  max_table_size = 100L,
  max_matching_size = 100L,
  max_nearest_size = 100L,
  max_alternatives = 3L,
  max_matching_radius = -1L,
  quiet = FALSE,
  verbose = FALSE,
  echo_cmd = FALSE
)
```

## Arguments

- osrm_path:

  Character(1). Path to the `.osrm.mldgr` or `.osrm.hsgr` file

- version:

  Logical; if `TRUE`, prints version and exits

- help:

  Logical; if `TRUE`, prints help and exits

- verbosity:

  Character; one of `"NONE","ERROR","WARNING","INFO","DEBUG"`

- trial:

  Logical or integer; if `TRUE` or \>0, quits after initialization
  (default: `FALSE`)

- ip:

  Character; IP address to bind (default: `"0.0.0.0"`)

- port:

  Integer; TCP port to listen on (default: `5001`)

- threads:

  Integer; number of worker threads (default: `8`)

- shared_memory:

  Logical; load graph from shared memory (default: `FALSE`)

- memory_file:

  Character or NULL; DEPRECATED (behaves like `mmap`)

- mmap:

  Logical; memory-map data files (default: `FALSE`)

- dataset_name:

  Character or NULL; name of shared memory dataset

- algorithm:

  Character or NULL; one of `"CH","CoreCH","MLD"`. If `NULL` (default),
  auto-selected based on file extension

- max_viaroute_size:

  Integer (default: `500`)

- max_trip_size:

  Integer (default: `100`)

- max_table_size:

  Integer (default: `100`)

- max_matching_size:

  Integer (default: `100`)

- max_nearest_size:

  Integer (default: `100`)

- max_alternatives:

  Integer (default: `3`)

- max_matching_radius:

  Integer; use `-1` for unlimited (default: `-1`)

- quiet:

  Logical; when `TRUE`, suppresses package messages.

- verbose:

  Logical; reserved for controlling future server verbosity, included
  for interface consistency with
  [`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md).
  Defaults to `FALSE`.

- echo_cmd:

  Logical; echo command line to console before launch (default: `FALSE`)

## Value

A [`processx::process`](http://processx.r-lib.org/reference/process.md)
object for the running server (also registered internally).

## Details

The server's standard output and error streams can be controlled via R
options for non-interactive use or persistent logging. By default, they
are captured as pipes, which allows for reading output directly within
the R session (e.g., via `osrm_server$read_output_lines()`).

To redirect the server's output to one or more files, you can set the
`osrm.server.log_file` R option before calling this function:

- **Combined Log:** To send both `stdout` and `stderr` to a single file,
  provide a file path:
  `options(osrm.server.log_file = "path/to/osrm.log")`

- **Separate Logs:** To send `stdout` and `stderr` to separate files,
  provide a named list:
  `options(osrm.server.log_file = list(stdout = "out.log", stderr = "err.log"))`

- **Default Behavior:** To restore the default behavior of using pipes,
  set the option to `NULL`: `options(osrm.server.log_file = NULL)`

You can override the `osrm-routed` executable via
`options(osrm.routed.exec = "/full/path/to/osrm-routed")`.

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  install_dir <- osrm_install(
    version = "latest",
    path_action = "session",
    quiet = TRUE
  )

  # Build a graph then launch an OSRM server on a custom port
  pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
  dir.create(osrm_dir, recursive = TRUE)
  tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
  file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)

  graph <- osrm_prepare_graph(
    input_osm = tmp_pbf,
    overwrite = TRUE,
    threads = 1L,
    algorithm = "mld"
  )

  server <- osrm_start_server(
    osrm_path = graph$osrm_job_artifact,
    port = 6000,
    threads = 1L
  )

  # Later, stop the server again
  osrm_stop(server)

  osrm_uninstall(
    dest_dir = install_dir,
    clear_path = TRUE,
    force = TRUE,
    quiet = TRUE
  )
  unlink(osrm_dir, recursive = TRUE)
}
# }
```
