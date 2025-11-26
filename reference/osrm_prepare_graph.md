# Prepare OSRM Graph for Routing (Extract + Partition/Contract)

High-level wrapper that first runs `osrm-extract` on an OSM file to
produce the base `.osrm` graph, then prepares it for routing via either
the MLD pipeline (`osrm-partition` + `osrm-customize`) or the CH
pipeline (`osrm-contract`).

## Usage

``` r
osrm_prepare_graph(
  input_osm,
  profile = osrm_find_profile("car.lua"),
  threads = 8L,
  overwrite = FALSE,
  verbosity = c("INFO", "NONE", "ERROR", "WARNING", "DEBUG"),
  data_version = NULL,
  small_component_size = 1000L,
  with_osm_metadata = FALSE,
  parse_conditional_restrictions = FALSE,
  location_dependent_data = NULL,
  disable_location_cache = FALSE,
  dump_nbg_graph = FALSE,
  algorithm = c("mld", "ch"),
  balance = 1.2,
  boundary = 0.25,
  optimizing_cuts = 10L,
  max_cell_sizes = c(128, 4096, 65536, 2097152),
  quiet = FALSE,
  verbose = FALSE,
  spinner = TRUE,
  echo_cmd = FALSE
)
```

## Arguments

- input_osm:

  A string. Path to the input OSM file (`.osm`, `.osm.bz2`, or
  `.osm.pbf`) or a directory containing exactly one OSM file with a
  supported extension.

- profile:

  A string. Path to the OSRM Lua profile (e.g. returned by
  `osrm_find_profile("car.lua")`).

- threads:

  An integer. Number of threads for extract and partition/contract;
  default `8`.

- overwrite:

  A logical. If `FALSE`, stops if any existing `.osrm*` files matching
  the base name are found alongside `input_osm`. Set to `TRUE` to
  overwrite them.

- verbosity:

  A string. Log verbosity for extract/partition/contract (one of
  `"NONE","ERROR","WARNING","INFO","DEBUG"`); default `"INFO"`.

- data_version:

  A string or `NULL`. Passed to `osrm-extract` via `-d`; default `NULL`.

- small_component_size:

  An integer. For extract & partition; default `1000`.

- with_osm_metadata:

  A logical. Adds `--with-osm-metadata` during extract; default `FALSE`.

- parse_conditional_restrictions:

  A logical. Adds `--parse-conditional-restrictions`; default `FALSE`.

- location_dependent_data:

  A string or `NULL`. Path to GeoJSON for extract; default `NULL`.

- disable_location_cache:

  A logical. Adds `--disable-location-cache`; default `FALSE`.

- dump_nbg_graph:

  A logical. Adds `--dump-nbg-graph`; default `FALSE`.

- algorithm:

  A string. One of `"mld"` (default) or `"ch"`.

- balance:

  A numeric. Balance for `osrm-partition`; default `1.2`.

- boundary:

  A numeric. Boundary percentage for `osrm-partition`; default `0.25`.

- optimizing_cuts:

  An integer. Optimizing cuts for `osrm-partition`; default `10`.

- max_cell_sizes:

  A numeric vector. Max cell sizes for `osrm-partition`; default
  `c(128,4096,65536,2097152)`.

- quiet:

  A logical. Master switch that suppresses package messages and process
  output when `TRUE`; default `FALSE`.

- verbose:

  A logical. When `TRUE` and `quiet = FALSE`, streams stdout and stderr
  from the underlying
  [`processx::run`](http://processx.r-lib.org/reference/run.md) calls.

- spinner:

  A logical. When `TRUE` and `quiet = FALSE`, shows a spinner instead of
  live logs; default `TRUE`.

- echo_cmd:

  A logical. When `TRUE` and `quiet = FALSE`, prints each command before
  running; default `FALSE`.

## Value

An object of class `osrm_job` with the following elements:

- osrm_job_artifact:

  The path to the final routing-ready graph file (`.osrm.hsgr` for CH or
  `.osrm.mldgr` for MLD).

- osrm_working_dir:

  The directory containing all OSRM files.

- logs:

  A list of
  [`processx::run`](http://processx.r-lib.org/reference/run.md) results
  for each stage: `extract`, `partition`/`contract`, and `customize` (if
  MLD).

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  install_dir <- osrm_install(
    version = "latest",
    path_action = "session",
    quiet = TRUE
  )

  # Prepare a routing-ready graph with the default MLD pipeline
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
  graph$osrm_job_artifact

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
