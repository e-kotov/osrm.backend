# Contract OSRM Graph for Contraction Hierarchies (CH)

Run the `osrm-contract` tool to contract an OSRM graph for the CH
pipeline. After running, a companion `<base>.osrm.hsgr` file must exist
to confirm success.

## Usage

``` r
osrm_contract(
  input_osrm,
  threads = 8L,
  verbosity = c("INFO", "NONE", "ERROR", "WARNING", "DEBUG"),
  segment_speed_file = NULL,
  turn_penalty_file = NULL,
  edge_weight_updates_over_factor = 0,
  parse_conditionals_from_now = 0,
  time_zone_file = NULL,
  quiet = FALSE,
  verbose = FALSE,
  spinner = TRUE,
  echo_cmd = FALSE
)
```

## Arguments

- input_osrm:

  A string. Path to a `.osrm.timestamp` file, the base path to the
  `.osrm` files (without extension), or a directory containing exactly
  one `.osrm.timestamp` file.

- threads:

  An integer. Number of threads to use; default `8`.

- verbosity:

  A string. Log verbosity level passed to `-l/--verbosity` (one of
  `"NONE","ERROR","WARNING","INFO","DEBUG"`); default `"INFO"`.

- segment_speed_file:

  A string or `NULL`. Path to nodeA,nodeB,speed CSV; default `NULL`.

- turn_penalty_file:

  A string or `NULL`. Path to from\_,to\_,via_nodes,penalties CSV;
  default `NULL`.

- edge_weight_updates_over_factor:

  A numeric. Threshold for logging large weight updates; default `0`.

- parse_conditionals_from_now:

  A numeric. UTC timestamp for conditional restrictions; default `0`.

- time_zone_file:

  A string or `NULL`. GeoJSON file for time zone boundaries; default
  `NULL`.

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

  The path to the contracted `.osrm.hsgr` file.

- osrm_working_dir:

  The directory containing all OSRM files.

- logs:

  The [`processx::run`](http://processx.r-lib.org/reference/run.md)
  result object.

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  install_dir <- osrm_install(
    version = "latest",
    path_action = "session",
    quiet = TRUE
  )

  # Prepare a small graph then contract it for the CH pipeline
  pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
  dir.create(osrm_dir, recursive = TRUE)
  tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
  file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
  profile <- osrm_find_profile("car.lua")

  extract_job <- osrm_extract(
    input_osm = tmp_pbf,
    profile = profile,
    overwrite = TRUE,
    threads = 1L
  )

  ch_graph <- osrm_contract(extract_job, threads = 1L, verbose = TRUE)
  ch_graph$osrm_job_artifact

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
