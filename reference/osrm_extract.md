# Extract OSM into OSRM Graph Files

Run the `osrm-extract` tool to preprocess an OSM file (`.osm`,
`.osm.bz2`, or `.osm.pbf`) into the base `.osrm` graph files using a
specified Lua profile. After running, a companion
`<base>.osrm.timestamp` file must exist to confirm success.

## Usage

``` r
osrm_extract(
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

  An integer. Number of threads for `-t/--threads`; default `8` (OSRM's
  default).

- overwrite:

  A logical. If `FALSE` (default), stops when any existing `.osrm*`
  files matching the base name are found alongside `input_osm`. Set to
  `TRUE` to proceed regardless.

- verbosity:

  A string. Log verbosity level passed to `-l/--verbosity` (one of
  `"NONE","ERROR","WARNING","INFO","DEBUG"`), default `"INFO"`.

- data_version:

  A string or `NULL`. Passed to `-d/--data_version`; default `NULL`, in
  which case the option is omitted.

- small_component_size:

  An integer. For `--small-component-size`; default `1000` (OSRM's
  default).

- with_osm_metadata:

  A logical. If `TRUE`, adds `--with-osm-metadata`; default `FALSE`.

- parse_conditional_restrictions:

  A logical. If `TRUE`, adds `--parse-conditional-restrictions`; default
  `FALSE`.

- location_dependent_data:

  A string or `NULL`. Path to GeoJSON, passed to
  `--location-dependent-data`; default `NULL`, in which case the option
  is omitted.

- disable_location_cache:

  A logical. If `TRUE`, adds `--disable-location-cache`; default
  `FALSE`.

- dump_nbg_graph:

  A logical. If `TRUE`, adds `--dump-nbg-graph`; default `FALSE`.

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

  The path to the generated `.osrm.timestamp` file.

- osrm_working_dir:

  The directory containing all OSRM files.

- logs:

  The [`processx::run`](http://processx.r-lib.org/reference/run.md)
  result object.

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  # Install OSRM (temporary, session PATH)
  install_dir <- osrm_install(
    version = "latest",
    path_action = "session",
    quiet = TRUE
  )

  # copy example OSM PBF into a temporary workspace to avoid polluting pkg data
  pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
  dir.create(osrm_dir, recursive = TRUE)
  tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
  file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)

  # Find the path to the profile first
  car_profile <- osrm_find_profile("car.lua")

  # extract OSRM graph files
  result <- osrm_extract(
    input_osm                  = tmp_pbf,
    profile                    = car_profile,
    overwrite                  = TRUE,
    threads                    = 1L
  )
  # path to generated .osrm files (specifically, the .osrm.timestamp file)
  result$osrm_job_artifact

  # Clean up binaries and workspace
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
