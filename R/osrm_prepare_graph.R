#' Prepare OSRM Graph for Routing (Extract + Partition/Contract)
#'
#' High-level wrapper that first runs `osrm-extract` on an OSM file
#' to produce the base `.osrm` graph, then prepares it for routing via
#' either the MLD pipeline (`osrm-partition` + `osrm-customize`) or the CH pipeline (`osrm-contract`).
#'
#' @param input_osm A string. Path to the input OSM file (`.osm`, `.osm.bz2`, or `.osm.pbf`) or a directory containing exactly one OSM file with a supported extension.
#' @param profile A string. Path to the OSRM Lua profile (e.g. returned by `osrm_find_profile("car.lua")`).
#' @param threads An integer. Number of threads for extract and partition/contract; default `8`.
#' @param overwrite A logical. If `FALSE`, stops if any existing `.osrm*` files matching
#'   the base name are found alongside `input_osm`. Set to `TRUE` to overwrite them.
#' @param verbosity A string. Log verbosity for extract/partition/contract
#'   (one of `"NONE","ERROR","WARNING","INFO","DEBUG"`); default `"INFO"`.
#' @param data_version A string or `NULL`. Passed to `osrm-extract` via `-d`; default `NULL`.
#' @param small_component_size An integer. For extract & partition; default `1000`.
#' @param with_osm_metadata A logical. Adds `--with-osm-metadata` during extract; default `FALSE`.
#' @param parse_conditional_restrictions A logical. Adds `--parse-conditional-restrictions`; default `FALSE`.
#' @param location_dependent_data A string or `NULL`. Path to GeoJSON for extract; default `NULL`.
#' @param disable_location_cache A logical. Adds `--disable-location-cache`; default `FALSE`.
#' @param dump_nbg_graph A logical. Adds `--dump-nbg-graph`; default `FALSE`.
#' @param algorithm A string. One of `"mld"` (default) or `"ch"`.
#' @param balance A numeric. Balance for `osrm-partition`; default `1.2`.
#' @param boundary A numeric. Boundary percentage for `osrm-partition`; default `0.25`.
#' @param optimizing_cuts An integer. Optimizing cuts for `osrm-partition`; default `10`.
#' @param max_cell_sizes A numeric vector. Max cell sizes for `osrm-partition`; default `c(128,4096,65536,2097152)`.
#' @param quiet A logical. Master switch that suppresses package messages and
#'   process output when `TRUE`; default `FALSE`.
#' @param verbose A logical. When `TRUE` and `quiet = FALSE`, streams stdout and
#'   stderr from the underlying `processx::run` calls.
#' @param spinner A logical. When `TRUE` and `quiet = FALSE`, shows a spinner
#'   instead of live logs; default `TRUE`.
#' @param echo_cmd A logical. When `TRUE` and `quiet = FALSE`, prints each
#'   command before running; default `FALSE`.
#'
#' @return A list with elements:
#' \describe{
#'   \item{osrm_path}{The normalized path to the final routing-ready graph file (`.osrm.hsgr` for CH or `.osrm.mldgr` for MLD). This can be passed over to [osrm_start_server()].}
#'   \item{logs}{A list of `processx::run` results for each stage:
#'     `extract`, `partition`/`contract`, and `customize` (if MLD).}
#' }
#'
#' @export
osrm_prepare_graph <- function(
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
) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required for osrm_prepare_graph", call. = FALSE)
  }

  # 1) Extract
  extract_res <- osrm_extract(
    input_osm = input_osm,
    profile = profile,
    overwrite = overwrite,
    verbosity = verbosity,
    data_version = data_version,
    threads = threads,
    small_component_size = small_component_size,
    with_osm_metadata = with_osm_metadata,
    parse_conditional_restrictions = parse_conditional_restrictions,
    location_dependent_data = location_dependent_data,
    disable_location_cache = disable_location_cache,
    dump_nbg_graph = dump_nbg_graph,
    quiet = quiet,
    verbose = verbose,
    spinner = spinner,
    echo_cmd = echo_cmd
  )
  base <- extract_res$osrm_path
  logs_list <- list(extract = extract_res$logs)

  # 2) Partition+Customize or Contract
  algorithm <- match.arg(algorithm)
  if (algorithm == "mld") {
    part_res <- osrm_partition(
      input_osrm = base,
      threads = threads,
      verbosity = verbosity,
      balance = balance,
      boundary = boundary,
      optimizing_cuts = optimizing_cuts,
      small_component_size = small_component_size,
      max_cell_sizes = max_cell_sizes,
      quiet = quiet,
      verbose = verbose,
      spinner = spinner,
      echo_cmd = echo_cmd
    )
    logs_list$partition <- part_res$logs

    osrm_graph <- osrm_customize(
      input_osrm = part_res$osrm_path,
      threads = threads,
      verbosity = verbosity,
      segment_speed_file = NULL,
      turn_penalty_file = NULL,
      edge_weight_updates_over_factor = 0,
      parse_conditionals_from_now = 0,
      time_zone_file = NULL,
      quiet = quiet,
      verbose = verbose,
      spinner = spinner,
      echo_cmd = echo_cmd
    )
    logs_list$customize <- osrm_graph$logs
  } else {
    osrm_graph <- osrm_contract(
      input_osrm = base,
      threads = threads,
      verbosity = verbosity,
      segment_speed_file = NULL,
      turn_penalty_file = NULL,
      edge_weight_updates_over_factor = 0,
      parse_conditionals_from_now = 0,
      time_zone_file = NULL,
      quiet = quiet,
      verbose = verbose,
      spinner = spinner,
      echo_cmd = echo_cmd
    )
    logs_list$contract <- osrm_graph$logs
  }

  final_path <- osrm_graph$osrm_path

  list(
    osrm_path = final_path,
    logs = logs_list
  )
}
