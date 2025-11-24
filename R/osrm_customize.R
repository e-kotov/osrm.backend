#' Customize OSRM Graph for Multi-Level Dijkstra (MLD)
#'
#' Run the `osrm-customize` tool to customize a partitioned OSRM graph for the MLD pipeline.
#' After running, a companion `<base>.osrm.mldgr` file must exist to confirm success.
#'
#' @param input_osrm A string. Path to a `.osrm.partition` file, the base path to the partitioned `.osrm` files (without extension),
#'   or a directory containing exactly one `.osrm.partition` file.
#' @param threads An integer. Number of threads to use; default `8` (osrm-customize's default).
#' @param verbosity A string. Log verbosity level passed to `-l/--verbosity`
#'   (one of `"NONE","ERROR","WARNING","INFO","DEBUG"`); default `"INFO"`.
#' @param segment_speed_file A string or `NULL`. Path to nodeA,nodeB,speed CSV; default `NULL`.
#' @param turn_penalty_file A string or `NULL`. Path to from_,to_,via_nodes,penalties CSV; default `NULL`.
#' @param edge_weight_updates_over_factor A numeric. Factor threshold for logging large weight updates; default `0`.
#' @param parse_conditionals_from_now A numeric. UTC timestamp from which to evaluate conditional turn restrictions; default `0`.
#' @param time_zone_file A string or `NULL`. GeoJSON file with time zone boundaries; default `NULL`.
#' @inheritParams osrm_prepare_graph
#'
#' @return A list with elements:
#' \describe{
#'   \item{osrm_path}{The normalized path to the customized `.osrm.mldgr` file. This can be passed over to [osrm_start_server()].}
#'   \item{logs}{The `processx::run` result object.}
#' }
#'
#' @export
osrm_customize <- function(
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
) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required for osrm_customize", call. = FALSE)
  }
  if (!is.character(input_osrm) || length(input_osrm) != 1) {
    stop(
      "'input_osrm' must be a single string (path to .osrm base)",
      call. = FALSE
    )
  }

  # Resolve input path (file or directory)
  input_osrm <- resolve_osrm_path(
    input_osrm,
    pattern = "\\.osrm\\.partition$",
    file_description = ".osrm.partition files",
    error_context = "Please check that you have run `osrm_partition` first."
  )

  if (!grepl("\\.partition$", input_osrm, ignore.case = TRUE)) {
    stop(
      "'input_osrm' must have extension .partition",
      call. = FALSE
    )
  }

  if (!file.exists(input_osrm)) {
    stop(
      "File does not exist: ",
      input_osrm,
      "\nPlease check that you have run `osrm_partition` first.",
      call. = FALSE
    )
  }

  osrm_path <- gsub("\\.partition$", "", input_osrm)

  verbosity <- match.arg(verbosity)
  arguments <- c(osrm_path, "-l", verbosity, "-t", as.character(threads))
  if (!is.null(segment_speed_file)) {
    arguments <- c(arguments, "--segment-speed-file", segment_speed_file)
  }
  if (!is.null(turn_penalty_file)) {
    arguments <- c(arguments, "--turn-penalty-file", turn_penalty_file)
  }
  if (!is.null(edge_weight_updates_over_factor)) {
    arguments <- c(
      arguments,
      "--edge-weight-updates-over-factor",
      as.character(edge_weight_updates_over_factor)
    )
  }
  if (!is.null(parse_conditionals_from_now)) {
    arguments <- c(
      arguments,
      "--parse-conditionals-from-now",
      as.character(parse_conditionals_from_now)
    )
  }
  if (!is.null(time_zone_file)) {
    arguments <- c(arguments, "--time-zone-file", time_zone_file)
  }

  # Determine final processx parameters
  show_echo <- !quiet && verbose
  show_spinner <- !quiet && spinner
  show_echo_cmd <- !quiet && echo_cmd

  logs <- processx::run(
    "osrm-customize",
    args = arguments,
    echo = show_echo,
    spinner = show_spinner,
    echo_cmd = show_echo_cmd
  )

  mld_file <- paste0(gsub("\\.osrm$", "", osrm_path), ".osrm.mldgr")
  if (!file.exists(mld_file)) {
    stop(
      "Customization did not produce MLD graph file: ",
      basename(mld_file),
      call. = FALSE
    )
  }

  list(
    osrm_path = normalizePath(mld_file),
    logs = logs
  )
}
