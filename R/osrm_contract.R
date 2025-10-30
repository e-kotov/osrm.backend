#' Contract OSRM Graph for Contraction Hierarchies (CH)
#'
#' Run the `osrm-contract` tool to contract an OSRM graph for the CH pipeline.
#' After running, a companion `<base>.osrm.hsgr` file must exist to confirm success.
#'
#' @param input_osrm A string. Base path to the `.osrm` files (without extension).
#' @param threads An integer. Number of threads to use; default `8`.
#' @param verbosity A string. Log verbosity level passed to `-l/--verbosity`
#'   (one of `"NONE","ERROR","WARNING","INFO","DEBUG"`); default `"INFO"`.
#' @param core A numeric. Percentage of the graph to contract [0..1]; default `1.0`.
#' @param segment_speed_file A string or `NULL`. Path to nodeA,nodeB,speed CSV; default `NULL`.
#' @param turn_penalty_file A string or `NULL`. Path to from_,to_,via_nodes,penalties CSV; default `NULL`.
#' @param edge_weight_updates_over_factor A numeric. Threshold for logging large weight updates; default `0`.
#' @param parse_conditionals_from_now A numeric. UTC timestamp for conditional restrictions; default `0`.
#' @param time_zone_file A string or `NULL`. GeoJSON file for time zone boundaries; default `NULL`.
#' @inheritParams osrm_prepare_graph
#'
#' @return A list with elements:
#' \describe{
#'   \item{osrm_path}{The normalized path to the input `.osrm` base (invisibly). This can be passed over to [osrm_start_server()].}
#'   \item{logs}{The `processx::run` result object.}
#' }
#'
#' @export
osrm_contract <- function(
  input_osrm,
  threads = 8L,
  verbosity = c("INFO", "NONE", "ERROR", "WARNING", "DEBUG"),
  core = 1.0,
  segment_speed_file = NULL,
  turn_penalty_file = NULL,
  edge_weight_updates_over_factor = 0,
  parse_conditionals_from_now = 0,
  time_zone_file = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required for osrm_contract", call. = FALSE)
  }
  if (!is.character(input_osrm) || length(input_osrm) != 1) {
    stop(
      "'input_osrm' must be a single string (path to .osrm base)",
      call. = FALSE
    )
  }

  if (!grepl("\\.timestamp$", input_osrm, ignore.case = TRUE)) {
    stop(
      "'input_osrm' must have extension .timestamp",
      call. = FALSE
    )
  }

  if (!file.exists(input_osrm)) {
    stop(
      stop(
        "File does not exist: ",
        input_osrm,
        "\n",
        "Please check that you have run `osrm_partition` first.",
        call. = FALSE
      )
    )
  }

  osrm_path <- gsub("\\.timestamp$", "", input_osrm)

  verbosity <- match.arg(verbosity)
  arguments <- c(
    osrm_path,
    "-l",
    verbosity,
    "-t",
    as.character(threads),
    "-k",
    as.character(core)
  )
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

  logs <- processx::run(
    "osrm-contract",
    args = arguments,
    echo = echo,
    spinner = spinner,
    echo_cmd = echo_cmd
  )

  hsgr_file <- paste0(gsub("\\.osrm$", "", input_osrm), ".osrm.hsgr")
  if (!file.exists(hsgr_file)) {
    stop(
      "Contracting did not produce CH hierarchy file: ",
      basename(hsgr_file),
      call. = FALSE
    )
  }

  invisible(list(
    osrm_path = normalizePath(input_osrm),
    logs = logs
  ))
}
