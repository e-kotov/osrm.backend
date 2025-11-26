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
#' @return An object of class \code{osrm_job} with the following elements:
#' \describe{
#'   \item{osrm_job_artifact}{The path to the customized `.osrm.mldgr` file.}
#'   \item{osrm_working_dir}{The directory containing all OSRM files.}
#'   \item{logs}{The \code{processx::run} result object.}
#' }
#'
#' @examples
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   install_dir <- osrm_install(
#'     version = "latest",
#'     path_action = "session",
#'     quiet = TRUE
#'   )
#'
#'   # Partition then customize a graph for the MLD pipeline
#'   pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#'   osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
#'   dir.create(osrm_dir, recursive = TRUE)
#'   tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#'   file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
#'   profile <- osrm_find_profile("car.lua")
#'
#'   extract_job <- osrm_extract(
#'     input_osm = tmp_pbf,
#'     profile = profile,
#'     overwrite = TRUE,
#'     threads = 1L
#'   )
#'   partition_job <- osrm_partition(extract_job, threads = 1L)
#'
#'   mld_graph <- osrm_customize(partition_job, threads = 1L, verbose = TRUE)
#'   mld_graph$osrm_job_artifact
#'
#'   osrm_uninstall(
#'     dest_dir = install_dir,
#'     clear_path = TRUE,
#'     force = TRUE,
#'     quiet = TRUE
#'   )
#'   unlink(osrm_dir, recursive = TRUE)
#' }
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

  # Extract previous logs if input is an osrm_job object
  input_logs <- if (inherits(input_osrm, "osrm_job")) {
    if (is.list(input_osrm$logs) && length(input_osrm$logs) > 0) {
      input_osrm$logs
    } else {
      list()
    }
  } else {
    list()
  }

  # Extract path from osrm_job object if needed
  input_osrm <- get_osrm_path_from_input(input_osrm)

  if (!is.character(input_osrm) || length(input_osrm) != 1) {
    stop(
      "'input_osrm' must be a single string (path to .osrm base)",
      call. = FALSE
    )
  }

  # Check if user is trying to use customize without partition
  if (grepl("\\.timestamp$", input_osrm, ignore.case = TRUE)) {
    stop(
      "`osrm_customize` requires a partitioned graph from `osrm_partition`.\n",
      "These are part of different pipelines:\n",
      "  - CH pipeline: extract -> contract\n",
      "  - MLD pipeline: extract -> partition -> customize\n",
      "After `osrm_extract`, use `osrm_partition` before `osrm_customize`.",
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

  # Check for algorithm conflicts (CH files in directory)
  base_name <- sub("\\.osrm$", "", basename(osrm_path))
  dir_path <- dirname(osrm_path)
  check_algorithm_conflict(dir_path, base_name, "mld", "customize")

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

  # Accumulate logs from previous stages
  accumulated_logs <- c(input_logs, list(customize = logs))

  as_osrm_job(
    osrm_job_artifact = normalizePath(mld_file),
    osrm_working_dir = dirname(normalizePath(mld_file)),
    logs = accumulated_logs
  )
}
