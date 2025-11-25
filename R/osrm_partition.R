#' Partition OSRM Graph for Multi-Level Dijkstra (MLD)
#'
#' Run the `osrm-partition` tool to partition an OSRM graph for the MLD pipeline.
#' After running, a companion `<base>.osrm.partition` file must exist to confirm success.
#'
#' @param input_osrm A string. Path to a `.osrm.timestamp` file, the base path to the `.osrm` files (without extension),
#'   or a directory containing exactly one `.osrm.timestamp` file.
#' @param threads An integer. Number of threads to use; default `8` (osrm-partition's default).
#' @param verbosity A string. Log verbosity level passed to `-l/--verbosity`
#'   (one of `"NONE","ERROR","WARNING","INFO","DEBUG"`); default `"INFO"`.
#' @param balance A numeric. Balance for left and right side in single bisection; default `1.2`.
#' @param boundary A numeric. Percentage of embedded nodes to contract as sources and sinks; default `0.25`.
#' @param optimizing_cuts An integer. Number of cuts to use for optimizing a single bisection; default `10`.
#' @param small_component_size An integer. Size threshold for small components; default `1000`.
#' @param max_cell_sizes A numeric vector. Maximum cell sizes starting from level 1; default `c(128,4096,65536,2097152)`.
#' @inheritParams osrm_prepare_graph
#'
#' @return An object of class \code{osrm_job} with the following elements:
#' \describe{
#'   \item{osrm_job_artifact}{The path to the partitioned `.osrm.partition` file.}
#'   \item{osrm_working_dir}{The directory containing all OSRM files.}
#'   \item{logs}{The \code{processx::run} result object.}
#' }
#'
#' @export
osrm_partition <- function(
  input_osrm,
  threads = 8L,
  verbosity = c("INFO", "NONE", "ERROR", "WARNING", "DEBUG"),
  balance = 1.2,
  boundary = 0.25,
  optimizing_cuts = 10L,
  small_component_size = 1000L,
  max_cell_sizes = c(128, 4096, 65536, 2097152),
  quiet = FALSE,
  verbose = FALSE,
  spinner = TRUE,
  echo_cmd = FALSE
) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required for osrm_partition", call. = FALSE)
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

  # Resolve input path (file or directory)
  input_osrm <- resolve_osrm_path(
    input_osrm,
    pattern = "\\.osrm\\.timestamp$",
    file_description = ".osrm.timestamp files",
    error_context = "Please check that you have run `osrm_extract` first."
  )

  if (!file.exists(input_osrm)) {
    stop(
      "File does not exist: ",
      input_osrm,
      "\nPlease check that you have run `osrm_extract` first.",
      call. = FALSE
    )
  }

  osrm_path <- gsub("\\.timestamp$", "", input_osrm)

  # Check for algorithm conflicts (CH files in directory)
  base_name <- sub("\\.osrm$", "", basename(osrm_path))
  dir_path <- dirname(osrm_path)
  check_algorithm_conflict(dir_path, base_name, "mld", "partition")

  verbosity <- match.arg(verbosity)
  arguments <- c(
    osrm_path,
    "-l",
    verbosity,
    "-t",
    as.character(threads),
    "--balance",
    as.character(balance),
    "--boundary",
    as.character(boundary),
    "--optimizing-cuts",
    as.character(optimizing_cuts),
    "--small-component-size",
    as.character(small_component_size),
    "--max-cell-sizes",
    paste(max_cell_sizes, collapse = ",")
  )

  # Determine final processx parameters
  show_echo <- !quiet && verbose
  show_spinner <- !quiet && spinner
  show_echo_cmd <- !quiet && echo_cmd

  logs <- processx::run(
    "osrm-partition",
    args = arguments,
    echo = show_echo,
    spinner = show_spinner,
    echo_cmd = show_echo_cmd
  )

  partition_file <- paste0(gsub("\\.osrm$", "", osrm_path), ".osrm.partition")
  if (!file.exists(partition_file)) {
    stop(
      "Partitioning did not produce partition file: ",
      basename(partition_file),
      call. = FALSE
    )
  }

  # Accumulate logs from previous stages
  accumulated_logs <- c(input_logs, list(partition = logs))

  as_osrm_job(
    osrm_job_artifact = normalizePath(partition_file),
    osrm_working_dir = dirname(normalizePath(partition_file)),
    logs = accumulated_logs
  )
}
