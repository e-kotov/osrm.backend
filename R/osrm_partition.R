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
#' @return A list with elements:
#' \describe{
#'   \item{osrm_path}{The normalized path to the partitioned `.osrm.partition` file.}
#'   \item{logs}{The `processx::run` result object.}
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
  if (!is.character(input_osrm) || length(input_osrm) != 1) {
    stop(
      "'input_osrm' must be a single string (path to .osrm base)",
      call. = FALSE
    )
  }

  # normalize input
  input_osrm <- normalizePath(input_osrm, mustWork = TRUE)

  # if input_osrm is a directory, search for .osrm.timestamp files
  if (dir.exists(input_osrm)) {
    osrm_files <- list.files(
      input_osrm,
      pattern = "\\.osrm\\.timestamp$",
      ignore.case = TRUE,
      full.names = TRUE
    )

    if (length(osrm_files) == 0) {
      stop(
        "No .osrm.timestamp files found in directory: ",
        input_osrm,
        "\nPlease check that you have run `osrm_extract` first.",
        call. = FALSE
      )
    } else if (length(osrm_files) > 1) {
      stop(
        "Multiple .osrm.timestamp files found in directory: ",
        input_osrm,
        "\n  Files: ",
        paste(basename(osrm_files), collapse = ", "),
        "\n  Please specify a single file path instead of a directory.",
        call. = FALSE
      )
    }

    input_osrm <- osrm_files[1]
  }

  if (!file.exists(input_osrm)) {
    stop(
      "File does not exist: ",
      input_osrm,
      "\nPlease check that you have run `osrm_extract` first.",
      call. = FALSE
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

  list(
    osrm_path = normalizePath(partition_file),
    logs = logs
  )
}
