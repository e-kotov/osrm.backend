#' Partition OSRM Graph for Multi-Level Dijkstra (MLD)
#'
#' Run the `osrm-partition` tool to partition an OSRM graph for the MLD pipeline.
#' After running, a companion `<base>.osrm.partition` file must exist to confirm success.
#'
#' @param input_osrm A string. Base path to the `.osrm` files (without extension).
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
#'   \item{osrm_path}{The normalized path to the input `.osrm` base (invisibly).}
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
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
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

  if (!file.exists(input_osrm)) {
    stop(
      stop(
        "File does not exist: ",
        input_osrm,
        "\n",
        "Please check that you have run `osrm_extract` first.",
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

  logs <- processx::run(
    "osrm-partition",
    args = arguments,
    echo = echo,
    spinner = spinner,
    echo_cmd = echo_cmd
  )

  partition_file <- paste0(gsub("\\.osrm$", "", osrm_path), ".osrm.partition")
  if (!file.exists(partition_file)) {
    stop(
      "Partitioning did not produce partition file: ",
      basename(partition_file),
      call. = FALSE
    )
  }

  invisible(list(
    osrm_path = normalizePath(partition_file),
    logs = logs
  ))
}
