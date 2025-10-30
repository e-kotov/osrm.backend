#' Start an OSRM MLD/CH server with `osrm-routed`
#'
#' @description
#' Launches an `osrm-routed` process pointing at a localized OSRM graph (either `.osrm.mldgr` for MLD or `.osrm.hsgr` for CH/CoreCH).
#'
#' @param osrm_path Character(1). Path to the `.osrm.mldgr` or `.osrm.hsgr` file
#' @param version Logical; if `TRUE`, prints version and exits
#' @param help Logical; if `TRUE`, prints help and exits
#' @param verbosity Character; one of `"NONE","ERROR","WARNING","INFO","DEBUG"`
#' @param trial Logical or integer; if `TRUE` or >0, quits after initialization (default: `FALSE`)
#' @param ip Character; IP address to bind (default: `"0.0.0.0"`)
#' @param port Integer; TCP port to listen on (default: `5000`)
#' @param threads Integer; number of worker threads (default: `8`)
#' @param shared_memory Logical; load graph from shared memory (default: `FALSE`)
#' @param memory_file Character or NULL; DEPRECATED (behaves like `mmap`)
#' @param mmap Logical; memory-map data files (default: `FALSE`)
#' @param dataset_name Character or NULL; name of shared memory dataset
#' @param algorithm Character or NULL; one of `"CH","CoreCH","MLD"`. If `NULL` (default), auto-selected based on file extension
#' @param max_viaroute_size Integer (default: `500`)
#' @param max_trip_size Integer (default: `100`)
#' @param max_table_size Integer (default: `100`)
#' @param max_matching_size Integer (default: `100`)
#' @param max_nearest_size Integer (default: `100`)
#' @param max_alternatives Integer (default: `3`)
#' @param max_matching_radius Integer; use `-1` for unlimited (default: `-1`)
#' @inheritParams osrm_prepare_graph
#'
#' @return A `processx::process` object for the running server
#' @export
osrm_start_server <- function(
  osrm_path,
  version = FALSE,
  help = FALSE,
  verbosity = c("INFO", "ERROR", "WARNING", "NONE", "DEBUG"),
  trial = FALSE,
  ip = "0.0.0.0",
  port = 5001L,
  threads = 8L,
  shared_memory = FALSE,
  memory_file = NULL,
  mmap = FALSE,
  dataset_name = NULL,
  algorithm = NULL,
  max_viaroute_size = 500L,
  max_trip_size = 100L,
  max_table_size = 100L,
  max_matching_size = 100L,
  max_nearest_size = 100L,
  max_alternatives = 3L,
  max_matching_radius = -1L,
  echo_cmd = FALSE
) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required for osrm_start_server", call. = FALSE)
  }
  if (!is.character(osrm_path) || length(osrm_path) != 1) {
    stop(
      "'osrm_path' must be a single string pointing to .osrm.mldgr or .osrm.hsgr",
      call. = FALSE
    )
  }
  if (!file.exists(osrm_path)) {
    stop("File does not exist: ", osrm_path, call. = FALSE)
  }
  ext <- tolower(sub(".*\\.osrm\\.(.+)$", "\\1", osrm_path))
  if (!ext %in% c("mldgr", "hsgr")) {
    stop("'osrm_path' must end in .osrm.mldgr or .osrm.hsgr", call. = FALSE)
  }
  if (is.null(algorithm)) {
    algorithm <- if (ext == "mldgr") "MLD" else "CH"
  } else {
    algorithm <- match.arg(algorithm, c("CH", "CoreCH", "MLD"))
    if (ext == "mldgr" && algorithm != "MLD") {
      stop(
        "Algorithm must be 'MLD' when using an .osrm.mldgr file",
        call. = FALSE
      )
    }
    if (ext == "hsgr" && !(algorithm %in% c("CH", "CoreCH"))) {
      stop(
        "Algorithm must be 'CH' or 'CoreCH' when using an .osrm.hsgr file",
        call. = FALSE
      )
    }
  }
  prefix <- sub(
    "\\.osrm\\.(?:mldgr|hsgr)$",
    "\\.osrm",
    osrm_path,
    ignore.case = TRUE
  )

  # --- build arguments, omitting defaults (but always passing port) ---
  arguments <- character()

  # standalone flags
  if (version) arguments <- c(arguments, "-v")
  if (help) arguments <- c(arguments, "-h")

  # verbosity only if not default
  verbosity <- match.arg(verbosity)
  if (verbosity != "INFO") {
    arguments <- c(arguments, "-l", verbosity)
  }

  # trial only if requested
  if (!identical(trial, FALSE)) {
    val <- if (is.logical(trial) && trial) 1L else as.integer(trial)
    arguments <- c(arguments, "--trial", as.character(val))
  }

  # ip only if not default
  if (ip != "0.0.0.0") {
    arguments <- c(arguments, "-i", ip)
  }

  # **always** pass port
  arguments <- c(arguments, "-p", as.character(port))

  # threads only if not default
  if (threads != 8L) {
    arguments <- c(arguments, "-t", as.character(threads))
  }

  # other flags
  if (shared_memory) arguments <- c(arguments, "--shared-memory")
  if (!is.null(memory_file))
    arguments <- c(arguments, "--memory_file", memory_file)
  if (mmap) arguments <- c(arguments, "-m")
  if (!is.null(dataset_name))
    arguments <- c(arguments, "--dataset-name", dataset_name)

  # always pass algorithm
  arguments <- c(arguments, "-a", algorithm)

  # size-limits only if different from defaults
  if (max_viaroute_size != 500L)
    arguments <- c(
      arguments,
      "--max-viaroute-size",
      as.character(max_viaroute_size)
    )
  if (max_trip_size != 100L)
    arguments <- c(arguments, "--max-trip-size", as.character(max_trip_size))
  if (max_table_size != 100L)
    arguments <- c(arguments, "--max-table-size", as.character(max_table_size))
  if (max_matching_size != 100L)
    arguments <- c(
      arguments,
      "--max-matching-size",
      as.character(max_matching_size)
    )
  if (max_nearest_size != 100L)
    arguments <- c(
      arguments,
      "--max-nearest-size",
      as.character(max_nearest_size)
    )
  if (max_alternatives != 3L)
    arguments <- c(
      arguments,
      "--max-alternatives",
      as.character(max_alternatives)
    )
  if (max_matching_radius != -1L)
    arguments <- c(
      arguments,
      "--max-matching-radius",
      as.character(max_matching_radius)
    )

  # finally, add the graph prefix
  arguments <- c(arguments, prefix)

  # echo & launch
  if (isTRUE(echo_cmd)) {
    message("osrm-routed ", paste(shQuote(arguments), collapse = " "))
  }
  osrm_server <- processx::process$new(
    "osrm-routed",
    args = arguments,
    echo_cmd = echo_cmd
  )
  invisible(osrm_server)
}
