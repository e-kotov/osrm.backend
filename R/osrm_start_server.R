#' Start an OSRM MLD/CH server with `osrm-routed`
#'
#' @description
#' Launches an `osrm-routed` process pointing at a localized OSRM graph (either
#' `.osrm.mldgr` for MLD or `.osrm.hsgr` for CH/CoreCH).
#'
#' @details
#' The server's standard output and error streams can be controlled via R options
#' for non-interactive use or persistent logging. By default, they are captured
#' as pipes, which allows for reading output directly within the R session
#' (e.g., via `osrm_server$read_output_lines()`).
#'
#' To redirect the server's output to one or more files, you can set the
#' `osrm.server.log_file` R option before calling this function:
#' \itemize{
#'   \item **Combined Log:** To send both `stdout` and `stderr` to a single file,
#'     provide a file path:
#'     `options(osrm.server.log_file = "path/to/osrm.log")`
#'
#'   \item **Separate Logs:** To send `stdout` and `stderr` to separate files,
#'     provide a named list:
#'     `options(osrm.server.log_file = list(stdout = "out.log", stderr = "err.log"))`
#'
#'   \item **Default Behavior:** To restore the default behavior of using pipes,
#'     set the option to `NULL`:
#'     `options(osrm.server.log_file = NULL)`
#' }
#'
#' You can override the `osrm-routed` executable via
#' `options(osrm.routed.exec = "/full/path/to/osrm-routed")`.
#'
#' @param osrm_path Character(1). Path to the `.osrm.mldgr` or `.osrm.hsgr` file
#' @param version Logical; if `TRUE`, prints version and exits
#' @param help Logical; if `TRUE`, prints help and exits
#' @param verbosity Character; one of `"NONE","ERROR","WARNING","INFO","DEBUG"`
#' @param trial Logical or integer; if `TRUE` or >0, quits after initialization (default: `FALSE`)
#' @param ip Character; IP address to bind (default: `"0.0.0.0"`)
#' @param port Integer; TCP port to listen on (default: `5001`)
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
#' @param echo_cmd Logical; echo command line to console before launch (default: `FALSE`)
#'
#' @return A `processx::process` object for the running server (also registered internally)
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
  # Dependencies
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop(
      "'processx' package is required for osrm_start_server()",
      call. = FALSE
    )
  }

  # Validate inputs
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

  # Algorithm selection
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

  # Prefix (graph base path without the .mldgr/.hsgr suffix)
  prefix <- sub(
    "\\.osrm\\.(?:mldgr|hsgr)$",
    "\\.osrm",
    osrm_path,
    ignore.case = TRUE
  )

  # Build CLI arguments (omit defaults where possible; always pass port & algorithm)
  arguments <- character()

  # Standalone flags
  if (isTRUE(version)) {
    arguments <- c(arguments, "-v")
  }
  if (isTRUE(help)) {
    arguments <- c(arguments, "-h")
  }

  # Verbosity
  verbosity <- match.arg(verbosity)
  if (verbosity != "INFO") {
    arguments <- c(arguments, "-l", verbosity)
  }

  # Trial
  if (!identical(trial, FALSE)) {
    val <- if (is.logical(trial) && trial) 1L else as.integer(trial)
    arguments <- c(arguments, "--trial", as.character(val))
  }

  # IP (only if not default)
  if (!identical(ip, "0.0.0.0")) {
    arguments <- c(arguments, "-i", ip)
  }

  # Always pass port
  arguments <- c(arguments, "-p", as.character(as.integer(port)))

  # Threads (only if not default)
  if (!identical(as.integer(threads), 8L)) {
    arguments <- c(arguments, "-t", as.character(as.integer(threads)))
  }

  # Other flags
  if (isTRUE(shared_memory)) {
    arguments <- c(arguments, "--shared-memory")
  }
  if (!is.null(memory_file)) {
    arguments <- c(arguments, "--memory_file", memory_file)
  }
  if (isTRUE(mmap)) {
    arguments <- c(arguments, "-m")
  }
  if (!is.null(dataset_name)) {
    arguments <- c(arguments, "--dataset-name", dataset_name)
  }

  # Always pass algorithm
  arguments <- c(arguments, "-a", algorithm)

  # Size limits (only if not defaults)
  if (!identical(as.integer(max_viaroute_size), 500L)) {
    arguments <- c(
      arguments,
      "--max-viaroute-size",
      as.character(as.integer(max_viaroute_size))
    )
  }
  if (!identical(as.integer(max_trip_size), 100L)) {
    arguments <- c(
      arguments,
      "--max-trip-size",
      as.character(as.integer(max_trip_size))
    )
  }
  if (!identical(as.integer(max_table_size), 100L)) {
    arguments <- c(
      arguments,
      "--max-table-size",
      as.character(as.integer(max_table_size))
    )
  }
  if (!identical(as.integer(max_matching_size), 100L)) {
    arguments <- c(
      arguments,
      "--max-matching-size",
      as.character(as.integer(max_matching_size))
    )
  }
  if (!identical(as.integer(max_nearest_size), 100L)) {
    arguments <- c(
      arguments,
      "--max-nearest-size",
      as.character(as.integer(max_nearest_size))
    )
  }
  if (!identical(as.integer(max_alternatives), 3L)) {
    arguments <- c(
      arguments,
      "--max-alternatives",
      as.character(as.integer(max_alternatives))
    )
  }
  if (!identical(as.integer(max_matching_radius), -1L)) {
    arguments <- c(
      arguments,
      "--max-matching-radius",
      as.character(as.integer(max_matching_radius))
    )
  }

  # Finally, add the graph prefix
  arguments <- c(arguments, prefix)

  # --- Check R options for log file redirection ---
  stdout_dest <- "|"
  stderr_dest <- "|"
  log_opt <- getOption("osrm.server.log_file")

  # Helper to prepare a path: normalize it and ensure its directory exists
  prepare_log_path <- function(path) {
    if (
      is.null(path) || !is.character(path) || length(path) != 1 || !nzchar(path)
    ) {
      return(NULL)
    }
    abs_path <- normalizePath(path, mustWork = FALSE)
    log_dir <- dirname(abs_path)
    if (!dir.exists(log_dir)) {
      dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
    }
    abs_path
  }

  if (!is.null(log_opt)) {
    if (is.character(log_opt)) {
      # Option 1: A single path for both stdout and stderr
      log_path <- prepare_log_path(log_opt)
      if (!is.null(log_path)) {
        stdout_dest <- log_path
        stderr_dest <- log_path
        message("Redirecting server stdout and stderr to: ", log_path)
      }
    } else if (is.list(log_opt)) {
      # Option 2: Separate stdout and stderr
      stdout_path <- prepare_log_path(log_opt$stdout)
      if (!is.null(stdout_path)) {
        stdout_dest <- stdout_path
        message("Redirecting server stdout to: ", stdout_path)
      }
      stderr_path <- prepare_log_path(log_opt$stderr)
      if (!is.null(stderr_path)) {
        stderr_dest <- stderr_path
        message("Redirecting server stderr to: ", stderr_path)
      }
    }
  }

  # Resolve osrm-routed executable (optionally override via option)
  osrm_exec <- getOption("osrm.routed.exec", "osrm-routed")
  if (!nzchar(Sys.which(osrm_exec))) {
    stop(
      "Cannot find '",
      osrm_exec,
      "' on PATH. Install OSRM or set options(osrm.routed.exec = '/path/to/osrm-routed').",
      call. = FALSE
    )
  }

  # Echo & launch
  if (isTRUE(echo_cmd)) {
    message(osrm_exec, " ", paste(shQuote(arguments), collapse = " "))
  }

  osrm_server <- processx::process$new(
    osrm_exec,
    args = arguments,
    echo_cmd = echo_cmd,
    stdout = stdout_dest,
    stderr = stderr_dest
  )

  # If the process died immediately, provide a helpful error (only safe to read pipes)
  alive_now <- FALSE
  err_msg <- NULL
  try(
    {
      alive_now <- osrm_server$is_alive()
      if (!alive_now) {
        status <- try(osrm_server$get_exit_status(), silent = TRUE)
        status <- if (inherits(status, "try-error")) NA_integer_ else status
        # Only try to read from pipes if they are pipes
        out_lines <- if (identical(stdout_dest, "|")) {
          tryCatch(osrm_server$read_output_lines(), error = function(e) {
            character()
          })
        } else {
          character()
        }
        err_lines <- if (identical(stderr_dest, "|")) {
          tryCatch(osrm_server$read_error_lines(), error = function(e) {
            character()
          })
        } else {
          character()
        }
        err_msg <- paste0(
          "osrm-routed failed to start (exit status: ",
          status,
          ").\n",
          if (length(out_lines)) {
            paste0("[stdout]\n", paste(out_lines, collapse = "\n"), "\n")
          } else {
            ""
          },
          if (length(err_lines)) {
            paste0("[stderr]\n", paste(err_lines, collapse = "\n"), "\n")
          } else {
            ""
          }
        )
      }
    },
    silent = TRUE
  )

  if (!isTRUE(alive_now)) {
    # Ensure we don't leak a defunct process object
    try(osrm_server$kill(), silent = TRUE)
    if (!is.null(err_msg)) {
      stop(err_msg, call. = FALSE)
    }
    stop("osrm-routed failed to start.", call. = FALSE)
  }

  # --- Register the process for later management (stop by id/port/pid across session) ---
  # Best-effort: ignore errors if registry is unavailable for any reason.
  try(
    .osrm_register(
      osrm_server,
      port = port,
      prefix = prefix,
      algorithm = algorithm
    ),
    silent = TRUE
  )

  invisible(osrm_server)
}
