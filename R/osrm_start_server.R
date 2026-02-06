#' Start an OSRM MLD/CH server with `osrm-routed`
#'
#' @description
#' Launches an `osrm-routed` process pointing at a localized OSRM graph (either
#' `.osrm.mldgr` for MLD or `.osrm.hsgr` for CH/CoreCH).
#'
#' @details
#' The server's standard output and error streams are handled via temporary files
#' by default to prevent deadlocks in R's single-threaded environment. This ensures
#' reliable operation while preserving logs for debugging startup failures.
#'
#' To customize logging behavior, you can use the following approaches:
#' \itemize{
#'   \item **Default (Temp File):** Logs are written to a temporary file. This prevents
#'     deadlocks while keeping logs available for debugging.
#'
#'   \item **Verbose Mode:** Set `verbose = TRUE` to display logs directly in the
#'     R console. Note: This can cause deadlocks in tight loops if R is busy.
#'
#'   \item **Custom Log File:** Set the `osrm.server.log_file` option to redirect
#'     output to a specific file:
#'     `options(osrm.server.log_file = "path/to/osrm.log")`
#'
#'     Note: List specifications (e.g., `list(stdout = "...", stderr = "...")`)
#'     are deprecated and will fall back to the default temporary file behavior.
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
#' @param quiet Logical; when `TRUE`, suppresses package messages.
#' @param verbose Logical; when `TRUE`, routes server stdout and stderr to the R
#'   console for live debugging. Note: This can cause deadlocks in tight loops
#'   if R is busy. Defaults to `FALSE`, which writes logs to a temporary file.
#'
#' @return A `processx::process` object for the running server (also registered internally).
#' @examples
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   install_dir <- osrm_install(
#'     version = "latest",
#'     path_action = "session",
#'     quiet = TRUE
#'   )
#'
#'   # Build a graph then launch an OSRM server on a custom port
#'   pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#'   osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
#'   dir.create(osrm_dir, recursive = TRUE)
#'   tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#'   file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
#'
#'   graph <- osrm_prepare_graph(
#'     input_osm = tmp_pbf,
#'     overwrite = TRUE,
#'     threads = 1L,
#'     algorithm = "mld"
#'   )
#'
#'   server <- osrm_start_server(
#'     osrm_path = graph$osrm_job_artifact,
#'     port = 6000,
#'     threads = 1L
#'   )
#'
#'   # Later, stop the server again
#'   osrm_stop(server)
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
  quiet = FALSE,
  verbose = FALSE,
  echo_cmd = FALSE
) {
  # Dependencies
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop(
      "'processx' package is required for osrm_start_server()",
      call. = FALSE
    )
  }
  quiet <- isTRUE(quiet)
  verbose <- isTRUE(verbose)

  # Add this at the very beginning of the function body
  osrm_path <- get_osrm_path_from_input(osrm_path)

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

  # --- LOGGING CONFIGURATION ---
  # We prefer a temp file to prevent pipe deadlocks while keeping debug info.

  log_opt <- getOption("osrm.server.log_file")

  stdout_dest <- NULL
  stderr_dest <- NULL
  log_file_path <- NULL

  # Helper to validate path and create directory if needed
  prepare_log_path <- function(path) {
    if (
      is.null(path) ||
        !is.character(path) ||
        length(path) != 1 ||
        !nzchar(path) ||
        is.na(path)
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

  # Helper to read only the last n lines from a file (memory-efficient)
  read_last_n_lines <- function(file_path, n = 100) {
    # Get file size
    file_size <- file.size(file_path)
    if (is.na(file_size) || file_size == 0) {
      return(character())
    }

    # Estimate bytes to read: assume average 100 bytes per line
    bytes_per_line <- 100
    bytes_to_read <- min(file_size, n * bytes_per_line * 2)

    # Open file and seek to position
    con <- file(file_path, "rb")
    on.exit(close(con), add = TRUE)

    # Seek to estimated position (or beginning if file is small)
    seek(con, max(0, file_size - bytes_to_read))

    # Read remaining content
    raw_content <- readBin(con, "raw", n = bytes_to_read)
    content <- rawToChar(raw_content)

    # Split into lines and return last n
    lines <- strsplit(content, "\r?\n")[[1]]

    # Remove potential partial first line (incomplete due to seeking)
    if (file_size > bytes_to_read && length(lines) > 1) {
      lines <- lines[-1]
    }

    # Return last n lines (or all if fewer)
    tail(lines, n)
  }

  if (isTRUE(verbose)) {
    # Direct to console (developer mode)
    # Note: This CAN cause deadlocks in tight loops if R is busy!
    stdout_dest <- ""
    stderr_dest <- ""
  } else if (!is.null(log_opt) && is.character(log_opt)) {
    # User override via options - only character path supported
    log_path <- prepare_log_path(log_opt)
    if (!is.null(log_path)) {
      log_file_path <- log_path
      stdout_dest <- log_path
      stderr_dest <- log_path
      if (!quiet) {
        message("Redirecting server stdout and stderr to: ", log_path)
      }
    } else {
      # Invalid character path - fall back to temp file
      log_file_path <- tempfile(pattern = "osrm_", fileext = ".log")
      stdout_dest <- log_file_path
      stderr_dest <- log_file_path
    }
  } else if (is.list(log_opt)) {
    # List option is deprecated - silently fall back to temp file
    # (was causing deadlocks with incomplete configuration)
    log_file_path <- tempfile(pattern = "osrm_", fileext = ".log")
    stdout_dest <- log_file_path
    stderr_dest <- log_file_path
  } else {
    # DEFAULT: Write to a temp file.
    # This prevents deadlocks and allows post-mortem debugging.
    log_file_path <- tempfile(pattern = "osrm_", fileext = ".log")
    stdout_dest <- log_file_path
    stderr_dest <- log_file_path
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
  show_echo_cmd <- !quiet && isTRUE(echo_cmd)

  if (show_echo_cmd) {
    message(osrm_exec, " ", paste(shQuote(arguments), collapse = " "))
  }

  osrm_server <- processx::process$new(
    osrm_exec,
    args = arguments,
    echo_cmd = show_echo_cmd,
    stdout = stdout_dest,
    stderr = stderr_dest
  )

  # --- HEALTH CHECK & ERROR REPORTING ---

  # Allow a tiny grace period for the C++ binary to fail (e.g. port binding)
  Sys.sleep(0.1)

  alive_now <- FALSE
  err_msg <- NULL

  try(
    {
      alive_now <- osrm_server$is_alive()
      if (!alive_now) {
        status <- try(osrm_server$get_exit_status(), silent = TRUE)
        status <- if (inherits(status, "try-error")) NA_integer_ else status

        # Retrieve logs to explain WHY it failed
        log_content <- character()

        if (!is.null(log_file_path) && file.exists(log_file_path)) {
          # Read only the last ~100 lines to avoid loading large files
          log_content <- read_last_n_lines(log_file_path, n = 100)
        } else if (identical(stdout_dest, "|")) {
          # Fallback for pipes (unlikely with new default)
          log_content <- c(
            tryCatch(osrm_server$read_output_lines(), error = function(e) {
              character()
            }),
            tryCatch(osrm_server$read_error_lines(), error = function(e) {
              character()
            })
          )
        }

        # Format the error message
        err_msg <- paste0(
          "osrm-routed failed to start (exit status: ",
          status,
          ").\n"
        )

        if (length(log_content) > 0) {
          # Show the last 10 lines of the error log
          err_msg <- paste0(
            err_msg,
            "Last 10 log lines:\n",
            paste(utils::tail(log_content, 10), collapse = "\n")
          )
        } else {
          if (isTRUE(verbose) && is.null(log_file_path)) {
            err_msg <- paste0(
              err_msg,
              "Logs were sent to the R console and cannot be recovered. ",
              "Set a log file path to persist logs for debugging."
            )
          } else {
            err_msg <- paste0(
              err_msg,
              "No logs captured (check if 'verbose' or log file options are set)."
            )
          }
        }
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
      algorithm = algorithm,
      log = log_file_path
    ),
    silent = TRUE
  )

  # Attach log path as attribute for user access
  if (!is.null(log_file_path)) {
    attr(osrm_server, "log_path") <- log_file_path
  }

  osrm_server
}
