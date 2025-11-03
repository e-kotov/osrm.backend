#' Start an OSRM Server with Automatic Setup
#'
#' A high-level, "one-shot" function to start an OSRM server that automatically
#' handles OSRM installation and graph preparation. This is the recommended
#' function for most users to get a server running quickly with minimal steps.
#'
#' @details
#' This function is designed for convenience and automates the entire setup
#' process. By default, it is not verbose and only prints high-level status
#' messages.
#'
#' \enumerate{
#'   \item \strong{Check for OSRM Installation:} It first verifies if the `osrm-routed`
#'     executable is available in the system's `PATH`. If not, it automatically
#'     calls `osrm_install()` to download and install the latest stable version.
#'   \item \strong{Process Input Path and Prepare Graph:} The function intelligently
#'     handles the `path` argument to find or create the necessary graph files.
#'     If the graph files do not exist, it automatically runs `osrm_prepare_graph()`
#'     to build them, which may take some time.
#'   \item \strong{Start Server:} Once the graph files are located or prepared, it
#'     launches the `osrm-routed` server and prints a confirmation message
#'     with the server's PID and port.
#' }
#' For advanced users or for debugging, set `verbose = TRUE` to see the detailed
#' console output from the installation and graph preparation steps. For full
#' manual control, use the lower-level functions like `osrm_prepare_graph()` and
#' `osrm_start_server()` directly.
#'
#' @param path A string. Path to the input data. Can be one of:
#'   \itemize{
#'     \item A path to an OSM file (e.g., `/path/to/data.osm.pbf`).
#'     \item A path to a directory containing OSRM graph files or an OSM file.
#'     \item A direct path to a final graph file (`.osrm.mldgr` or `.osrm.hsgr`).
#'   }
#' @param algorithm A string specifying the routing algorithm to use for graph
#'   preparation, either `"mld"` (Multi-Level Dijkstra, default) or `"ch"`
#'   (Contraction Hierarchies). This is only used when `osrm_prepare_graph` is
#'   automatically called.
#' @param verbose A logical. If `FALSE` (default), suppresses detailed console
#'   output from backend commands. If `TRUE`, shows all output, which is useful
#'   for debugging.
#' @param ... Additional arguments passed on to `osrm_prepare_graph()` (e.g.,
#'   `overwrite = TRUE`) and `osrm_start_server()` (e.g., `port = 5001`).
#'
#' @return A `processx::process` object for the running server.
#' @export
#' @seealso [osrm_stop()], [osrm_start_server()] for manual server startup.
#' @examples
#' \dontrun{
#' # Get the path to the example OSM file included in the package
#' pbf_file <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#'
#' # Create a temporary directory to work in
#' temp_dir <- tempdir()
#' file.copy(pbf_file, temp_dir)
#' local_pbf <- file.path(temp_dir, "cur.osm.pbf")
#'
#' # Start the server with one command.
#' # It will quietly install OSRM and prepare the graph if needed.
#' osrm_process <- osrm_start(local_pbf)
#'
#' # Stop the server when done.
#' osrm_stop()
#'
#' # To see all backend logs during setup, use verbose = TRUE
#' osrm_process_verbose <- osrm_start(local_pbf, verbose = TRUE)
#' osrm_stop()
#' }
osrm_start <- function(
  path,
  algorithm = c("mld", "ch"),
  verbose = FALSE,
  ...
) {
  # Match algorithm argument
  algorithm <- match.arg(algorithm)

  # Capture additional arguments
  dot_args <- list(...)

  # Separate args for prepare_graph and start_server based on their formals
  prepare_args_names <- names(formals(osrm_prepare_graph))
  server_args_names <- names(formals(osrm_start_server))

  prepare_args <- dot_args[names(dot_args) %in% prepare_args_names]
  server_args <- dot_args[names(dot_args) %in% server_args_names]

  # 1. Check if OSRM is installed, if not, install it
  osrm_exec <- getOption("osrm.routed.exec", "osrm-routed")
  if (!nzchar(Sys.which(osrm_exec))) {
    message("OSRM backend not found. Installing latest version...")
    if (isTRUE(verbose)) {
      osrm_install(version = "latest", path_action = "session")
    } else {
      suppressMessages({
        osrm_install(version = "latest", path_action = "session")
      })
    }
    message("Installation complete.")
  }

  # 2. Resolve input path and prepare graph if necessary
  if (!file.exists(path)) {
    stop("Input path does not exist: ", path, call. = FALSE)
  }

  final_graph_path <- NULL

  if (dir.exists(path)) {
    # Input is a directory
    mld_files <- list.files(
      path,
      pattern = "\\.osrm\\.mldgr$",
      full.names = TRUE
    )
    ch_files <- list.files(path, pattern = "\\.osrm\\.hsgr$", full.names = TRUE)

    if (length(mld_files) > 0) {
      final_graph_path <- mld_files[1]
    } else if (length(ch_files) > 0) {
      final_graph_path <- ch_files[1]
    } else {
      osm_files <- list.files(
        path,
        pattern = "\\.osm\\.pbf$",
        full.names = TRUE
      )
      if (length(osm_files) == 0) {
        stop(
          "Directory contains no prepared OSRM graphs or .osm.pbf files.",
          call. = FALSE
        )
      }
      osm_input <- osm_files[1]

      message(
        "OSRM graph not found. Preparing graph from '",
        basename(osm_input),
        "', this may take a while..."
      )
      prepare_args$input_osm <- osm_input
      prepare_args$algorithm <- algorithm
      prepare_args$echo <- dot_args$echo %||% verbose
      prepare_args$spinner <- dot_args$spinner %||% !verbose # Show spinner if not verbose

      prepared_graph <- do.call(osrm_prepare_graph, prepare_args)
      final_graph_path <- prepared_graph$osrm_path
      message("Graph preparation complete.")
    }
  } else {
    # Input is a file
    if (grepl("\\.osm(\\.pbf|\\.bz2)?$", path, ignore.case = TRUE)) {
      base_name <- sub("\\.osm(\\.pbf|\\.bz2)?$", "", path, ignore.case = TRUE)
      mld_graph <- paste0(base_name, ".osrm.mldgr")
      ch_graph <- paste0(base_name, ".osrm.hsgr")

      if (file.exists(mld_graph)) {
        final_graph_path <- mld_graph
      } else if (file.exists(ch_graph)) {
        final_graph_path <- ch_graph
      } else {
        message(
          "OSRM graph not found. Preparing graph from '",
          basename(path),
          "', this may take a while..."
        )
        prepare_args$input_osm <- path
        prepare_args$algorithm <- algorithm
        prepare_args$echo <- dot_args$echo %||% verbose
        prepare_args$spinner <- dot_args$spinner %||% !verbose

        prepared_graph <- do.call(osrm_prepare_graph, prepare_args)
        final_graph_path <- prepared_graph$osrm_path
        message("Graph preparation complete.")
      }
    } else if (grepl("\\.osrm\\.(mldgr|hsgr)$", path, ignore.case = TRUE)) {
      final_graph_path <- path
    } else {
      stop(
        "Invalid input file type. `path` must be an OSM file, a prepared graph, or a directory.",
        call. = FALSE
      )
    }
  }

  if (is.null(final_graph_path)) {
    stop("Could not find or prepare a valid OSRM graph file.", call. = FALSE)
  }

  # 3. Start the server
  server_args$osrm_path <- final_graph_path
  message(
    "Starting OSRM server with graph '",
    basename(final_graph_path),
    "'..."
  )

  osrm_process <- do.call(osrm_start_server, server_args)

  # Extract info for the final confirmation message
  pid <- osrm_process$get_pid()
  port <- server_args$port %||% 5001L # Get port from args or use default

  message(sprintf(
    "OSRM server started successfully (pid %s, port %s).",
    pid,
    port
  ))

  return(osrm_process)
}
