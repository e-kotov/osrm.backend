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
#' @inheritParams osrm_install
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
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   install_dir <- osrm_install(
#'     version = "latest",
#'     path_action = "session",
#'     quiet = TRUE
#'   )
#'
#'   # copy example OSM PBF into a temporary workspace to avoid polluting pkg data
#'   pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#'   osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
#'   dir.create(osrm_dir, recursive = TRUE)
#'   local_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#'   file.copy(from = pbf_path, to = local_pbf, overwrite = TRUE)
#'
#'   # Start the server with one command.
#'   # It will quietly install OSRM and prepare the graph if needed.
#'   osrm_process <- osrm_start(local_pbf)
#'
#'   # Stop the server when done.
#'   osrm_stop()
#'
#'   # To see all backend logs during setup, use verbose = TRUE
#'   osrm_process_verbose <- osrm_start(local_pbf, verbose = TRUE)
#'   osrm_stop()
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
osrm_start <- function(
  path,
  algorithm = c("mld", "ch"),
  force_rebuild = FALSE,
  show_progress = TRUE,
  quiet = FALSE,
  verbose = FALSE,
  ...
) {
  # Match algorithm argument
  algorithm <- match.arg(algorithm)
  quiet <- isTRUE(quiet)
  verbose <- isTRUE(verbose)
  force_rebuild <- isTRUE(force_rebuild)
  show_progress <- isTRUE(show_progress)

  # Capture additional arguments
  dot_args <- list(...)

  # Separate args for prepare_graph and start_server based on their formals
  # We use match.arg or similar logic to route them correctly
  prepare_formals <- names(formals(osrm_prepare_graph))
  server_formals <- names(formals(osrm_start_server))
  
  # Arguments that should go to prepare_graph
  # We perform a strict check to avoiding passing server args to build
  build_args <- dot_args[names(dot_args) %in% prepare_formals]
  
  # Arguments that should go to start_server
  server_args <- dot_args[names(dot_args) %in% server_formals]
  
  # Common arguments handling: threads, verbosity might be in both
  # If 'threads' is passed in ..., it goes to both via the above logic
  
  # 1. Check if OSRM is installed, if not, install it
  osrm_exec <- getOption("osrm.routed.exec", "osrm-routed")
  if (!nzchar(Sys.which(osrm_exec))) {
    if (!quiet) {
      message("OSRM backend not found. Installing latest version...")
    }
    osrm_install(version = "latest", path_action = "session", quiet = quiet)
    if (!quiet) message("Installation complete.")
  }

  # 2. Resolve input path and prepare graph if necessary
  if (!file.exists(path)) {
    stop("Input path does not exist: ", path, call. = FALSE)
  }

  final_graph_path <- NULL
  original_osm_path <- NULL
  should_build <- FALSE
  
  # --- LOGIC TO DETERMINE IF WE NEED TO BUILD ---
  
  # Case A: Input is a directory
  if (dir.exists(path)) {
    # Check for existing graphs
    suffix <- if (algorithm == "mld") "\\.osrm\\.mldgr$" else "\\.osrm\\.hsgr$"
    existing_graphs <- list.files(path, pattern = suffix, full.names = TRUE)
    
    if (length(existing_graphs) > 0 && !force_rebuild) {
      final_graph_path <- existing_graphs[1]
    } else {
      # Need to build. Find source PBF.
      osm_files <- list.files(path, pattern = "\\.osm(\\.pbf|\\.bz2)?$", full.names = TRUE)
      if (length(osm_files) == 0) {
        if (force_rebuild) stop("Force rebuild requested but no source OSM file found in directory.", call. = FALSE)
        stop("Directory contains no prepared OSRM graphs or source OSM files.", call. = FALSE)
      }
      original_osm_path <- osm_files[1]
      should_build <- TRUE
    }
  } 
  # Case B: Input is a source file (.osm.pbf, .osm)
  else if (grepl("\\.osm(\\.pbf|\\.bz2)?$", path, ignore.case = TRUE)) {
    original_osm_path <- path
    base_name <- sub("\\.osm(\\.pbf|\\.bz2)?$", "", path, ignore.case = TRUE)
    suffix <- if (algorithm == "mld") ".osrm.mldgr" else ".osrm.hsgr"
    expected_graph <- paste0(base_name, suffix)
    
    if (file.exists(expected_graph) && !force_rebuild) {
      final_graph_path <- expected_graph
    } else {
      should_build <- TRUE
    }
  }
  # Case C: Input is already a graph file
  else if (grepl("\\.osrm\\.(mldgr|hsgr)$", path, ignore.case = TRUE)) {
    if (force_rebuild) {
       stop("Cannot force rebuild when input 'path' is already a compiled graph file. Point to the source .osm.pbf instead.", call. = FALSE)
    }
    final_graph_path <- path
    # Try to find original OSM for tracking if possible (heuristic)
    heuristic_osm <- sub("\\.osrm\\.(mldgr|hsgr)$", ".osm.pbf", path)
    if (file.exists(heuristic_osm)) original_osm_path <- heuristic_osm
  } else {
    stop("Invalid input path. Must be a directory, OSM source file, or OSRM graph.", call. = FALSE)
  }

  # --- PERFORM BUILD IF NEEDED ---
  
  if (should_build) {
    if (!quiet) {
      msg <- if (force_rebuild) "Rebuilding OSRM graph..." else "OSRM graph not found. Preparing..."
      message(msg, " This may take a while.")
    }
    
    # Construct arguments for osrm_prepare_graph
    # We enforce known args + user provided build_args
    build_call_args <- utils::modifyList(
      build_args,
      list(
        input_osm = original_osm_path,
        algorithm = algorithm,
        quiet = quiet,
        verbose = verbose,
        overwrite = force_rebuild || isTRUE(build_args$overwrite) # Ensure overwrite if forced
      )
    )
    
    # Run build
    prepared_graph <- do.call(osrm_prepare_graph, build_call_args)
    final_graph_path <- prepared_graph$osrm_job_artifact
    
    if (!quiet) message("Graph preparation complete.")
  }

  if (is.null(final_graph_path)) {
    stop("Could not find or prepare a valid OSRM graph file.", call. = FALSE)
  }

  # 3. Start the server
  # Construct arguments for osrm_start_server
  server_call_args <- utils::modifyList(
    server_args,
    list(
      osrm_path = final_graph_path,
      quiet = quiet,
      verbose = verbose,
      input_osm = original_osm_path
    )
  )
  
  # Only pass algorithm if explicitly provided or inferred? 
  # osrm_start_server usually auto-detects from extension, but we can pass it to be safe if mld/ch match
  if (is.null(server_call_args$algorithm)) {
     server_call_args$algorithm <- if (grepl("mldgr$", final_graph_path)) "MLD" else "CH"
  }

  if (!quiet) {
    message(
      "Starting OSRM server with graph '",
      basename(final_graph_path),
      "'..."
    )
  }

  osrm_process <- do.call(osrm_start_server, server_call_args)

  # Extract info for the final confirmation message
  pid <- osrm_process$get_pid()
  # Port might be in server_args or default
  port <- server_call_args$port %||% 5001L

  if (!quiet) {
    message(sprintf(
      "OSRM server started successfully (pid %s, port %s).",
      pid,
      port
    ))
  }

  return(osrm_process)
}
