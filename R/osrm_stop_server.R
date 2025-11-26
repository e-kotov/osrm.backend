# R/osrm_stop_server.R

#' List OSRM servers started via this package
#'
#' Returns a snapshot of servers registered by [osrm_start_server()] or [osrm_start()].
#' You can stop one by passing its `id`, `port`, or `pid` to [osrm_stop()].
#'
#' @return A data.frame with columns:
#'   `id`, `pid`, `port`, `algorithm`, `started_at`, `alive`, `has_handle`.
#' @examples
#' \dontrun{
#' # copy example OSM PBF into a temporary workspace to avoid polluting pkg data
#' pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#' osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
#' dir.create(osrm_dir, recursive = TRUE)
#' tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#' file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
#' graph <- osrm_prepare_graph(tmp_pbf, overwrite = TRUE, threads = 1L)
#'
#' srv <- osrm_start_server(graph$osrm_job_artifact, port = 6000, threads = 1L)
#' osrm_servers()
#' osrm_stop(srv)
#'
#' unlink(osrm_dir, recursive = TRUE)
#' }
#' @export
osrm_servers <- function() {
  reg <- .osrm_state$registry
  if (!length(reg)) {
    out <- data.frame(
      id = character(),
      pid = integer(),
      port = integer(),
      algorithm = character(),
      started_at = as.POSIXct(character()),
      alive = logical(),
      has_handle = logical(),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  alive_vec <- logical(length(reg))
  handle_vec <- logical(length(reg))
  for (i in seq_along(reg)) {
    e <- reg[[i]]
    if (!is.null(e$proc) && inherits(e$proc, "process")) {
      handle_vec[i] <- TRUE
      alive_vec[i] <- tryCatch(e$proc$is_alive(), error = function(...) FALSE)
    } else {
      handle_vec[i] <- FALSE
      alive_vec[i] <- .osrm_pid_is_running(e$pid)
    }
  }

  out <- data.frame(
    id = vapply(reg, `[[`, "", "id"),
    pid = as.integer(vapply(reg, `[[`, 0L, "pid")),
    port = as.integer(vapply(reg, `[[`, 0L, "port")),
    algorithm = vapply(reg, `[[`, "", "algorithm"),
    started_at = as.POSIXct(vapply(reg, `[[`, "", "started_at")),
    alive = alive_vec,
    has_handle = handle_vec,
    stringsAsFactors = FALSE
  )
  out
}

#' Stop an OSRM Server
#'
#' Terminates an `osrm-routed` process launched by `osrm_start()` or
#' `osrm_start_server()`.
#'
#' @details
#' This function provides a flexible way to stop a running OSRM process. If no
#' arguments are specified, it defaults to stopping the most recently started
#' server that is still alive.
#'
#' You can also stop a specific server by providing:
#' \itemize{
#'   \item The `processx::process` object returned by `osrm_start()` or `osrm_start_server()`.
#'   \item The server's `id`, `port`, or `pid` (use `osrm_servers()` to find these).
#' }
#'
#' @param server Optional `processx::process` object returned by `osrm_start_server()`.
#' @param id Optional character id from `osrm_servers()`.
#' @param port Optional integer TCP port.
#' @param pid Optional integer process id.
#' @param wait Integer milliseconds to wait for clean shutdown (default `1000`).
#' @param quiet Logical; suppress messages (default `FALSE`).
#'
#' @return A list with fields `id`, `pid`, `port`, `stopped` (logical).
#' @export
#' @examples
#' \dontrun{
#' # copy example OSM PBF into a temporary workspace to avoid polluting pkg data
#' pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#' osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
#' dir.create(osrm_dir, recursive = TRUE)
#' tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#' file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
#' graph <- osrm_prepare_graph(tmp_pbf, overwrite = TRUE, threads = 1L)
#'
#' srv <- osrm_start_server(graph$osrm_job_artifact, port = 6000, threads = 1L)
#'
#' # Stop by passing the process object
#' osrm_stop(srv)
#'
#' # Or stop by port after the process is registered
#' osrm_stop(port = 6000)
#'
#' unlink(osrm_dir, recursive = TRUE)
#' }
#' @seealso [osrm_start()], [osrm_servers()], [osrm_stop_all()]
osrm_stop <- function(
  server = NULL,
  id = NULL,
  port = NULL,
  pid = NULL,
  wait = 1000L,
  quiet = FALSE
) {
  quiet <- isTRUE(quiet)
  # Case 1: user supplied a process object directly
  if (!is.null(server)) {
    if (!inherits(server, "process")) {
      stop("'server' must be a processx::process object", call. = FALSE)
    }
    targ_pid <- suppressWarnings(try(server$get_pid(), silent = TRUE))
    if (!inherits(targ_pid, "try-error")) {
      # Try to find and deregister by pid from the internal registry
      reg <- .osrm_state$registry
      hit <- names(reg)[vapply(
        reg,
        function(e) identical(e$pid, targ_pid),
        logical(1)
      )]
      if (server$is_alive()) {
        try(server$kill(), silent = TRUE)
        try(server$wait(as.integer(wait)), silent = TRUE)
      }
      if (length(hit)) {
        try(.osrm_deregister(hit[[1]]), silent = TRUE)
      }
      if (!quiet) {
        message("Stopped OSRM server (pid ", targ_pid, ").")
      }
      return(list(
        id = if (length(hit)) hit[[1]] else NA_character_,
        pid = as.integer(targ_pid),
        port = NA_integer_,
        stopped = TRUE
      ))
    } else {
      # Could not read pid; best effort kill
      try(server$kill(), silent = TRUE)
      try(server$wait(as.integer(wait)), silent = TRUE)
      if (!quiet) {
        message("Stopped OSRM server (unknown pid).")
      }
      return(list(
        id = NA_character_,
        pid = NA_integer_,
        port = NA_integer_,
        stopped = TRUE
      ))
    }
  }

  # Case 2: selection via registry (id, port, pid, or default)
  reg <- .osrm_state$registry
  if (!length(reg)) {
    if (!quiet) {
      message("No registered OSRM servers.")
    }
    return(list(
      id = NA_character_,
      pid = NA_integer_,
      port = NA_integer_,
      stopped = FALSE
    ))
  }

  pick_index <- function() {
    if (!is.null(id)) {
      which(names(reg) == id)[1]
    } else if (!is.null(port)) {
      hits <- which(vapply(
        reg,
        function(e) identical(e$port, as.integer(port)),
        logical(1)
      ))
      if (length(hits)) utils::tail(hits, 1) else integer()
    } else if (!is.null(pid)) {
      hits <- which(vapply(
        reg,
        function(e) identical(e$pid, as.integer(pid)),
        logical(1)
      ))
      if (length(hits)) utils::tail(hits, 1) else integer()
    } else {
      # Default: most recently started alive server
      alive <- vapply(
        reg,
        function(e) {
          if (!is.null(e$proc) && inherits(e$proc, "process")) {
            tryCatch(e$proc$is_alive(), error = function(...) FALSE)
          } else {
            .osrm_pid_is_running(e$pid)
          }
        },
        logical(1)
      )
      utils::tail(which(alive), 1)
    }
  }

  idx <- pick_index()
  if (!length(idx) || is.na(idx) || idx < 1) {
    # If a specific selector was used, error. If default, it's just empty.
    if (!is.null(id) || !is.null(port) || !is.null(pid)) {
      stop(
        "Could not identify a server to stop with the specified criteria.",
        call. = FALSE
      )
    }
    if (!quiet) {
      message("No running OSRM servers to stop.")
    }
    return(list(stopped = FALSE))
  }

  entry <- reg[[idx]]
  stopped <- FALSE

  # Prefer to use the live process handle if it exists in the session
  if (!is.null(entry$proc) && inherits(entry$proc, "process")) {
    if (tryCatch(entry$proc$is_alive(), error = function(...) FALSE)) {
      try(entry$proc$kill(), silent = TRUE)
      try(entry$proc$wait(as.integer(wait)), silent = TRUE)
      stopped <- TRUE
    } else {
      # Already dead; treat as stopped
      stopped <- TRUE
    }
  } else if (!is.null(entry$pid)) {
    # Fallback to killing by PID if no process handle
    if (.osrm_pid_is_running(entry$pid)) {
      .osrm_kill_pid(entry$pid)
      stopped <- TRUE
    } else {
      stopped <- TRUE
    }
  }

  # Deregister regardless to keep the registry clean
  try(.osrm_deregister(entry$id), silent = TRUE)

  if (!quiet) {
    msg <- sprintf(
      "Stopped OSRM server id=%s (pid %s, port %s).",
      entry$id,
      as.character(entry$pid %||% "NA"),
      as.character(entry$port %||% "NA")
    )
    message(msg)
  }

  list(
    id = entry$id,
    pid = as.integer(entry$pid %||% NA_integer_),
    port = as.integer(entry$port %||% NA_integer_),
    stopped = isTRUE(stopped)
  )
}

#' Stop all running OSRM servers started via this package
#'
#' @return The number of servers attempted.
#' @export
#' @examples
#' \dontrun{
#' # Stop any registered servers, regardless of how they were started
#' stopped <- osrm_stop_all()
#' stopped
#' }
osrm_stop_all <- function() {
  reg <- .osrm_state$registry
  if (!length(reg)) {
    return(0L)
  }

  ids <- names(reg)
  n <- 0L
  for (id in ids) {
    n <- n + 1L
    try(osrm_stop(id = id, quiet = TRUE), silent = TRUE)
  }
  n
}
