# R/osrm_stop_server.R

#' List OSRM servers
#'
#' @description
#' Lists `osrm-routed` processes. By default, it returns a snapshot of servers
#' started by the current R session (registered via [osrm_start_server()] or [osrm_start()]).
#' You can optionally list all `osrm-routed` processes running on the system,
#' including those started by other sessions or manually.
#'
#' You can stop a server by passing its `id`, `port`, or `pid` to [osrm_stop()].
#'
#' @param include_all Logical; if `TRUE`, scans the system process table for
#'   all `osrm-routed` processes, including those not started by this package
#'   in the current session. Default is `FALSE`.
#'
#' @return A data.frame with columns:
#'   `id`, `pid`, `port`, `algorithm`, `started_at`, `alive`, `has_handle`, `log`, `input_osm`.
#'   External servers will have `id` prefixed with `sys-` and `log` set to `<external>`.
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
#'   tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#'   file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
#'   graph <- osrm_prepare_graph(tmp_pbf, overwrite = TRUE, threads = 1L)
#'
#'   srv <- osrm_start_server(graph$osrm_job_artifact, port = 6000, threads = 1L)
#'   osrm_servers()
#'   osrm_stop(srv)
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
osrm_servers <- function(include_all = FALSE) {
  # 1. Background Cleanup: Always scan other registries to GC dead files
  # This ensures orphaned registry files from crashed sessions are pruned.
  orphans <- tryCatch(.osrm_registry_scan_others(), error = function(e) list())

  # 2. Internal Registry (Current Session)
  reg <- .osrm_state$registry
  
  # Helper to build an empty/structured data.frame
  build_df <- function(ids, pids, ports, algos, starts, alives, handles, logs, inputs) {
    data.frame(
      id = as.character(ids),
      pid = as.integer(pids),
      port = as.integer(ports),
      algorithm = as.character(algos),
      started_at = as.POSIXct(starts),
      alive = as.logical(alives),
      has_handle = as.logical(handles),
      log = as.character(logs),
      input_osm = as.character(inputs),
      stringsAsFactors = FALSE
    )
  }

  out_reg <- if (length(reg)) {
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
    build_df(
      ids = vapply(reg, `[[`, "", "id"),
      pids = vapply(reg, function(e) as.integer(e$pid %||% NA_integer_), 0L),
      ports = vapply(reg, function(e) as.integer(e$port %||% NA_integer_), 0L),
      algos = vapply(reg, function(e) as.character(e$algorithm %||% ""), ""),
      starts = vapply(reg, function(e) as.character(e$started_at %||% NA_character_), ""),
      alives = alive_vec,
      handles = handle_vec,
      logs = vapply(reg, function(e) as.character(e$log %||% ""), ""),
      inputs = vapply(reg, function(e) as.character(e$input_osm %||% ""), "")
    )
  } else {
    build_df(character(), integer(), integer(), character(), character(), logical(), logical(), character(), character())
  }

  if (!isTRUE(include_all)) {
    return(out_reg)
  }
  
  # 3. Process Orphans/Foreign (already scanned in Step 1)
  out_orph <- if (length(orphans)) {
    build_df(
      ids = vapply(orphans, `[[`, "", "id"),
      pids = vapply(orphans, `[[`, 0L, "pid"),
      ports = vapply(orphans, `[[`, 0L, "port"),
      algos = vapply(orphans, function(e) as.character(e$algorithm %||% ""), ""),
      starts = vapply(orphans, function(e) as.character(e$started_at %||% NA_character_), ""),
      alives = rep(TRUE, length(orphans)),
      handles = rep(FALSE, length(orphans)),
      logs = vapply(orphans, function(e) as.character(e$log %||% ""), ""),
      inputs = vapply(orphans, function(e) as.character(e$input_osm %||% ""), "")
    )
  } else {
    build_df(character(), integer(), integer(), character(), character(), logical(), logical(), character(), character())
  }

  # 3. System Process Discovery via ps
  sys_procs <- tryCatch(ps::ps(), error = function(e) NULL)
  
  sys_rows <- if (!is.null(sys_procs)) {
    # Filter for osrm-routed
    is_osrm <- grepl("osrm-routed", sys_procs$name, ignore.case = TRUE)
    sys_osrm <- sys_procs[is_osrm, ]
    
    if (nrow(sys_osrm) > 0) {
      ext_ids <- character()
      ext_pids <- integer()
      ext_ports <- integer()
      ext_algos <- character()
      ext_starts <- as.POSIXct(character())
      ext_alives <- logical()
      ext_handles <- logical()
      ext_logs <- character()
      ext_inputs <- character()

      # PIDs we already know about (Current + Orphans)
      known_pids <- c(out_reg$pid, out_orph$pid)

      for (i in seq_len(nrow(sys_osrm))) {
        p <- sys_osrm[i, ]
        this_pid <- p$pid
        
        if (this_pid %in% known_pids) next

        # Parse command line
        cmd <- tryCatch(ps::ps_cmdline(p), error = function(e) character())
        
        # Defaults
        port <- 5000L
        algo <- "CH"
        input <- NA_character_
        
        # Simple parsing logic
        if (length(cmd) > 1) {
          # Port
          idx_p <- which(cmd %in% c("-p", "--port"))
          if (length(idx_p) && (idx_p[1] + 1 <= length(cmd))) {
            val <- suppressWarnings(as.integer(cmd[idx_p[1] + 1]))
            if (!is.na(val)) port <- val
          }
          
          # Algorithm
          idx_a <- which(cmd %in% c("-a", "--algorithm"))
          if (length(idx_a) && (idx_a[1] + 1 <= length(cmd))) {
            algo <- cmd[idx_a[1] + 1]
          }
          
          # Input: heuristic - last arg that doesn't start with "-"
          args <- cmd[-1]
          non_flag <- args[!startsWith(args, "-")]
          if (length(non_flag) > 0) {
            input <- utils::tail(non_flag, 1)
          }
        }

        ext_ids <- c(ext_ids, paste0("sys-", this_pid))
        ext_pids <- c(ext_pids, this_pid)
        ext_ports <- c(ext_ports, port)
        ext_algos <- c(ext_algos, algo)
        ext_starts <- c(ext_starts, NA) 
        ext_alives <- c(ext_alives, TRUE)
        ext_handles <- c(ext_handles, FALSE)
        ext_logs <- c(ext_logs, "<external>")
        ext_inputs <- c(ext_inputs, if (is.na(input)) "" else input)
      }
      
      if (length(ext_pids) > 0) {
        build_df(ext_ids, ext_pids, ext_ports, ext_algos, ext_starts, ext_alives, ext_handles, ext_logs, ext_inputs)
      } else {
        NULL
      }
    } else {
      NULL
    }
  } else {
    NULL
  }

  # Merge: Current + Orphans + System
  res <- out_reg
  if (nrow(out_orph) > 0) res <- rbind(res, out_orph)
  if (!is.null(sys_rows) && nrow(sys_rows) > 0) res <- rbind(res, sys_rows)
  
  res
}

#' Stop an OSRM Server
#'
#' Terminates an `osrm-routed` process launched by `osrm_start()` or
#' `osrm_start_server()`. Can also stop external servers by PID or ID.
#'
#' @details
#' This function provides a flexible way to stop a running OSRM process. If no
#' arguments are specified, it defaults to stopping the most recently started
#' server that is still alive in the current session.
#'
#' You can also stop a specific server by providing:
#' \itemize{
#'   \item The `processx::process` object returned by `osrm_start()` or `osrm_start_server()`.
#'   \item The server's `id`, `port`, or `pid` (use `osrm_servers()` to find these).
#' }
#' 
#' **Advanced Use:**
#' You can stop an external `osrm-routed` process (one not started by the current
#' R session) by passing its PID, or by finding it via `osrm_servers(include_all = TRUE)`
#' and passing its `id` or `port`. This requires permission to signal the process.
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
#'   tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#'   file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
#'   graph <- osrm_prepare_graph(tmp_pbf, overwrite = TRUE, threads = 1L)
#'
#'   srv <- osrm_start_server(graph$osrm_job_artifact, port = 6000, threads = 1L)
#'
#'   # Stop by passing the process object
#'   osrm_stop(srv)
#'
#'   # Or stop by port after the process is registered
#'   osrm_stop(port = 6000)
#'
#'   osrm_uninstall(
#'     dest_dir = install_dir,
#'     clear_path = TRUE,
#'     force = TRUE,
#'     quiet = TRUE
#'   )
#'   unlink(osrm_dir, recursive = TRUE)
#' }
#' 
#' \dontrun{
#'   # Advanced: Stop an external server by PID
#'   # 1. Find the PID of an external server
#'   srvs <- osrm_servers(include_all = TRUE)
#'   # 2. Stop it by PID
#'   if (nrow(srvs) > 0) {
#'     osrm_stop(pid = srvs$pid[1])
#'   }
#'   
#'   # Or stop by its external ID (e.g., "sys-12345")
#'   osrm_stop(id = "sys-12345")
#' }
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

  # Case 2: Direct PID (External/Manual stop)
  if (!is.null(pid)) {
    pid_int <- as.integer(pid)
    
    # Check if it's in our local registry first (to clean up properly)
    reg <- .osrm_state$registry
    hit <- names(reg)[vapply(
      reg,
      function(e) identical(e$pid, pid_int),
      logical(1)
    )]
    
    if (length(hit)) {
      # Redirect to local stop logic (Case 3 logic will handle it if we just fall through, 
      # but let's be explicit)
      return(osrm_stop(id = hit[1], wait = wait, quiet = quiet))
    }
    
    # External stop
    if (.osrm_pid_is_running(pid_int)) {
      .osrm_kill_pid(pid_int)
      # Wait a moment
      Sys.sleep(wait / 1000)
      still_alive <- .osrm_pid_is_running(pid_int)
      if (!quiet) {
        if (!still_alive) {
          message("Stopped external OSRM server (pid ", pid_int, ").")
        } else {
          message("Attempted to stop external OSRM server (pid ", pid_int, "), but it may still be running.")
        }
      }
      return(list(
        id = NA_character_,
        pid = pid_int,
        port = NA_integer_,
        stopped = !still_alive
      ))
    } else {
      if (!quiet) message("Process with PID ", pid_int, " not found.")
      return(list(
        id = NA_character_,
        pid = pid_int,
        port = NA_integer_,
        stopped = TRUE
      ))
    }
  }

  # Case 3: Selection via Registry (Local OR External)
  reg <- .osrm_state$registry
  
  # Search Local Registry
  idx <- NULL
  if (length(reg)) {
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
  }

  # Found in Local Registry?
  if (length(idx) && !is.na(idx) && idx >= 1) {
    entry <- reg[[idx]]
    stopped <- FALSE

    # Prefer to use the live process handle if it exists in the session
    if (!is.null(entry$proc) && inherits(entry$proc, "process")) {
      if (tryCatch(entry$proc$is_alive(), error = function(...) FALSE)) {
        try(entry$proc$kill(), silent = TRUE)
        try(entry$proc$wait(as.integer(wait)), silent = TRUE)
        stopped <- TRUE
      } else {
        stopped <- TRUE
      }
    } else if (!is.null(entry$pid)) {
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

    return(list(
      id = entry$id,
      pid = as.integer(entry$pid %||% NA_integer_),
      port = as.integer(entry$port %||% NA_integer_),
      stopped = isTRUE(stopped)
    ))
  }

  # Case 4: Not found locally. Try External Discovery.
  # Only try if specific criteria were given (don't default to killing random external servers)
  if (!is.null(id) || !is.null(port)) {
    # Special case: id="sys-PID" -> Kill PID
    if (!is.null(id) && grepl("^sys-(\\d+)$", id)) {
      pid_from_id <- as.integer(sub("^sys-(\\d+)$", "\\1", id))
      return(osrm_stop(pid = pid_from_id, wait = wait, quiet = quiet))
    }

    # Search all
    all_srvs <- osrm_servers(include_all = TRUE)
    
    target_pid <- NULL
    target_id <- NA_character_
    
    if (!is.null(id)) {
      match <- all_srvs[all_srvs$id == id, ]
      if (nrow(match) > 0) {
        target_pid <- match$pid[1]
        target_id <- match$id[1]
      }
    } else if (!is.null(port)) {
      # Find by port (most recent first if multiple? usually only one per port)
      match <- all_srvs[all_srvs$port == as.integer(port), ]
      if (nrow(match) > 0) {
        target_pid <- utils::tail(match$pid, 1)
        target_id <- utils::tail(match$id, 1)
      }
    }
    
    if (!is.null(target_pid)) {
      return(osrm_stop(pid = target_pid, wait = wait, quiet = quiet))
    }
  }

  # Case 5: Failure
  if (!is.null(id) || !is.null(port)) {
    stop(
      "Could not identify a server to stop with the specified criteria (scanned local and external).",
      call. = FALSE
    )
  }
  
  if (!quiet) {
    message("No running OSRM servers to stop in the current session.")
  }
  
  list(stopped = FALSE)
}

#' Stop all running OSRM servers started via this package
#'
#' @return The number of servers attempted.
#' @export
#' @examples
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   install_dir <- osrm_install(
#'     version = "latest",
#'     path_action = "session",
#'     quiet = TRUE
#'   )
#'
#'   pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#'   osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
#'   dir.create(osrm_dir, recursive = TRUE)
#'   tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#'   file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
#'   graph <- osrm_prepare_graph(tmp_pbf, overwrite = TRUE, threads = 1L)
#'
#'   srv <- osrm_start_server(graph$osrm_job_artifact, port = 6000, threads = 1L)
#'   stopped <- osrm_stop_all()
#'   stopped
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
osrm_stop_all <- function(include_all = FALSE) {
  # Get all servers (registry + optional external)
  srvs <- osrm_servers(include_all = include_all)
  
  if (nrow(srvs) == 0) {
    return(0L)
  }

  n <- 0L
  
  # 1. Stop registry servers first (by ID)
  reg_srvs <- srvs[srvs$id != "" & !startsWith(srvs$id, "sys-"), ]
  for (id in reg_srvs$id) {
    n <- n + 1L
    try(osrm_stop(id = id, quiet = TRUE), silent = TRUE)
  }
  
  # 2. Stop external servers (by PID)
  ext_srvs <- srvs[startsWith(srvs$id, "sys-"), ]
  for (pid in ext_srvs$pid) {
    n <- n + 1L
    try(osrm_stop(pid = pid, quiet = TRUE), silent = TRUE)
  }
  
  n
}