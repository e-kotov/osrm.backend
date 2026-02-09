# R/zzz.R  — registry with tools::R_user_dir() and jsonlite JSON persistence

# Package-private state --------------------------------------------------------

.osrm_state <- new.env(parent = emptyenv())
.osrm_state$registry <- list() # id -> list(id, pid, port, prefix, algorithm, started_at, proc?)

# Small helpers ---------------------------------------------------------------

`%||%` <- function(a, b) {
  if (is.null(a) || (is.character(a) && !nzchar(a))) b else a
}

.osrm_registry_dir <- function() {
  # CI/override first
  opt <- getOption("osrm.server.state_dir")
  if (!is.null(opt) && nzchar(opt)) {
    return(opt)
  }

  env <- Sys.getenv("OSRM_BACKEND_STATE_DIR", unset = "")
  if (nzchar(env)) {
    return(env)
  }

  # Non-interactive: tempdir() is safest to avoid collisions on CI
  if (!interactive()) {
    return(tempdir())
  }

  # Interactive: stable per-user cache dir (base R)
  tools::R_user_dir("osrm.backend", which = "cache")
}

.osrm_registry_path <- function() {
  file.path(.osrm_registry_dir(), "servers.json")
}

# Persistence (atomic JSON via jsonlite) --------------------------------------

.osrm_registry_save <- function() {
  dir <- .osrm_registry_dir()
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  fn <- .osrm_registry_path()
  tmp <- paste0(fn, ".tmp")

  # Strip non-serializable fields (processx::process) before writing
  reg <- .osrm_state$registry
  reg_serializable <- lapply(reg, function(x) {
    x$proc <- NULL
    x
  })

  # Write JSON atomically
  jsonlite::write_json(reg_serializable, tmp, auto_unbox = TRUE, pretty = TRUE)
  ok <- suppressWarnings(file.rename(tmp, fn))
  if (!ok) {
    suppressWarnings(file.copy(tmp, fn, overwrite = TRUE))
    unlink(tmp, force = TRUE)
  }
}

.osrm_registry_load <- function() {
  fn <- .osrm_registry_path()
  if (!file.exists(fn)) {
    .osrm_state$registry <- list()
    return(invisible(NULL))
  }
  reg <- try(jsonlite::read_json(fn, simplifyVector = TRUE), silent = TRUE)
  if (inherits(reg, "try-error") || !is.list(reg)) {
    .osrm_state$registry <- list()
    return(invisible(NULL))
  }
  # Ensure list with names = ids when possible
  if (!is.null(names(reg)) && all(nzchar(names(reg)))) {
    .osrm_state$registry <- reg
  } else {
    idx <- vapply(
      reg,
      function(e) is.list(e) && length(e$id) == 1 && nzchar(e$id),
      logical(1)
    )
    if (any(idx)) {
      .osrm_state$registry <- stats::setNames(
        reg[idx],
        vapply(reg[idx], `[[`, "", "id")
      )
    } else {
      .osrm_state$registry <- list()
    }
  }
  .osrm_cleanup_orphans()
  invisible(NULL)
}

# Process utilities (ps optional) ---------------------------------------------

.osrm_has_ps <- function() {
  requireNamespace("ps", quietly = TRUE)
}

.osrm_pid_is_running <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (is.na(pid) || pid <= 0) {
    return(FALSE)
  }

  if (.osrm_has_ps()) {
    h <- try(ps::ps_handle(pid), silent = TRUE)
    if (inherits(h, "try-error")) {
      return(FALSE)
    }
    return(tryCatch(ps::ps_is_running(h), error = function(...) FALSE))
  }

  # Without ps: best-effort
  if (.Platform$OS.type == "unix" && file.exists("/proc")) {
    return(dir.exists(file.path("/proc", pid)))
  }
  # Conservative fallback (can't reliably check): assume TRUE
  TRUE
}

.osrm_kill_pid <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (is.na(pid) || pid <= 0) {
    return(invisible(FALSE))
  }

  if (.osrm_has_ps()) {
    h <- try(ps::ps_handle(pid), silent = TRUE)
    if (!inherits(h, "try-error")) {
      if (tryCatch(ps::ps_is_running(h), error = function(...) FALSE)) {
        try(ps::ps_kill(h), silent = TRUE)
      }
    }
    return(invisible(TRUE))
  }

  if (.Platform$OS.type == "unix") {
    # SIGTERM-equivalent; OSRM should exit promptly
    try(tools::pskill(pid), silent = TRUE)
  } else {
    tk <- Sys.which("taskkill")
    if (nzchar(tk)) {
      system2(tk, c("/PID", pid, "/F"), stdout = FALSE, stderr = FALSE)
    }
  }
  invisible(TRUE)
}

.osrm_cleanup_orphans <- function() {
  reg <- .osrm_state$registry
  if (!length(reg)) {
    return(invisible(NULL))
  }

  dead_ids <- character(0)
  for (nm in names(reg)) {
    e <- reg[[nm]]
    alive <- FALSE
    if (!is.null(e$proc) && inherits(e$proc, "process")) {
      alive <- tryCatch(e$proc$is_alive(), error = function(...) FALSE)
    } else if (!is.null(e$pid)) {
      alive <- .osrm_pid_is_running(e$pid)
    }
    if (!alive) dead_ids <- c(dead_ids, nm)
  }

  if (length(dead_ids)) {
    for (id in dead_ids) {
      reg[[id]] <- NULL
    }
    .osrm_state$registry <- reg
    .osrm_registry_save()
  }
  invisible(NULL)
}

# Registration API used by osrm_start_server() --------------------------------

.osrm_register <- function(proc, port, prefix, algorithm, log = NULL, input_osm = NULL, profile = NULL) {
  ts <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC", usetz = FALSE)
  pid <- tryCatch(
    if (!is.null(proc)) proc$get_pid() else NA_integer_,
    error = function(...) NA_integer_
  )
  prt <- suppressWarnings(as.integer(port))
  if (is.na(prt)) {
    prt <- NA_integer_
  }

  id <- sprintf(
    "osrm-%s-%s-%s",
    if (!is.na(pid)) pid else "na",
    if (!is.na(prt)) prt else "na",
    format(Sys.time(), "%Y%m%d%H%M%OS3")
  )

  .osrm_state$registry[[id]] <- list(
    id = id,
    pid = pid,
    port = prt,
    prefix = as.character(prefix %||% ""),
    algorithm = as.character(algorithm %||% ""),
    started_at = ts,
    log = as.character(log %||% ""),
    input_osm = as.character(input_osm %||% ""),
    profile = as.character(profile %||% ""),
    proc = proc
  )

  .osrm_registry_save()
  id
}

.osrm_deregister <- function(id) {
  if (length(.osrm_state$registry)) {
    .osrm_state$registry[[id]] <- NULL
    .osrm_registry_save()
  }
  invisible(NULL)
}

# Internal stop-all for unload -------------------------------------------------

.osrm_stop_all_internal <- function() {
  reg <- .osrm_state$registry
  if (!length(reg)) {
    return(invisible(NULL))
  }

  for (e in reg) {
    if (!is.null(e$proc) && inherits(e$proc, "process")) {
      if (tryCatch(e$proc$is_alive(), error = function(...) FALSE)) {
        try(e$proc$kill(), silent = TRUE)
        try(e$proc$wait(500), silent = TRUE)
      }
    } else if (!is.null(e$pid)) {
      if (.osrm_pid_is_running(e$pid)) .osrm_kill_pid(e$pid)
    }
  }
  .osrm_state$registry <- list()
  .osrm_registry_save()
  invisible(NULL)
}

# Lifecycle hooks -------------------------------------------------------------

.onLoad <- function(libname, pkgname) {
  .osrm_registry_load()
}

.onUnload <- function(libpath) {
  stop_on_unload <- getOption(
    "osrm.server.stop_on_unload",
    default = !interactive()
  )
  if (isTRUE(stop_on_unload)) {
    try(.osrm_stop_all_internal(), silent = TRUE)
  }
}

#' Retrieve the OSRM Profile for a Running Server
#'
#' @description
#' Attempts to determine the profile (e.g., "car", "bike", "foot") used by an OSRM server.
#' It follows a priority list:
#' 1. Checks the active server registry for the given port or ID.
#' 2. Checks for a `dataset.meta.json` file in the directory of the graph file.
#' 3. Checks the graph filename for hints (e.g. `berlin-car.osrm`).
#' 4. Falls back to `getOption("osrm.profile")`.
#'
#' @param input_osrm Optional. Can be an `osrm_job` object, a path string, or NULL.
#' @param port Optional integer. The port of the server.
#' @return A character string representing the profile name (default "car").
#' @export
osrm_get_server_profile <- function(input_osrm = NULL, port = NULL) {
  # 1. Check Registry (if port is known or input_osrm is process)
  reg_profile <- NULL
  
  if (inherits(input_osrm, "process")) {
    # It's an osrm_job process
    pid <- tryCatch(input_osrm$get_pid(), error = function(e) NA)
    if (!is.na(pid)) {
      reg <- .osrm_state$registry
      match <- Filter(function(x) identical(x$pid, pid), reg)
      if (length(match) > 0 && nzchar(match[[1]]$profile)) {
        return(match[[1]]$profile)
      }
    }
  } else if (!is.null(port)) {
    # Check by port
    prt <- as.integer(port)
    reg <- .osrm_state$registry
    # Find active server on this port
    match <- Filter(function(x) identical(x$port, prt), reg)
    if (length(match) > 0) {
      # Use the most recently started one
      last <- match[[length(match)]]
      if (nzchar(last$profile)) return(last$profile)
      
      # If registry entry exists but has no profile, maybe we can look up its path?
      # (Implementation detail: 'prefix' in registry stores the path)
      if (nzchar(last$prefix)) input_osrm <- last$prefix
    }
  }

  # 2. Check Metadata File (if we have a path)
  path <- NULL
  if (is.character(input_osrm) && length(input_osrm) == 1) {
    path <- input_osrm
  }
  
  if (!is.null(path)) {
    # Check dataset.meta.json
    meta_path <- file.path(dirname(path), "dataset.meta.json")
    if (file.exists(meta_path)) {
      meta <- tryCatch(jsonlite::read_json(meta_path), error = function(e) NULL)
      if (!is.null(meta) && !is.null(meta$profile) && nzchar(meta$profile)) {
        return(meta$profile)
      }
    }
    
    # 3. Heuristic: Check filename (e.g. "berlin-car.osrm")
    base <- basename(path)
    if (grepl("car", base, ignore.case = TRUE)) return("car")
    if (grepl("bike|bicycle", base, ignore.case = TRUE)) return("bike")
    if (grepl("foot|walk", base, ignore.case = TRUE)) return("foot")
  }

  # 4. Fallback
  getOption("osrm.profile", default = "car")
}
