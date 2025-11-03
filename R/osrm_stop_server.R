# R/osrm_stop_server.R

#' List OSRM servers started via this package
#'
#' Returns a snapshot of servers registered by [osrm_start_server()].
#' You can stop one by passing its `id`, `port`, or `pid` to [osrm_stop_server()].
#'
#' @return A tibble (if available) or data.frame with columns:
#'   `id`, `pid`, `port`, `algorithm`, `started_at`, `alive`, `has_handle`.
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
    if (requireNamespace("tibble", quietly = TRUE)) {
      return(tibble::as_tibble(out))
    }
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

  if (requireNamespace("tibble", quietly = TRUE)) {
    return(tibble::as_tibble(out))
  }
  out
}

#' Stop an OSRM server
#'
#' Terminates an `osrm-routed` process that was launched by [osrm_start_server()].
#' You may pass the original `processx::process` object **or** select a server by
#' `id`, `port`, or `pid`. If nothing is specified, the most recently started
#' *alive* server is stopped.
#'
#' @param server Optional `processx::process` returned by [osrm_start_server()].
#' @param id Optional character id from [osrm_servers()].
#' @param port Optional integer TCP port.
#' @param pid Optional integer process id.
#' @param wait Integer milliseconds to wait for clean shutdown (default `1000`).
#' @param quiet Logical; suppress messages (default `FALSE`).
#'
#' @return Invisibly, a list with fields `id`, `pid`, `port`, `stopped` (logical).
#' @export
osrm_stop_server <- function(
  server = NULL,
  id = NULL,
  port = NULL,
  pid = NULL,
  wait = 1000L,
  quiet = FALSE
) {
  # Case 1: legacy API — user supplied a process object
  if (!is.null(server)) {
    if (!inherits(server, "process")) {
      stop("'server' must be a processx::process object", call. = FALSE)
    }
    targ_pid <- suppressWarnings(try(server$get_pid(), silent = TRUE))
    if (!inherits(targ_pid, "try-error")) {
      # Try to find and deregister by pid
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
      return(invisible(list(
        id = if (length(hit)) hit[[1]] else NA_character_,
        pid = as.integer(targ_pid),
        port = NA_integer_,
        stopped = TRUE
      )))
    } else {
      # Could not read pid; best effort kill
      try(server$kill(), silent = TRUE)
      try(server$wait(as.integer(wait)), silent = TRUE)
      if (!quiet) {
        message("Stopped OSRM server (unknown pid).")
      }
      return(invisible(list(
        id = NA_character_,
        pid = NA_integer_,
        port = NA_integer_,
        stopped = TRUE
      )))
    }
  }

  # Case 2: selection via registry
  reg <- .osrm_state$registry
  if (!length(reg)) {
    if (!quiet) {
      message("No registered OSRM servers.")
    }
    return(invisible(list(
      id = NA_character_,
      pid = NA_integer_,
      port = NA_integer_,
      stopped = FALSE
    )))
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
      if (length(hits)) tail(hits, 1) else integer()
    } else if (!is.null(pid)) {
      hits <- which(vapply(
        reg,
        function(e) identical(e$pid, as.integer(pid)),
        logical(1)
      ))
      if (length(hits)) tail(hits, 1) else integer()
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
      tail(which(alive), 1)
    }
  }

  idx <- pick_index()
  if (!length(idx) || is.na(idx) || idx < 1) {
    stop(
      "Could not identify a server to stop. Provide 'id', 'port', or 'pid'.",
      call. = FALSE
    )
  }

  entry <- reg[[idx]]
  stopped <- FALSE

  # Prefer live process handle
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

  invisible(list(
    id = entry$id,
    pid = as.integer(entry$pid %||% NA_integer_),
    port = as.integer(entry$port %||% NA_integer_),
    stopped = isTRUE(stopped)
  ))
}

#' Stop all running OSRM servers started via this package
#'
#' @return Invisibly, the number of servers attempted.
#' @export
osrm_stop_all <- function() {
  reg <- .osrm_state$registry
  if (!length(reg)) {
    return(invisible(0L))
  }

  ids <- names(reg)
  n <- 0L
  for (id in ids) {
    n <- n + 1L
    try(osrm_stop_server(id = id, quiet = TRUE), silent = TRUE)
  }
  invisible(n)
}
