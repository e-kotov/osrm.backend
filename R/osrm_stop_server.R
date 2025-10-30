#' Stop an OSRM server started with `osrm_start_server`
#'
#' @description
#' Terminates an `osrm-routed` process that was launched by [osrm_start_server()].
#'
#' @param server A `processx::process` object returned by [osrm_start_server()].
#' @return Invisibly returns the same `processx::process` object (now stopped).
#' @export
osrm_stop_server <- function(server) {
  if (!inherits(server, "process")) {
    stop("'server' must be a processx::process object", call. = FALSE)
  }
  if (server$is_alive()) {
    server$kill() # sends SIGKILL on Unix, TerminateProcess on Windows
    # optionally wait a moment:
    server$wait(1000) # waits up to 1 s for clean shutdown
  }
  invisible(server)
}
