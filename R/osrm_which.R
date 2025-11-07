#' Locate the OSRM Installation Used by `osrm.backend`
#'
#' Resolves the `osrm-routed` executable available on the current `PATH`
#' (or the override provided via `options(osrm.routed.exec)`). Runs
#' `osrm-routed --version` to verify availability, then prints the directory
#' containing the executable together with the backend version reported by
#' `osrm-routed` so you know what will be used in the current session.
#' @param quiet Logical; if `FALSE` (default), prints information about the located installation. If `TRUE`, suppresses printed output and only returns the information invisibly as a list.
#' @return Invisibly returns a list with components `executable` (full path to
#'   `osrm-routed`), `directory` (its parent folder), `osrm_version` (character
#'   vector of non-empty lines emitted by `osrm-routed --version`), and the raw
#'   `processx::run` result in `logs`.
#' @export
osrm_which <- function(quiet = FALSE) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required for osrm_which", call. = FALSE)
  }

  osrm_exec <- getOption("osrm.routed.exec", "osrm-routed")
  resolved <- Sys.which(osrm_exec)

  if (!nzchar(resolved)) {
    stop(
      "Cannot find '",
      osrm_exec,
      "' on PATH. Install OSRM or set options(osrm.routed.exec = '/path/to/osrm-routed').",
      call. = FALSE
    )
  }

  result <- processx::run(
    osrm_exec,
    args = "--version",
    echo = FALSE,
    spinner = FALSE,
    echo_cmd = FALSE,
    error_on_status = FALSE
  )

  if (!identical(result$status, 0L)) {
    stop(
      "Running '",
      osrm_exec,
      " --version' failed with status ",
      result$status,
      ".\n",
      result$stderr,
      call. = FALSE
    )
  }

  output <- c(result$stdout, result$stderr)
  output <- output[nzchar(output)]
  version_lines <- character()
  if (length(output)) {
    version_lines <- trimws(unlist(strsplit(
      paste(output, collapse = "\n"),
      "\r?\n"
    )))
    version_lines <- version_lines[nzchar(version_lines)]
  }

  install_dir <- dirname(resolved)

  if (!quiet) {
    message("OSRM installation directory: ", install_dir)
    if (length(version_lines)) {
      message("osrm-backend version: ", version_lines[1])
      if (length(version_lines) > 1) {
        for (extra in version_lines[-1]) {
          message("  ", extra)
        }
      }
    } else {
      message("osrm-backend version: <no output>")
    }
    message(
      "This is the installation that will be used by osrm_start() and other functions."
    )
  }

  invisible(list(
    executable = resolved,
    directory = install_dir,
    osrm_version = version_lines,
    logs = result
  ))
}
