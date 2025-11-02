skip_if_no_osrm <- function() {
  if (isTRUE(getOption("osrm.backend.skip_osrm_tests", FALSE))) {
    reason <- getOption("osrm.backend.install_error", "OSRM backend binaries are unavailable")
    testthat::skip(reason)
  }
}
