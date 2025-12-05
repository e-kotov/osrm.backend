# Allow tar-related warnings during tests so we can detect regressions when
# Windows-specific extraction issues resurface.
Sys.setenv(OSRM_BACKEND_ALLOW_TAR_WARNINGS = "true")

# Define a temporary path for the OSRM binary to avoid writing to cache directories
# during CRAN checks (which disallows modifications to home directories)
test_osrm_path <- file.path(tempdir(), "osrm_bin_test")
dir.create(test_osrm_path, showWarnings = FALSE, recursive = TRUE)

# Ensure OSRM backend binaries and profiles are available before tests run.
# We'll attempt to install them once if they are missing. If installation fails,
# we mark tests to be skipped via an option so individual tests can opt out.

has_osrm_binary <- function() nzchar(Sys.which("osrm-extract"))

has_osrm_profile <- function() {
  prof <- try(osrm_find_profile("car.lua"), silent = TRUE)
  if (inherits(prof, "try-error")) {
    return(FALSE)
  }
  options(osrm.backend.default_profile = prof)
  TRUE
}

if (!has_osrm_binary() || !has_osrm_profile()) {
  # If on CRAN, do NOT attempt install, just mark tests to skip
  if (!isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))) {
    options(osrm.backend.skip_osrm_tests = TRUE)
  } else {
    install_attempt <- try(
      osrm_install(version = "v5.27.1", path = test_osrm_path, quiet = TRUE),
      silent = TRUE
    )

    if (inherits(install_attempt, "try-error")) {
      options(
        osrm.backend.skip_osrm_tests = TRUE,
        osrm.backend.install_error = as.character(install_attempt)
      )
    }
  }
}

# Re-check after installation attempt and set skip flag accordingly.
if (!has_osrm_binary() || !has_osrm_profile()) {
  options(osrm.backend.skip_osrm_tests = TRUE)
} else {
  options(osrm.backend.skip_osrm_tests = FALSE)
}
