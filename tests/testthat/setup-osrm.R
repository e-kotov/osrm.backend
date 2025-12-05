# Allow tar-related warnings during tests so we can detect regressions when
# Windows-specific extraction issues resurface.
Sys.setenv(OSRM_BACKEND_ALLOW_TAR_WARNINGS = "true")

test_osrm_path <- file.path(tempdir(), "osrm_bin_test")

# Clean up any previous runs to ensure a fresh state
if (dir.exists(test_osrm_path)) unlink(test_osrm_path, recursive = TRUE)
dir.create(test_osrm_path, showWarnings = FALSE, recursive = TRUE)

# POLITE INSTALLATION:
# Only install the binary if we are running LOCALLY or on CI (NOT_CRAN="true").
# We skip installation on CRAN to avoid timeouts and policy violations.
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  message("Running locally/CI: Installing OSRM binary to tempdir...")
  try(osrm_install(version = "v5.27.1", path = test_osrm_path, quiet = TRUE), silent = TRUE)
} else {
  message("Running on CRAN: Skipping OSRM binary installation.")
}

# CONFIGURATION (Only runs if binary exists):
bin_path <- list.files(test_osrm_path, pattern = "^osrm-routed(\\.exe)?$", full.names = TRUE, recursive = TRUE)

if (length(bin_path) > 0) {
  exe_path <- normalizePath(bin_path[1], mustWork = FALSE)

  # CRITICAL: Ensure the binary is executable (fixes Sys.which failure on Linux/Mac)
  if (.Platform$OS.type == "unix") {
    Sys.chmod(exe_path, mode = "0755")
  }

  # Set the option required by osrm_start_server
  options(osrm.routed.exec = exe_path)

  # Add to PATH so Sys.which finds it
  old_path <- Sys.getenv("PATH")
  Sys.setenv(PATH = paste(dirname(exe_path), old_path, sep = .Platform$path.sep))
}

# Helper functions to check for required binaries and profiles
has_osrm_binary <- function() nzchar(Sys.which("osrm-extract")) || !is.null(getOption("osrm.routed.exec"))

has_osrm_profile <- function() {
  prof <- try(osrm_find_profile("car.lua"), silent = TRUE)
  if (inherits(prof, "try-error")) {
    return(FALSE)
  }
  options(osrm.backend.default_profile = prof)
  TRUE
}

# Set skip flag based on availability
if (!has_osrm_binary() || !has_osrm_profile()) {
  options(osrm.backend.skip_osrm_tests = TRUE)
} else {
  options(osrm.backend.skip_osrm_tests = FALSE)
}
