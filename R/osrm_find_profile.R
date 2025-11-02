#' Locate an OSRM Lua profile (e.g. car.lua) in a host installation
#'
#' By default OSRM ships profiles for "car", "bike" and "foot" in a
#' `profiles/` directory alongside the binaries.  This function will try to locate \code{osrm-routed} on the \code{PATH}, resolve symlinks, and look first for a `profiles/` directory next to the binary (as placed there by \code{osrm_install()}). If that fails, it looks for sibling directories `share/osrm/profiles` and `share/osrm-backend/profiles`. IF that fails, it will try to fall back on \code{/usr/local/share/osrm/profiles},\code{/usr/local/share/osrm-backend/profiles}, \code{/usr/share/osrm/profiles}, and \code{/usr/share/osrm-backend/profiles}.
#'
#' @param profile A single string, the name of the Lua profile file
#'   (e.g. \code{"car.lua"}). Defaults to \code{"car.lua"}.
#' @return The normalized filesystem path to the profile.
#'
#' @examples
#' \donttest{
#' osrm_find_profile("car.lua")
#' }
#'
#' @export
osrm_find_profile <- function(
  profile = "car.lua"
) {
  # locate and resolve osrm-routed
  osrm_bin <- Sys.which("osrm-routed")
  if (!nzchar(osrm_bin)) {
    stop(
      "`osrm-routed` not found in PATH; please install OSRM backend",
      call. = FALSE
    )
  }
  real_bin <- try(normalizePath(osrm_bin), silent = TRUE)
  if (inherits(real_bin, "try-error")) {
    real_bin <- osrm_bin
  }
  bindir <- dirname(real_bin)

  # probe common relative locations
  candidates <- c(
    file.path(bindir, "profiles", profile),
    file.path(bindir, "..", "share", "osrm", "profiles", profile),
    file.path(bindir, "..", "share", "osrm-backend", "profiles", profile),
    file.path(bindir, "..", "lib", "osrm", "profiles", profile),
    file.path(bindir, "..", "lib", "osrm-backend", "profiles", profile),
    file.path("/", "usr", "local", "share", "osrm", "profiles", profile),
    file.path(
      "/",
      "usr",
      "local",
      "share",
      "osrm-backend",
      "profiles",
      profile
    ),
    file.path("/", "usr", "share", "osrm", "profiles", profile),
    file.path("/", "usr", "share", "osrm-backend", "profiles", profile)
  )
  for (p in candidates) {
    if (file.exists(p)) {
      return(normalizePath(p))
    }
  }

  stop(
    sprintf(
      "Could not locate OSRM profile '%s'. Please locate it manually and provide the full path.",
      profile
    ),
    call. = FALSE
  )
}
