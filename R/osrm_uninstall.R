#' Uninstall OSRM Backend Binaries
#'
#' Removes the OSRM backend binaries and optionally clears the `PATH`
#' configuration from the project's `.Rprofile`.
#'
#' @param dest_dir A string specifying the directory from which to remove OSRM binaries.
#'   If `NULL` (the default), the function looks for an installation in the
#'   per-version subdirectories inside `tools::R_user_dir("osrm.backend", which = "cache")`
#'   and removes it. When multiple versions are installed, interactive sessions
#'   that are not `quiet` will be prompted (with a numbered menu and `0` to cancel)
#'   to choose a directory; otherwise, `dest_dir` must be supplied.
#' @param clear_path A logical value. If `TRUE` (default), also removes the
#'   `PATH` configuration from the project's `.Rprofile` by calling `osrm_clear_path()`.
#' @param quiet A logical value. If `TRUE`, suppresses informational messages
#'   and confirmation prompts. Defaults to `FALSE`.
#' @return `TRUE` if the directory was successfully removed,
#'   and `FALSE` otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Uninstall OSRM and clear .Rprofile (will ask for confirmation)
#' osrm_uninstall()
#'
#' # Only uninstall binaries, leave .Rprofile untouched
#' osrm_uninstall(clear_path = FALSE)
#' }
osrm_uninstall <- function(dest_dir = NULL, clear_path = TRUE, quiet = FALSE) {
  quiet <- isTRUE(quiet)
  default_root <- osrm_default_install_root()
  default_root_display <- osrm_try_normalize_path(default_root)
  auto_selected <- FALSE

  if (is.null(dest_dir)) {
    version_installs <- osrm_detect_installations(default_root)

    if (length(version_installs) == 0) {
      if (!quiet) {
        message("OSRM installation not found under: ", default_root_display)
      }
      if (clear_path) {
        osrm_clear_path(quiet = quiet)
      }
      return(FALSE)
    }

    if (length(version_installs) > 1) {
      if (interactive() && !quiet) {
        selection <- osrm_prompt_install_selection(version_installs)
        if (is.null(selection)) {
          if (!quiet) {
            message("Uninstall aborted; no selection made.")
          }
          if (clear_path) {
            osrm_clear_path(quiet = quiet)
          }
          return(FALSE)
        }
        dest_dir <- selection
        auto_selected <- TRUE
      } else {
        if (!quiet) {
          message("Multiple OSRM installations detected:")
          for (p in version_installs) {
            message("  - ", p)
          }
          message("Please supply dest_dir to choose which installation to remove.")
        }
        if (clear_path) {
          osrm_clear_path(quiet = quiet)
        }
        return(FALSE)
      }
    }

    if (is.null(dest_dir)) {
      dest_dir <- version_installs[[1]]
      auto_selected <- TRUE
    }
  }

  dest_dir_norm <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)

  # --- 1. Check if the directory exists ---
  dir_existed <- dir.exists(dest_dir_norm)
  if (!dir_existed) {
    if (!quiet) {
      message("OSRM installation not found at: ", dest_dir_norm)
      message("Nothing to uninstall.")
    }
    if (clear_path) {
      osrm_clear_path(quiet = quiet)
    }
    return(FALSE)
  }

  # --- 2. Ask for confirmation for file deletion ---
  if (auto_selected) {
    if (!quiet) {
      message("Uninstalling OSRM from detected installation: ", dest_dir_norm)
    }
  }

  proceed <- FALSE
  if (!quiet) {
    message("This will permanently remove the OSRM installation from:")
    message(dest_dir_norm)
  }
  if (interactive() && !quiet) {
    ans <- utils::askYesNo(
      "Do you want to proceed with deleting files?",
      default = FALSE
    )
    if (isTRUE(ans)) {
      proceed <- TRUE
    }
  } else {
    proceed <- TRUE
  }

  # --- 3. Perform deletion if confirmed ---
  uninstalled_ok <- FALSE
  if (proceed) {
    if (!quiet) {
      message("Removing directory...")
    }
    tryCatch(
      {
        unlink(dest_dir_norm, recursive = TRUE, force = TRUE)
        if (!quiet) {
          message("Successfully uninstalled OSRM backend binaries.")
        }
        uninstalled_ok <- TRUE
      },
      error = function(e) {
        if (!quiet) {
          warning(
            "An error occurred while trying to remove the directory: ",
            e$message,
            call. = FALSE
          )
        }
      }
    )
  } else {
    if (!quiet) {
      message("Uninstall of binaries aborted by user.")
    }
  }

  # --- 4. Clear path if requested ---
  if (clear_path) {
    osrm_clear_path(quiet = quiet)
  }

  return(uninstalled_ok)
}

#' @noRd
osrm_prompt_install_selection <- function(paths) {
  if (!length(paths)) {
    return(NULL)
  }

  repeat {
    message("Multiple OSRM installations detected:")
    for (i in seq_along(paths)) {
      message(sprintf("  [%d] %s", i, paths[[i]]))
    }
    message("  [0] Cancel uninstall")
    ans <- readline("Enter the number of the installation to remove: ")
    if (!nzchar(ans)) {
      next
    }
    choice <- suppressWarnings(as.integer(ans))
    if (is.na(choice)) {
      message("Please enter a number between 0 and ", length(paths), ".")
      next
    }
    if (choice == 0) {
      return(NULL)
    }
    if (choice < 0 || choice > length(paths)) {
      message("Please enter a number between 0 and ", length(paths), ".")
      next
    }
    return(paths[[choice]])
  }
}
