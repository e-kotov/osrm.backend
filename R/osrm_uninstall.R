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
#'   to choose a directory; otherwise, `dest_dir` must be supplied. Ignored if `all = TRUE`.
#' @param clear_path A logical value. If `TRUE` (default), also removes the
#'   `PATH` configuration from the project's `.Rprofile` by calling `osrm_clear_path()`.
#' @param quiet A logical value. If `TRUE`, suppresses informational messages
#'   and confirmation prompts. Defaults to `FALSE`.
#' @param all A logical value. If `TRUE`, removes all OSRM installations found
#'   in the default cache directory. Will prompt for confirmation unless `force = TRUE`.
#'   Defaults to `FALSE`. When `TRUE`, the `dest_dir` parameter is ignored.
#' @param force A logical value. If `TRUE`, skips all confirmation prompts,
#'   enabling non-interactive usage. Defaults to `FALSE`.
#' @return `TRUE` if one or more directories were successfully removed,
#'   and `FALSE` otherwise.
#' @export
#' @examples
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   # Install OSRM temporarily
#'   install_dir <- osrm_install(path_action = "session", quiet = TRUE)
#'
#'   # Uninstall that specific version and clear PATH changes
#'   osrm_uninstall(
#'     dest_dir = install_dir,
#'     clear_path = TRUE,
#'     force = TRUE,
#'     quiet = TRUE
#'   )
#'
#'   # If multiple installs exist, remove them all
#'   osrm_uninstall(all = TRUE, force = TRUE, quiet = TRUE)
#' }
#' }
osrm_uninstall <- function(dest_dir = NULL, clear_path = TRUE, quiet = FALSE, all = FALSE, force = FALSE) {
  quiet <- isTRUE(quiet)
  all <- isTRUE(all)
  force <- isTRUE(force)
  default_root <- osrm_default_install_root()
  default_root_display <- osrm_try_normalize_path(default_root)
  auto_selected <- FALSE

  # --- Handle 'all' parameter: remove all installations ---
  if (all) {
    if (!is.null(dest_dir) && !quiet) {
      warning(
        "The 'dest_dir' parameter is ignored when 'all = TRUE'.",
        call. = FALSE
      )
    }

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

    # Ask for confirmation to remove all
    if (!quiet) {
      message(sprintf("Found %d OSRM installation%s:",
                      length(version_installs),
                      if (length(version_installs) == 1) "" else "s"))
      for (p in version_installs) {
        message("  - ", p)
      }
    }

    proceed_all <- FALSE
    if (!force) {
      if (interactive() && !quiet) {
        ans <- utils::askYesNo(
          sprintf("Do you want to permanently remove all %d installation%s?",
                  length(version_installs),
                  if (length(version_installs) == 1) "" else "s"),
          default = FALSE
        )
        if (isTRUE(ans)) {
          proceed_all <- TRUE
        }
      } else {
        proceed_all <- TRUE
      }
    } else {
      proceed_all <- TRUE
    }

    if (!proceed_all) {
      if (!quiet) {
        message("Uninstall aborted by user.")
      }
      if (clear_path) {
        osrm_clear_path(quiet = quiet)
      }
      return(FALSE)
    }

    # Remove all installations
    any_removed <- FALSE
    for (install_dir in version_installs) {
      # Use OS-native path for file system ops, forward-slash for display
      install_dir_fs <- normalizePath(install_dir, mustWork = FALSE)
      install_dir_display <- normalizePath(install_dir, winslash = "/", mustWork = FALSE)

      if (!dir.exists(install_dir_fs)) {
        if (!quiet) {
          message("Skipping (not found): ", install_dir_display)
        }
        next
      }

      if (!quiet) {
        message("Removing: ", install_dir_display)
      }

      tryCatch(
        {
          unlink(install_dir_fs, recursive = TRUE, force = TRUE)
          if (!quiet) {
            message("  Successfully removed.")
          }
          any_removed <- TRUE
        },
        error = function(e) {
          if (!quiet) {
            warning(
              "Failed to remove ", install_dir_display, ": ",
              e$message,
              call. = FALSE
            )
          }
        }
      )
    }

    if (clear_path) {
      osrm_clear_path(quiet = quiet)
    }

    if (any_removed && !quiet) {
      message("Successfully uninstalled OSRM backend binaries.")
    }

    return(any_removed)
  }

  # --- Original single-installation logic ---
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

  dest_dir_fs <- normalizePath(dest_dir, mustWork = FALSE)
  dest_dir_display <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)

  # --- 1. Check if the directory exists ---
  dir_existed <- dir.exists(dest_dir_fs)
  if (!dir_existed) {
    if (!quiet) {
      message("OSRM installation not found at: ", dest_dir_display)
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
      message("Uninstalling OSRM from detected installation: ", dest_dir_display)
    }
  }

  proceed <- FALSE
  if (!force) {
    if (!quiet) {
      message("This will permanently remove the OSRM installation from:")
      message(dest_dir_display)
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
        unlink(dest_dir_fs, recursive = TRUE, force = TRUE)
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
