#' Uninstall OSRM Backend Binaries
#'
#' Removes the OSRM backend binaries and optionally clears the `PATH`
#' configuration from the project's `.Rprofile`.
#'
#' @param dest_dir A string specifying the directory from which to remove OSRM binaries.
#'   If `NULL` (the default), the default user cache directory is used.
#' @param clear_path A logical value. If `TRUE` (default), also removes the
#'   `PATH` configuration from the project's `.Rprofile` by calling `osrm_clear_path()`.
#' @param quiet A logical value. If `TRUE`, suppresses confirmation prompts.
#'   Defaults to `FALSE`.
#' @return Invisibly returns `TRUE` if the directory was successfully removed,
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
  # --- 1. Determine destination directory ---
  if (is.null(dest_dir)) {
    dest_dir <- tools::R_user_dir("osrm.backend", which = "cache")
  }
  dest_dir <- normalizePath(dest_dir, mustWork = FALSE)

  # --- 2. Check if the directory exists ---
  dir_existed <- dir.exists(dest_dir)
  if (!dir_existed) {
    message("OSRM installation not found at: ", dest_dir)
    message("Nothing to uninstall.")
  }

  # --- 3. Ask for confirmation for file deletion ---
  proceed <- FALSE
  if (dir_existed) {
    message("This will permanently remove the OSRM installation from:")
    message(dest_dir)
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
  }

  # --- 4. Perform deletion if confirmed ---
  uninstalled_ok <- FALSE
  if (proceed) {
    message("Removing directory...")
    tryCatch(
      {
        unlink(dest_dir, recursive = TRUE, force = TRUE)
        message("✅ Successfully uninstalled OSRM backend binaries.")
        uninstalled_ok <- TRUE
      },
      error = function(e) {
        warning(
          "An error occurred while trying to remove the directory: ",
          e$message,
          call. = FALSE
        )
      }
    )
  } else if (dir_existed) {
    message("Uninstall of binaries aborted by user.")
  }

  # --- 5. Clear path if requested ---
  if (clear_path) {
    osrm_clear_path(quiet = quiet)
  }

  return(invisible(uninstalled_ok))
}
