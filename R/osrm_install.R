#' Install OSRM Backend Binaries
#'
#' Downloads and installs pre-compiled binaries for the OSRM backend from the
#' official GitHub releases. The function automatically detects the user's
#' operating system and architecture to download the appropriate files.
#'
#' @details
#' The function performs the following steps:
#' 1.  Queries the GitHub API to find the specified release of `Project-OSRM/osrm-backend`.
#' 2.  Identifies the correct binary (`.tar.gz` archive) for the user's OS (Linux, macOS, or Windows) and architecture (x64, arm64).
#' 3.  Downloads the archive to a temporary location.
#' 4.  Extracts the archive and locates the OSRM executables (e.g., `osrm-routed`, `osrm-extract`).
#' 5.  Copies these executables to a local directory (defaults to `tools::R_user_dir("osrm.backend", which = "cache")`).
#' 6.  Optionally modifies the `PATH` environment variable for the current session or project.
#'
#' @param version A string specifying the OSRM version tag to install.
#'   Defaults to `"v5.27.1"`. Use `"latest"` to automatically find the most
#'   recent stable version by calling `osrm_check_latest_version()`.
#' @param dest_dir A string specifying the directory where OSRM binaries should be installed.
#'   If `NULL` (the default), a user-friendly, persistent location is chosen via
#'   `tools::R_user_dir("osrm.backend", which = "cache")`.
#' @param force A logical value. If `TRUE`, reinstall OSRM even if it's already found in `dest_dir`.
#'   If `FALSE` (default), the function will stop if an existing installation is detected.
#' @param path_action A string specifying how to handle the system `PATH`. One of:
#'   \itemize{
#'     \item `"session"` (default): Adds the OSRM bin directory to the `PATH` for the current R session only.
#'     \item `"project"`: Modifies the `.Rprofile` in the current project to set the `PATH` for all future sessions in that project.
#'     \item `"none"`: Does not modify the `PATH`.
#'   }
#' @param quiet A logical value. If `TRUE`, suppresses the confirmation prompt when `path_action = "project"`.
#'   Defaults to `FALSE`.
#' @return The path to the installation directory, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Install the default stable version and set PATH for this session
#' osrm_install()
#'
#' # Install for a project non-interactively (e.g., in a script)
#' osrm_install(path_action = "project", quiet = TRUE)
#'
#' # Clean up the project's .Rprofile
#' osrm_clear_path()
#' }
osrm_install <- function(
  version = "v5.27.1",
  dest_dir = NULL,
  force = FALSE,
  path_action = c("session", "project", "none"),
  quiet = FALSE
) {
  path_action <- match.arg(path_action)

  # --- 1. Determine destination directory ---
  if (is.null(dest_dir)) {
    dest_dir <- tools::R_user_dir("osrm.backend", which = "cache")
  }
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  dest_dir <- normalizePath(dest_dir)

  # --- 2. Check for existing installation ---
  existing_bin <- file.path(dest_dir, "osrm-routed")
  if (.Platform$OS.type == "windows") {
    existing_bin <- paste0(existing_bin, ".exe")
  }
  if (file.exists(existing_bin) && !force) {
    message("OSRM backend already found in: ", dest_dir)
    message("Use force = TRUE to reinstall.")
    # Handle path and return
    handle_path_setting(path_action, dest_dir, quiet)
    return(invisible(dest_dir))
  }

  # --- 3. Determine version and get release info ---
  if (version == "latest") {
    message("Finding latest stable version with available binaries...")
    version <- osrm_check_latest_version()
    message("Latest stable version is '", version, "'")
  }
  release_info <- get_release_by_tag(version)

  # --- 4. Detect platform ---
  platform <- get_platform_info()
  message(sprintf("Detected platform: %s-%s", platform$os, platform$arch))
  message(sprintf(
    "Found release: %s (%s)",
    release_info$tag_name,
    release_info$name
  ))

  # --- 5. Find matching asset download URL ---
  asset_url <- find_asset_url(release_info, platform)
  message("Found matching binary: ", basename(asset_url))

  # --- 6. Download and extract ---
  message("Downloading from ", asset_url)
  tmp_file <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tmp_file), add = TRUE)

  tryCatch(
    {
      req <- httr2::request(asset_url)
      httr2::req_perform(req, path = tmp_file)
    },
    error = function(e) {
      stop("Failed to download file: ", e$message, call. = FALSE)
    }
  )

  tmp_extract_dir <- tempfile()
  dir.create(tmp_extract_dir)
  on.exit(unlink(tmp_extract_dir, recursive = TRUE), add = TRUE)
  message("Extracting binaries...")
  tryCatch(
    utils::untar(tmp_file, exdir = tmp_extract_dir),
    error = function(e) {
      stop("Failed to extract archive: ", e$message, call. = FALSE)
    }
  )

  # --- 7. Locate and install binaries ---
  bin_names <- c(
    "osrm-routed",
    "osrm-extract",
    "osrm-contract",
    "osrm-partition",
    "osrm-customize",
    "osrm-datastore"
  )
  if (.Platform$OS.type == "windows") {
    bin_names <- paste0(bin_names, ".exe")
  }

  all_files <- list.files(tmp_extract_dir, recursive = TRUE, full.names = TRUE)
  found_bins <- all_files[basename(all_files) %in% bin_names]

  if (length(found_bins) == 0) {
    stop(
      "Could not find OSRM binaries in the downloaded archive. The archive structure may have changed.",
      call. = FALSE
    )
  }

  bin_source_dir <- dirname(found_bins[1])
  files_to_copy <- list.files(bin_source_dir, full.names = TRUE)

  message("Installing binaries to ", dest_dir)
  file.copy(from = files_to_copy, to = dest_dir, overwrite = TRUE)

  # --- 8. Set permissions and update PATH ---
  installed_bins <- file.path(dest_dir, basename(files_to_copy))
  if (.Platform$OS.type != "windows") {
    message("Setting executable permissions...")
    Sys.chmod(
      installed_bins[grepl(paste(bin_names, collapse = "|"), installed_bins)],
      mode = "0755"
    )
  }

  handle_path_setting(path_action, dest_dir, quiet)

  # --- 9. Final verification and user message ---
  osrm_path_check <- Sys.which("osrm-routed")
  if (path_action != "none" && !nzchar(osrm_path_check)) {
    warning(
      "Installation completed, but 'osrm-routed' was not found on the PATH immediately. You may need to restart your R session.",
      call. = FALSE
    )
  } else if (path_action != "none") {
    message("\n✅ Installation successful!")
  } else {
    message("\n✅ Installation successful! Binaries are in ", dest_dir)
  }

  return(invisible(dest_dir))
}

#' Check for the Latest Stable OSRM Version
#'
#' Queries the GitHub API to find the most recent stable (non-pre-release)
#' version tag for the OSRM backend that has binaries available for the current platform.
#'
#' @return A string containing the latest version tag (e.g., `"v5.27.1"`).
#' @export
#' @examples
#' \dontrun{
#' latest_v <- osrm_check_latest_version()
#' print(latest_v)
#' }
osrm_check_latest_version <- function() {
  releases <- get_all_releases()
  platform <- get_platform_info()
  for (rel in releases) {
    if (!isTRUE(rel$prerelease) && release_has_binaries(rel, platform)) {
      return(rel$tag_name)
    }
  }
  stop(
    "Could not find any stable releases with binaries for your platform.",
    call. = FALSE
  )
}

#' Check for Available OSRM Versions
#'
#' Queries the GitHub API to get a list of all available version tags for the
#' OSRM backend that have binaries for the current platform.
#'
#' @param prereleases A logical value. If `TRUE`, include pre-release versions
#'   in the returned list. Defaults to `FALSE`.
#' @return A character vector of available version tags.
#' @export
#' @examples
#' \dontrun{
#' # Get all stable versions with binaries for this platform
#' stable_versions <- osrm_check_available_versions()
#' head(stable_versions)
#'
#' # Get all versions, including pre-releases, with binaries
#' all_versions <- osrm_check_available_versions(prereleases = TRUE)
#' head(all_versions)
#' }
osrm_check_available_versions <- function(prereleases = FALSE) {
  releases <- get_all_releases()
  platform <- get_platform_info()

  # Filter for releases that have binaries for the current platform
  releases_with_binaries <- Filter(
    function(rel) release_has_binaries(rel, platform),
    releases
  )

  if (!prereleases) {
    releases_with_binaries <- Filter(
      function(rel) !isTRUE(rel$prerelease),
      releases_with_binaries
    )
  }

  tags <- vapply(
    releases_with_binaries,
    function(rel) rel$tag_name,
    FUN.VALUE = character(1)
  )
  return(tags)
}

#' Clear OSRM Path from Project's .Rprofile
#'
#' Scans the `.Rprofile` file in the current project's root directory and
#' removes any lines that were added by `osrm_install()` to modify the `PATH`.
#'
#' @param quiet A logical value. If `TRUE`, suppresses messages. Defaults to `FALSE`.
#' @return Invisibly returns `TRUE` if the file was modified, `FALSE` otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Clean up the project's .Rprofile
#' osrm_clear_path()
#' }
osrm_clear_path <- function(quiet = FALSE) {
  r_profile_path <- file.path(getwd(), ".Rprofile")

  if (!file.exists(r_profile_path)) {
    if (!quiet) {
      message("No .Rprofile file found in the current project directory.")
    }
    return(invisible(FALSE))
  }

  lines <- readLines(r_profile_path, warn = FALSE)
  comment_tag <- "#added-by-r-pkg-osrm.backend"

  lines_to_keep <- lines[!grepl(comment_tag, lines, fixed = TRUE)]

  if (length(lines_to_keep) == length(lines)) {
    if (!quiet) {
      message(
        ".Rprofile does not contain any paths set by osrm.backend. No changes made."
      )
    }
    return(invisible(FALSE))
  }

  if (!quiet) {
    message("Removing osrm.backend PATH configuration from: ", r_profile_path)
  }
  writeLines(lines_to_keep, r_profile_path)
  if (!quiet) {
    message(
      "Successfully removed configuration. Please restart R for changes to take effect."
    )
  }

  return(invisible(TRUE))
}


# --- Helper functions (not exported) ---

#' @noRd
get_platform_info <- function() {
  os <- switch(
    Sys.info()[["sysname"]],
    Linux = "linux",
    Darwin = "darwin",
    Windows = "win32"
  )
  arch <- switch(
    Sys.info()[["machine"]],
    x86_64 = "x64",
    amd64 = "x64",
    aarch64 = "arm64",
    arm64 = "arm64"
  )
  if (is.null(os) || is.null(arch)) {
    stop(
      "Your platform is not supported by pre-compiled OSRM binaries. OS: ",
      Sys.info()[["sysname"]],
      ", Arch: ",
      Sys.info()[["machine"]],
      call. = FALSE
    )
  }
  list(os = os, arch = arch)
}

#' @noRd
get_all_releases <- function() {
  repo <- "Project-OSRM/osrm-backend"
  url <- paste0("https://api.github.com/repos/", repo, "/releases")

  req <- httr2::request(url) |>
    httr2::req_error(body = function(resp) httr2::resp_body_json(resp)$message)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("GitHub API request failed: ", e$message, call. = FALSE)
    }
  )

  httr2::resp_body_json(resp)
}

#' @noRd
get_release_by_tag <- function(version) {
  repo <- "Project-OSRM/osrm-backend"
  url <- paste0(
    "https://api.github.com/repos/",
    repo,
    "/releases/tags/",
    version
  )

  req <- httr2::request(url) |>
    httr2::req_error(body = function(resp) httr2::resp_body_json(resp)$message)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop("GitHub API request failed: ", e$message, call. = FALSE)
    }
  )

  httr2::resp_body_json(resp)
}

#' @noRd
release_has_binaries <- function(release, platform) {
  pattern <- sprintf(".*%s-%s-Release\\.tar\\.gz$", platform$os, platform$arch)
  for (asset in release$assets) {
    if (grepl(pattern, asset$name, ignore.case = TRUE)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' @noRd
find_asset_url <- function(release_info, platform) {
  pattern <- sprintf(".*%s-%s-Release\\.tar\\.gz$", platform$os, platform$arch)

  asset_url <- NULL
  for (asset in release_info$assets) {
    if (grepl(pattern, asset$name, ignore.case = TRUE)) {
      asset_url <- asset$browser_download_url
      break
    }
  }

  if (is.null(asset_url)) {
    stop(
      "Could not find a compatible binary for your platform in release '",
      release_info$tag_name,
      "'.\nLooked for asset name matching pattern: ",
      pattern,
      call. = FALSE
    )
  }
  return(asset_url)
}

#' @noRd
handle_path_setting <- function(action, dir, quiet = FALSE) {
  if (action == "session") {
    set_path_session(dir)
  } else if (action == "project") {
    set_path_project(dir, quiet)
  }
  # If "none", do nothing
}

#' @noRd
set_path_session <- function(dir) {
  path_sep <- .Platform$path.sep
  current_path <- Sys.getenv("PATH")

  path_elements <- strsplit(current_path, path_sep)[[1]]
  normalized_dir <- normalizePath(dir, mustWork = FALSE)
  normalized_paths <- normalizePath(path_elements, mustWork = FALSE)

  if (!normalized_dir %in% normalized_paths) {
    new_path <- paste(normalized_dir, current_path, sep = path_sep)
    Sys.setenv(PATH = new_path)
    message(sprintf("Added '%s' to PATH for this session.", normalized_dir))
  }
}

#' @noRd
set_path_project <- function(dir, quiet = FALSE) {
  r_profile_path <- file.path(getwd(), ".Rprofile")

  message("You chose to set the OSRM path for this project.")
  message("This will add a line to the following file: ", r_profile_path)

  if (interactive() && !quiet) {
    ans <- utils::askYesNo("Do you want to proceed?", default = TRUE)
    if (!isTRUE(ans)) {
      message("Aborted. .Rprofile was not modified.")
      return(invisible())
    }
  }

  comment_tag <- "#added-by-r-pkg-osrm.backend"
  line_to_add <- sprintf(
    'Sys.setenv(PATH = paste("%s", Sys.getenv("PATH"), sep = "%s")) %s',
    normalizePath(dir),
    .Platform$path.sep,
    comment_tag
  )

  if (!file.exists(r_profile_path)) {
    message("Creating new .Rprofile file.")
    writeLines(line_to_add, r_profile_path)
  } else {
    lines <- readLines(r_profile_path, warn = FALSE)
    # Check if our tag is already there to avoid duplicates
    if (any(grepl(comment_tag, lines, fixed = TRUE))) {
      message(
        ".Rprofile already contains OSRM path configuration. No changes made."
      )
      return(invisible())
    }
    message("Appending configuration to existing .Rprofile.")
    # Add a newline for separation if the file doesn't end with one
    if (length(lines) > 0 && nzchar(lines[length(lines)])) {
      lines <- c(lines, "")
    }
    lines <- c(lines, line_to_add)
    writeLines(lines, r_profile_path)
  }

  message(
    "\nSuccessfully modified .Rprofile. Please restart R for the changes to take effect."
  )
}
