#' Install OSRM Backend Binaries
#'
#' Downloads and installs pre-compiled binaries for the OSRM backend from the official GitHub releases. The function automatically detects the user's operating system and architecture to download the appropriate files. Only the latest v5 release (`v5.27.1`) and `v6.0.0` were manually tested and are known to work well; other releases available on GitHub can be installed but are not guranteed to function correctly.
#'
#' @details
#' The function performs the following steps:
#' 1.  Queries the GitHub API to find the specified release of `Project-OSRM/osrm-backend`.
#' 2.  Identifies the correct binary (`.tar.gz` archive) for the user's OS (Linux, macOS, or Windows) and architecture (x64, arm64).
#' 3.  Downloads the archive to a temporary location.
#' 4.  Extracts the archive and locates the OSRM executables (e.g., `osrm-routed`, `osrm-extract`).
#' 5.  Copies these executables to a local directory (defaults to `tools::R_user_dir("osrm.backend", which = "cache")`).
#' 6.  Downloads the matching Lua profiles from the release tarball and installs them alongside the binaries.
#' 7.  Optionally modifies the `PATH` environment variable for the current session or project.
#'
#' Power users (including package authors running cross-platform tests) can
#' override the auto-detected platform by setting the R options
#' `osrm.backend.override_os` and `osrm.backend.override_arch` (e.g.,
#' `options(osrm.backend.override_os = "linux", osrm.backend.override_arch = "arm64")`)
#' before calling `osrm_install()`. Overrides allow requesting binaries for any
#' OS and CPU combination that exists on the GitHub releases.
#'
#' @param version A string specifying the OSRM version tag to install.
#'   Defaults to `"v5.27.1"`. Use `"latest"` to automatically find the most
#'   recent stable version by calling `osrm_check_latest_version()`. Versions
#'   other than `v5.27.1` and `v6.0.0` will trigger a warning but are still
#'   attempted if binaries are available.
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
  profiles_dir <- file.path(dest_dir, "profiles")
  profiles_missing <- !dir.exists(profiles_dir)

  if (file.exists(existing_bin)) {
    if (!force && !profiles_missing) {
      message("OSRM backend already found in: ", dest_dir)
      message("Use force = TRUE to reinstall.")
      # Handle path and return
      handle_path_setting(path_action, dest_dir, quiet)
      return(invisible(dest_dir))
    }

    if (profiles_missing) {
      message(
        "Existing OSRM installation in ",
        dest_dir,
        " is missing Lua profiles. Reinstalling to restore them."
      )
    } else if (force) {
      message("force = TRUE; reinstalling OSRM backend in ", dest_dir)
    }
  }

  # --- 3. Determine version and get release info ---
  tested_versions <- c("v5.27.1", "v6.0.0")
  requested_version <- version
  if (version == "latest") {
    message("Finding latest stable version with available binaries...")
    version <- osrm_check_latest_version()
    message("Latest stable version is '", version, "'")
  }
  if (!version %in% tested_versions) {
    warning_message <- sprintf(
      "Version '%s' has not been validated by osrm.backend; only v5.27.1 and v6.0.0 are tested.",
      version
    )
    if (identical(requested_version, "latest")) {
      warning_message <- sprintf(
        "Latest available release '%s' has not been validated by osrm.backend; only v5.27.1 and v6.0.0 are tested.",
        version
      )
    }
    warning(warning_message, call. = FALSE)
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
  bin_regex <- "osrm-(routed|extract|contract|partition|customize|datastore)"

  all_files <- list.files(tmp_extract_dir, recursive = TRUE, full.names = TRUE)
  found_bins <- all_files[
    grepl(paste0("^", bin_regex, "(\\.exe)?$"), basename(all_files), ignore.case = TRUE)
  ]

  # Fallback for archives that place binaries alongside additional artefacts (e.g. node bindings)
  if (length(found_bins) == 0) {
    found_bins <- all_files[
      grepl(
        paste0(bin_regex, "(\\.exe)?$"),
        basename(all_files),
        ignore.case = TRUE
      )
    ]
  }

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

  runtime_version <- release_info$tag_name
  if (is.null(runtime_version) || !nzchar(runtime_version)) {
    runtime_version <- version
  }
  maybe_install_windows_v6_runtime(runtime_version, platform, dest_dir)

  # --- 8. Download and install Lua profiles ---
  install_profiles_for_release(release_info, dest_dir)

  # --- 9. Set permissions and update PATH ---
  installed_bins <- file.path(dest_dir, basename(files_to_copy))
  if (.Platform$OS.type != "windows") {
    message("Setting executable permissions...")
    Sys.chmod(
      installed_bins[
        grepl(paste0("^", bin_regex, "$"), basename(installed_bins), ignore.case = TRUE)
      ],
      mode = "0755"
    )
  }

  handle_path_setting(path_action, dest_dir, quiet)

  # --- 10. Final verification and user message ---
  osrm_path_check <- Sys.which("osrm-routed")
  if (path_action != "none" && !nzchar(osrm_path_check)) {
    warning(
      "Installation completed, but 'osrm-routed' was not found on the PATH immediately. You may need to restart your R session.",
      call. = FALSE
    )
  } else if (path_action != "none") {
    message("Installation successful!")
  } else {
    message("Installation successful! Binaries are in ", dest_dir)
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
  sys_info <- Sys.info()
  os <- switch(
    sys_info[["sysname"]],
    Linux = "linux",
    Darwin = "darwin",
    Windows = "win32"
  )
  arch <- switch(
    sys_info[["machine"]],
    x86_64 = "x64",
    amd64 = "x64",
    aarch64 = "arm64",
    arm64 = "arm64"
  )

  override_os <- getOption("osrm.backend.override_os")
  if (!is.null(override_os)) {
    if (length(override_os) != 1 || !is.character(override_os)) {
      override_os <- as.character(override_os)[1]
    }
    override_os <- trimws(override_os)
    if (!nzchar(override_os)) {
      stop(
        "Option 'osrm.backend.override_os' must be a non-empty string when set.",
        call. = FALSE
      )
    }
    os <- override_os
  }

  override_arch <- getOption("osrm.backend.override_arch")
  if (!is.null(override_arch)) {
    if (length(override_arch) != 1 || !is.character(override_arch)) {
      override_arch <- as.character(override_arch)[1]
    }
    override_arch <- trimws(override_arch)
    if (!nzchar(override_arch)) {
      stop(
        "Option 'osrm.backend.override_arch' must be a non-empty string when set.",
        call. = FALSE
      )
    }
    arch <- override_arch
  }

  if (is.null(os) || is.null(arch) || !nzchar(os) || !nzchar(arch)) {
    stop(
      "Your platform is not supported by pre-compiled OSRM binaries. OS: ",
      if (!is.null(override_os)) override_os else sys_info[["sysname"]],
      ", Arch: ",
      if (!is.null(override_arch)) override_arch else sys_info[["machine"]],
      ".",
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
install_profiles_for_release <- function(release_info, dest_dir) {
  tarball_url <- release_info$tarball_url

  if (is.null(tarball_url) || !nzchar(tarball_url)) {
    warning(
      "Release metadata did not include a tarball URL. Skipping installation of profiles.",
      call. = FALSE
    )
    return(invisible(NULL))
  }

  message("Downloading profiles from release tarball...")
  tmp_tarball <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tmp_tarball), add = TRUE)

  tryCatch(
    {
      req <- httr2::request(tarball_url)
      httr2::req_perform(req, path = tmp_tarball)
    },
    error = function(e) {
      stop(
        "Failed to download source tarball for profiles: ",
        e$message,
        call. = FALSE
      )
    }
  )

  tmp_profiles_extract <- tempfile()
  dir.create(tmp_profiles_extract)
  on.exit(unlink(tmp_profiles_extract, recursive = TRUE), add = TRUE)

  message("Extracting profiles...")
  tryCatch(
    utils::untar(tmp_tarball, exdir = tmp_profiles_extract),
    error = function(e) {
      stop("Failed to extract profiles archive: ", e$message, call. = FALSE)
    }
  )

  top_level_dirs <- list.dirs(
    tmp_profiles_extract,
    recursive = FALSE,
    full.names = TRUE
  )
  profile_source <- NULL
  for (root in top_level_dirs) {
    candidate <- file.path(root, "profiles")
    if (dir.exists(candidate)) {
      profile_source <- candidate
      break
    }
  }

  if (is.null(profile_source)) {
    stop(
      "The release tarball did not contain a 'profiles' directory.",
      call. = FALSE
    )
  }

  dest_profiles_dir <- file.path(dest_dir, "profiles")
  if (dir.exists(dest_profiles_dir)) {
    unlink(dest_profiles_dir, recursive = TRUE)
  }
  dir.create(dest_profiles_dir, recursive = TRUE, showWarnings = FALSE)

  files_to_copy <- list.files(
    profile_source,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  copied <- file.copy(
    from = files_to_copy,
    to = dest_profiles_dir,
    recursive = TRUE,
    overwrite = TRUE
  )

  if (!all(copied)) {
    stop(
      "Failed to copy one or more profile files to the destination directory.",
      call. = FALSE
    )
  }

  message("Installed profiles to ", dest_profiles_dir)
  invisible(dest_profiles_dir)
}

#' @noRd
maybe_install_windows_v6_runtime <- function(version, platform, dest_dir) {
  if (!identical(platform$os, "win32")) {
    return(invisible(NULL))
  }

  if (is.null(version) || !nzchar(version)) {
    return(invisible(NULL))
  }

  version_tag <- tolower(as.character(version)[1])
  if (is.na(version_tag) || !startsWith(version_tag, "v6")) {
    return(invisible(NULL))
  }

  install_windows_v6_runtime(dest_dir)
}

#' @noRd
install_windows_v6_runtime <- function(dest_dir) {
  message("Fetching additional Windows runtime dependencies (TBB, BZip2)...")

  tbb_url <- paste0(
    "https://github.com/uxlfoundation/oneTBB/releases/download/v2022.3.0/",
    "oneapi-tbb-2022.3.0-win.zip"
  )
  download_zip_member(
    url = tbb_url,
    member_path = "oneapi-tbb-2022.3.0/redist/intel64/vc14/tbb12.dll",
    dest_path = file.path(dest_dir, "tbb12.dll")
  )
  message("  - Installed tbb12.dll")

  bzip_url <- paste0(
    "https://github.com/philr/bzip2-windows/releases/download/v1.0.8.0/",
    "bzip2-dll-1.0.8.0-win-x64.zip"
  )
  download_zip_member(
    url = bzip_url,
    member_path = "libbz2.dll",
    dest_path = file.path(dest_dir, "bz2.dll")
  )
  message("  - Installed bz2.dll")

  invisible(dest_dir)
}

#' @noRd
download_zip_member <- function(url, member_path, dest_path) {
  member_path <- gsub("^/+", "", member_path)
  normalized_member <- gsub("\\\\", "/", member_path)

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  req <- httr2::request(url)
  tryCatch(
    httr2::req_perform(req, path = tmp_zip),
    error = function(e) {
      stop(
        sprintf("Failed to download '%s': %s", url, e$message),
        call. = FALSE
      )
    }
  )

  contents <- utils::unzip(tmp_zip, list = TRUE)
  if (is.null(contents) || nrow(contents) == 0) {
    stop(
      sprintf("Archive '%s' did not contain any files.", basename(url)),
      call. = FALSE
    )
  }

  matches <- contents$Name[tolower(contents$Name) == tolower(normalized_member)]
  if (length(matches) == 0) {
    stop(
      sprintf(
        "Archive '%s' did not contain expected file '%s'.",
        basename(url),
        member_path
      ),
      call. = FALSE
    )
  }

  selected_member <- matches[[1]]

  tmp_extract <- tempfile()
  dir.create(tmp_extract)
  on.exit(unlink(tmp_extract, recursive = TRUE), add = TRUE)

  tryCatch(
    utils::unzip(tmp_zip, files = selected_member, exdir = tmp_extract),
    error = function(e) {
      stop(
        sprintf(
          "Failed to extract '%s' from archive '%s': %s",
          selected_member,
          basename(url),
          e$message
        ),
        call. = FALSE
      )
    }
  )

  parts <- strsplit(selected_member, "/", fixed = TRUE)[[1]]
  src_path <- do.call(file.path, c(list(tmp_extract), parts))

  if (!file.exists(src_path)) {
    stop(
      sprintf("Extraction did not produce expected file '%s'.", src_path),
      call. = FALSE
    )
  }

  dest_parent <- dirname(dest_path)
  if (!dir.exists(dest_parent)) {
    dir.create(dest_parent, recursive = TRUE, showWarnings = FALSE)
  }

  if (!file.copy(src_path, dest_path, overwrite = TRUE)) {
    stop(
      sprintf("Failed to copy '%s' to '%s'.", src_path, dest_path),
      call. = FALSE
    )
  }

  invisible(dest_path)
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
