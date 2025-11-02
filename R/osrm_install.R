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
#' When installing OSRM v6.x for Windows, the upstream release omits the Intel
#' Threading Building Blocks (TBB) runtime and a compatible `bz2` DLL. To keep
#' the executables runnable out of the box, `osrm_install()` fetches TBB from
#' \href{https://github.com/uxlfoundation/oneTBB/releases/tag/v2022.3.0}{oneTBB
#' v2022.3.0} and the BZip2 runtime from
#' \href{https://github.com/philr/bzip2-windows/releases/tag/v1.0.8.0}{bzip2-windows
#' v1.0.8.0}, verifying their SHA-256 checksums before extraction. Without these
#' extra libraries, the OSRM v6 binaries shipped for Windows cannot start.
#'
#' On macOS, OSRM v6.x binaries also miss the bundled TBB runtime. The installer
#' reuses the libraries from release `v5.27.1` to keep the binaries functional
#' and patches their `libbz2` linkage using `install_name_tool` so that they load
#' the system-provided BZip2 runtime.
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

  # Validate requested tag exists for this platform before hitting the API.
  if (!identical(requested_version, "latest")) {
    available_versions <- tryCatch(
      osrm_check_available_versions(prereleases = TRUE),
      error = identity
    )

    if (inherits(available_versions, "error")) {
      warning(
        sprintf(
          "Unable to verify requested version '%s' against available releases: %s",
          version,
          available_versions$message
        ),
        call. = FALSE
      )
    } else if (!version %in% available_versions) {
      stop(
        sprintf(
          "Version '%s' is not available for this platform. Run osrm_check_available_versions(prereleases = TRUE) to list supported tags.",
          version
        ),
        call. = FALSE
      )
    }
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
  maybe_install_macos_v6_runtime(runtime_version, platform, dest_dir)

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
get_platform_info <- function(sys_info = Sys.info()) {
  if (is.null(sys_info)) {
    sys_info <- list()
  }

  sys_info <- as.list(sys_info)
  sysname <- sys_info[["sysname"]]
  if (!is.null(sysname)) {
    sysname <- as.character(sysname)[1]
  }

  machine <- sys_info[["machine"]]
  if (!is.null(machine)) {
    machine <- as.character(machine)[1]
  }

  sysname_key <- tolower(sysname)
  os <- switch(
    sysname_key,
    linux = "linux",
    darwin = "darwin",
    windows = "win32"
  )

  normalized_machine <- NULL
  if (!is.null(machine) && isTRUE(nzchar(machine))) {
    normalized_machine <- tolower(machine)
    normalized_machine <- gsub("[^a-z0-9]+", "_", normalized_machine)
    normalized_machine <- gsub("^_+|_+$", "", normalized_machine)
  }

  arch_map <- c(
    x86_64 = "x64",
    amd64 = "x64",
    x64 = "x64",
    x86 = "x86",
    i386 = "x86",
    i686 = "x86",
    aarch64 = "arm64",
    arm64 = "arm64"
  )
  arch <- arch_map[[normalized_machine]]

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
      if (!is.null(override_os)) override_os else sysname,
      ", Arch: ",
      if (!is.null(override_arch)) override_arch else machine,
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

  matching_assets <- Filter(
    function(asset) grepl(pattern, asset$name, ignore.case = TRUE),
    release_info$assets
  )

  if (!length(matching_assets)) {
    stop(
      "Could not find a compatible binary for your platform in release '",
      release_info$tag_name,
      "'.\nLooked for asset name matching pattern: ",
      pattern,
      call. = FALSE
    )
  }

  node_versions <- vapply(
    matching_assets,
    function(asset) {
      matches <- regexec("node-v([0-9]+)", asset$name, ignore.case = TRUE)
      captured <- regmatches(asset$name, matches)[[1]]
      if (length(captured) >= 2) {
        return(suppressWarnings(as.integer(captured[2])))
      }
      NA_integer_
    },
    integer(1)
  )

  if (all(is.na(node_versions))) {
    selected_asset <- matching_assets[[1]]
  } else {
    max_node <- max(node_versions, na.rm = TRUE)
    idx <- which(node_versions == max_node)[1]
    selected_asset <- matching_assets[[idx]]
  }

  selected_asset$browser_download_url
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

  allow_tar_warnings_env <- tolower(Sys.getenv(
    "OSRM_BACKEND_ALLOW_TAR_WARNINGS",
    unset = ""
  ))
  suppress_tar_warnings <- !allow_tar_warnings_env %in% c("1", "true", "yes")
  run_untar <- function(arg_list) {
    if (suppress_tar_warnings) {
      suppressWarnings(do.call(utils::untar, arg_list))
    } else {
      do.call(utils::untar, arg_list)
    }
  }

  list_args <- list(tarfile = tmp_tarball, list = TRUE)
  list_attempts <- list(list_args)
  if (.Platform$OS.type == "windows") {
    list_attempts[[1]]$tar <- "internal"
    list_attempts[[2]] <- list_args
  }

  tar_members <- NULL
  last_list_error <- NULL
  for (args in list_attempts) {
    tar_members <- tryCatch(
      run_untar(args),
      error = function(e) {
        last_list_error <<- e
        NULL
      }
    )
    if (!is.null(tar_members)) {
      break
    }
  }

  if (is.null(tar_members)) {
    stop(
      "Failed to inspect source tarball for profiles: ",
      last_list_error$message,
      call. = FALSE
    )
  }

  # Extract only the profiles subtree to dodge Windows tar path limitations.
  profile_members <- tar_members[
    grepl("(^|/)profiles(/|$)", tar_members)
  ]

  if (length(profile_members) == 0) {
    stop(
      "The release tarball did not contain a 'profiles' directory.",
      call. = FALSE
    )
  }

  profile_dirs <- unique(
    sub("^(.*?/profiles)(?:/.*)?$", "\\1", profile_members, perl = TRUE)
  )
  profile_dirs <- profile_dirs[nzchar(profile_dirs)]
  extract_members <- unique(c(profile_members, profile_dirs))

  message("Extracting profiles...")
  untar_args <- list(
    tarfile = tmp_tarball,
    files = extract_members,
    exdir = tmp_profiles_extract
  )
  untar_attempts <- list(untar_args)
  if (.Platform$OS.type == "windows") {
    untar_attempts[[1]]$tar <- "internal"
    untar_attempts[[2]] <- untar_args
  }

  last_extract_error <- NULL
  extracted <- FALSE
  for (args in untar_attempts) {
    tryCatch(
      {
        run_untar(args)
        extracted <- TRUE
      },
      error = function(e) {
        last_extract_error <<- e
      }
    )
    if (isTRUE(extracted)) {
      break
    }
  }

  if (!isTRUE(extracted)) {
    stop(
      "Failed to extract profiles archive: ",
      last_extract_error$message,
      call. = FALSE
    )
  }

  profile_dirs_found <- list.dirs(
    tmp_profiles_extract,
    recursive = TRUE,
    full.names = TRUE
  )
  profile_dirs_found <- profile_dirs_found[
    basename(profile_dirs_found) == "profiles"
  ]
  if (length(profile_dirs_found) == 0) {
    stop(
      "The release tarball did not contain a 'profiles' directory.",
      call. = FALSE
    )
  }
  profile_source <- profile_dirs_found[[which.min(nchar(profile_dirs_found))]]

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
  download_zip_asset(
    url = tbb_url,
    member_path = "oneapi-tbb-2022.3.0/redist/intel64/vc14/tbb12.dll",
    dest_path = file.path(dest_dir, "tbb12.dll"),
    sha256 = "e1b2373f25558bf47d16b4c89cf0a31e6689aaf7221400d209e8527afc7c9eee"
  )
  message("  - Installed tbb12.dll")

  bzip_url <- paste0(
    "https://github.com/philr/bzip2-windows/releases/download/v1.0.8.0/",
    "bzip2-dll-1.0.8.0-win-x64.zip"
  )
  download_zip_asset(
    url = bzip_url,
    member_path = "libbz2.dll",
    dest_path = file.path(dest_dir, "bz2.dll"),
    sha256 = "50340fece047960f49cf869034c778ff9f6af27dde2f1ea9773cd89ddb326254"
  )
  message("  - Installed bz2.dll")

  invisible(dest_dir)
}

#' @noRd
maybe_install_macos_v6_runtime <- function(version, platform, dest_dir) {
  if (!identical(platform$os, "darwin")) {
    return(invisible(NULL))
  }

  if (!version_at_least(version, "v6.0.0")) {
    return(invisible(NULL))
  }

  ensure_macos_tbb_runtime(dest_dir, platform)
  patch_macos_bzip2_rpath(dest_dir)
}

#' @noRd
ensure_macos_tbb_runtime <- function(dest_dir, platform, reference_version = "v5.27.1") {
  required_libs <- c(
    "libtbbmalloc.dylib",
    "libtbbmalloc.2.3.dylib",
    "libtbbmalloc_proxy.2.3.dylib",
    "libtbb.dylib",
    "libtbb.12.3.dylib",
    "libtbbmalloc.2.dylib",
    "libtbbmalloc_proxy.dylib",
    "libtbbmalloc_proxy.2.dylib",
    "libtbb.12.dylib"
  )

  missing_libs <- required_libs[!file.exists(file.path(dest_dir, required_libs))]
  if (length(missing_libs)) {
    message(
      "Fetching macOS TBB runtime components from OSRM release ",
      reference_version,
      "..."
    )
  } else {
    message(
      "Refreshing macOS TBB runtime components from OSRM release ",
      reference_version,
      "..."
    )
  }

  release_info <- get_release_by_tag(reference_version)
  asset_url <- find_asset_url(release_info, platform)

  tmp_tar <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tmp_tar), add = TRUE)

  tryCatch(
    {
      req <- httr2::request(asset_url)
      httr2::req_perform(req, path = tmp_tar)
    },
    error = function(e) {
      stop(
        "Failed to download macOS TBB runtime from OSRM release ",
        reference_version,
        ": ",
        e$message,
        call. = FALSE
      )
    }
  )

  tmp_extract_dir <- tempfile()
  dir.create(tmp_extract_dir)
  on.exit(unlink(tmp_extract_dir, recursive = TRUE), add = TRUE)

  tryCatch(
    utils::untar(tmp_tar, exdir = tmp_extract_dir),
    error = function(e) {
      stop(
        "Failed to extract OSRM release ",
        reference_version,
        " while retrieving macOS TBB runtime: ",
        e$message,
        call. = FALSE
      )
    }
  )

  extracted_files <- list.files(tmp_extract_dir, recursive = TRUE, full.names = TRUE)

  locate_lib <- function(lib) {
    matches <- extracted_files[tolower(basename(extracted_files)) == tolower(lib)]
    if (!length(matches)) {
      return(NA_character_)
    }
    matches[[1]]
  }

  lib_sources <- vapply(required_libs, locate_lib, FUN.VALUE = character(1))

  if (anyNA(lib_sources)) {
    missing <- required_libs[is.na(lib_sources)]
    stop(
      "Could not locate required TBB libraries in OSRM release ",
      reference_version,
      ": ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  dest_paths <- file.path(dest_dir, required_libs)
  copied <- mapply(
    function(src, dest) {
      if (is.na(src)) {
        return(TRUE)
      }
      file.copy(src, dest, overwrite = TRUE)
    },
    lib_sources,
    dest_paths,
    USE.NAMES = FALSE
  )

  if (!all(copied)) {
    stop(
      "Failed to copy one or more macOS TBB runtime libraries into ",
      dest_dir,
      call. = FALSE
    )
  }

  message("Installed macOS TBB runtime libraries.")
  invisible(dest_dir)
}

#' @noRd
patch_macos_bzip2_rpath <- function(dest_dir) {
  tool_path <- Sys.which("install_name_tool")
  if (!nzchar(tool_path)) {
    stop(
      "Required tool 'install_name_tool' not found in PATH; cannot rewrite macOS BZip2 linkage.",
      call. = FALSE
    )
  }

  binaries <- file.path(
    dest_dir,
    c(
      "osrm-routed",
      "osrm-partition",
      "osrm-extract",
      "osrm-datastore",
      "osrm-customize",
      "osrm-contract",
      "osrm-components"
    )
  )
  binaries <- binaries[file.exists(binaries)]

  if (!length(binaries)) {
    warning(
      "No OSRM executables found to patch in ",
      dest_dir,
      call. = FALSE
    )
    return(invisible(dest_dir))
  }

  message("Updating macOS BZip2 linkage...")
  for (bin in binaries) {
    status <- system2(
      tool_path,
      args = c(
        "-change",
        "@rpath/libbz2.1.dylib",
        "/usr/lib/libbz2.1.0.dylib",
        bin
      )
    )

    if (!identical(status, 0L)) {
      stop(
        sprintf(
          "install_name_tool failed for '%s' with exit status %s.",
          basename(bin),
          status
        ),
        call. = FALSE
      )
    }
  }

  invisible(dest_dir)
}

#' @noRd
version_at_least <- function(version, minimum) {
  normalize_version <- function(x) {
    if (is.null(x) || !length(x)) {
      return(NA_character_)
    }
    x <- as.character(x)[1]
    x <- trimws(x)
    if (!nzchar(x)) {
      return(NA_character_)
    }
    x <- tolower(x)
    sub("^v", "", x)
  }

  v_norm <- normalize_version(version)
  min_norm <- normalize_version(minimum)

  if (anyNA(c(v_norm, min_norm))) {
    return(FALSE)
  }

  cmp <- suppressWarnings(utils::compareVersion(v_norm, min_norm))
  if (is.na(cmp)) {
    return(FALSE)
  }

  cmp >= 0
}

#' @noRd
# Download a zip archive, verify its checksum, and extract a single file.
download_zip_asset <- function(url, member_path, dest_path, sha256 = NULL) {
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

  if (!is.null(sha256) && nzchar(sha256)) {
    if (!requireNamespace("digest", quietly = TRUE)) {
      stop(
        "Package 'digest' is required to verify download checksums.",
        call. = FALSE
      )
    }
    expected <- tolower(gsub("[^0-9a-f]", "", sha256))
    actual <- digest::digest(file = tmp_zip, algo = "sha256", serialize = FALSE)
    if (!identical(actual, expected)) {
      stop(
        sprintf(
          "SHA-256 checksum mismatch for '%s'. Expected %s but got %s.",
          basename(url),
          expected,
          actual
        ),
        call. = FALSE
      )
    }
  }

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
