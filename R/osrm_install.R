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
#' 5.  Copies these executables to a local directory (defaults to
#'     `file.path(tools::R_user_dir("osrm.backend", which = "cache"), <version>)`).
#' 6.  Downloads the matching Lua profiles from the release tarball and installs them alongside the binaries.
#' 7.  Optionally modifies the `PATH` environment variable for the current session or project.
#'
#' macOS users should note that upstream OSRM v6.x binaries are built for macOS 15.0 (Sequoia, Darwin 24.0.0) or newer.
#' `osrm_install()` automatically blocks v6 installs on older macOS releases and, when `version = "latest"`,
#' selects the most recent v5 build instead while warning about the requirement. Warnings include both the
#' marketing version and Darwin kernel so you'll see messages like `macOS 13 Ventura [Darwin 22.6.0]`.
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
#'   Defaults to `"latest"`. Use `"latest"` to automatically find the most
#'   recent stable version (internally calls [osrm_check_latest_version()]). Versions
#'   other than `v5.27.1` and `v6.0.0` will trigger a warning but are still
#'   attempted if binaries are available.
#' @param dest_dir A string specifying the directory where OSRM binaries should be installed.
#'   If `NULL` (the default), a user-friendly, persistent location is chosen via
#'   `tools::R_user_dir("osrm.backend", which = "cache")`, and the binaries are installed
#'   into a subdirectory named after the OSRM version (e.g. `.../cache/v6.0.0`).
#' @param force A logical value. If `TRUE`, reinstall OSRM even if it's already found in `dest_dir`.
#'   If `FALSE` (default), the function will stop if an existing installation is detected.
#' @param path_action A string specifying how to handle the system `PATH`. One of:
#'   \itemize{
#'     \item `"session"` (default): Adds the OSRM bin directory to the `PATH` for the current R session only.
#'     \item `"project"`: Modifies the `.Rprofile` in the current project to set the `PATH` for all future sessions in that project.
#'     \item `"none"`: Does not modify the `PATH`.
#'   }
#' @param quiet A logical value. If `TRUE`, suppresses installer messages and
#'   warnings. Defaults to `FALSE`.
#' @return The path to the installation directory.
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
  version = "latest",
  dest_dir = NULL,
  force = FALSE,
  path_action = c("session", "project", "none"),
  quiet = FALSE
) {
  quiet <- isTRUE(quiet)
  emit_message <- function(...) {
    if (!quiet) {
      message(...)
    }
  }
  emit_warning <- function(...) {
    if (!quiet) {
      warning(...)
    }
  }
  path_action <- match.arg(path_action)
  sys_info <- Sys.info()
  platform <- get_platform_info(sys_info = sys_info)
  mac_release_info <- get_macos_release_info(sys_info)

  # --- 1. Determine installation root (final directory depends on version) ---
  using_default_dest <- is.null(dest_dir)
  install_root <- if (using_default_dest) {
    osrm_default_install_root()
  } else {
    dest_dir
  }

  # --- 2. Determine version and get release info ---
  tested_versions <- c("v5.27.1", "v6.0.0")
  requested_version <- version
  mac_release_display <- mac_release_info$display_name
  if (is.null(mac_release_display) || !nzchar(mac_release_display)) {
    raw_release <- mac_release_info$release
    if (!is.na(raw_release) && nzchar(raw_release)) {
      mac_release_display <- paste0("Darwin ", raw_release)
    } else {
      mac_release_display <- "unknown macOS release"
    }
  }
  mac_required_display <- "macOS 15.0 (Sequoia) [Darwin 24]"
  is_macos <- identical(platform$os, "darwin")
  mac_too_old_for_v6 <- is_macos &&
    isFALSE(mac_release_info$meets_v6_requirement)

  if (identical(version, "latest")) {
    emit_message("Finding latest stable version with available binaries...")
    if (mac_too_old_for_v6) {
      version <- find_latest_pre_v6_release(platform)
      emit_message("Latest compatible version is '", version, "'")
      emit_warning(
        sprintf(
          "%s detected. OSRM v6.x requires %s or newer. Upgrade macOS to install v6.",
          mac_release_display,
          mac_required_display
        ),
        call. = FALSE
      )
    } else {
      version <- osrm_check_latest_version()
      emit_message("Latest stable version is '", version, "'")
    }
  }

  if (mac_too_old_for_v6 && version_at_least(version, "v6.0.0")) {
    stop(
      sprintf(
        "%s detected. OSRM v6.x requires %s or newer. Please install a v5.x release instead.",
        mac_release_display,
        mac_required_display
      ),
      call. = FALSE
    )
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
    emit_warning(warning_message, call. = FALSE)
  }

  # Validate requested tag exists for this platform before hitting the API.
  if (!identical(requested_version, "latest")) {
    available_versions <- tryCatch(
      osrm_check_available_versions(prereleases = TRUE),
      error = identity
    )

    if (inherits(available_versions, "error")) {
      emit_warning(
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

  # --- 3. Resolve final destination directory for this version ---
  if (using_default_dest) {
    dest_dir <- file.path(install_root, version)
  } else {
    dest_dir <- install_root
  }
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  dest_dir <- normalizePath(dest_dir, mustWork = FALSE)

  bin_candidates <- file.path(dest_dir, c("osrm-routed", "osrm-routed.exe"))
  has_existing_install <- any(file.exists(bin_candidates))
  profiles_dir <- file.path(dest_dir, "profiles")
  profiles_missing <- !dir.exists(profiles_dir)

  if (has_existing_install) {
    if (!force && !profiles_missing) {
      emit_message("OSRM backend already found in: ", dest_dir)
      emit_message("Use force = TRUE to reinstall.")
      # Handle path and return
      handle_path_setting(path_action, dest_dir, quiet)
      return(dest_dir)
    }

    if (profiles_missing) {
      emit_message(
        "Existing OSRM installation in ",
        dest_dir,
        " is missing Lua profiles. Reinstalling to restore them."
      )
    } else if (force) {
      emit_message("force = TRUE; reinstalling OSRM backend in ", dest_dir)
    }
  }

  # --- 4. Fetch release metadata ---
  release_info <- get_release_by_tag(version)

  # --- 5. Detect platform ---
  emit_message(sprintf("Detected platform: %s-%s", platform$os, platform$arch))
  emit_message(sprintf(
    "Found release: %s (%s)",
    release_info$tag_name,
    release_info$name
  ))

  # --- 6. Find matching asset download URL ---
  asset_url <- find_asset_url(release_info, platform)
  emit_message("Found matching binary: ", basename(asset_url))

  # --- 7. Download and extract ---
  emit_message("Downloading from ", asset_url)
  tmp_file <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tmp_file), add = TRUE)

  tryCatch(
    {
      req <- httr2::req_retry(httr2::request(asset_url), max_tries = 3)
      httr2::req_perform(req, path = tmp_file)
    },
    error = function(e) {
      stop("Failed to download file: ", e$message, call. = FALSE)
    }
  )

  tmp_extract_dir <- tempfile()
  dir.create(tmp_extract_dir)
  on.exit(unlink(tmp_extract_dir, recursive = TRUE), add = TRUE)
  emit_message("Extracting binaries...")
  tryCatch(
    utils::untar(tmp_file, exdir = tmp_extract_dir),
    error = function(e) {
      stop("Failed to extract archive: ", e$message, call. = FALSE)
    }
  )

  # --- 8. Locate and install binaries ---
  bin_regex <- "osrm-(routed|extract|contract|partition|customize|datastore)"

  all_files <- list.files(tmp_extract_dir, recursive = TRUE, full.names = TRUE)
  found_bins <- all_files[
    grepl(
      paste0("^", bin_regex, "(\\.exe)?$"),
      basename(all_files),
      ignore.case = TRUE
    )
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

  emit_message("Installing binaries to ", dest_dir)
  file.copy(from = files_to_copy, to = dest_dir, overwrite = TRUE)

  runtime_version <- release_info$tag_name
  if (is.null(runtime_version) || !nzchar(runtime_version)) {
    runtime_version <- version
  }
  maybe_install_windows_v6_runtime(
    runtime_version,
    platform,
    dest_dir,
    quiet = quiet
  )
  maybe_install_linux_v6_runtime(
    runtime_version,
    platform,
    dest_dir,
    quiet = quiet
  )
  maybe_install_macos_v6_runtime(
    runtime_version,
    platform,
    dest_dir,
    quiet = quiet
  )

  # --- 9. Download and install Lua profiles ---
  install_profiles_for_release(release_info, dest_dir, quiet = quiet)

  # --- 10. Set permissions and update PATH ---
  installed_bins <- file.path(dest_dir, basename(files_to_copy))
  if (.Platform$OS.type != "windows") {
    emit_message("Setting executable permissions...")
    Sys.chmod(
      installed_bins[
        grepl(
          paste0("^", bin_regex, "$"),
          basename(installed_bins),
          ignore.case = TRUE
        )
      ],
      mode = "0755"
    )
  }

  handle_path_setting(path_action, dest_dir, quiet)

  # --- 11. Final verification and user message ---
  osrm_path_check <- Sys.which("osrm-routed")
  if (path_action != "none" && !nzchar(osrm_path_check)) {
    emit_warning(
      "Installation completed, but 'osrm-routed' was not found on the PATH immediately. You may need to restart your R session.",
      call. = FALSE
    )
  } else if (path_action != "none") {
    emit_message("Installation successful!")
  } else {
    emit_message("Installation successful! Binaries are in ", dest_dir)
  }

  return(dest_dir)
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

#' @noRd
find_latest_pre_v6_release <- function(platform) {
  releases <- get_all_releases()
  for (rel in releases) {
    if (isTRUE(rel$prerelease)) {
      next
    }
    if (!release_has_binaries(rel, platform)) {
      next
    }
    if (version_at_least(rel$tag_name, "v6.0.0")) {
      next
    }
    return(rel$tag_name)
  }

  stop(
    "No pre-v6 releases with compatible binaries were found for this platform. Upgrade macOS to install OSRM v6.x or later.",
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
#' @return `TRUE` if the file was modified, `FALSE` otherwise.
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
    return(FALSE)
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
    return(FALSE)
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

  return(TRUE)
}


# --- Helper functions (not exported) ---

#' @noRd
osrm_default_install_root <- function() {
  tools::R_user_dir("osrm.backend", which = "cache")
}

#' @noRd
osrm_try_normalize_path <- function(path) {
  if (!file.exists(path)) {
    return(path)
  }
  tryCatch(
    normalizePath(path),
    error = function(...) path
  )
}

#' @noRd
osrm_is_install_dir <- function(path) {
  if (is.null(path) || !length(path)) {
    return(FALSE)
  }
  candidate <- osrm_try_normalize_path(path)
  if (!dir.exists(candidate)) {
    return(FALSE)
  }
  bin_candidates <- file.path(candidate, c("osrm-routed", "osrm-routed.exe"))
  any(file.exists(bin_candidates))
}

#' @noRd
osrm_detect_installations <- function(root = osrm_default_install_root()) {
  root_norm <- osrm_try_normalize_path(root)
  installs <- character(0)

  if (dir.exists(root_norm)) {
    entries <- dir(
      root_norm,
      full.names = TRUE,
      recursive = FALSE,
      include.dirs = TRUE,
      no.. = TRUE
    )
    if (length(entries)) {
      info <- file.info(entries)
      dirs <- entries[!is.na(info$isdir) & info$isdir]
      if (length(dirs)) {
        dir_norms <- vapply(dirs, osrm_try_normalize_path, character(1))
        valid <- vapply(dir_norms, osrm_is_install_dir, logical(1))
        installs <- unique(dir_norms[valid])
      }
    }
  }

  installs
}

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

  if (is.null(sysname) || !nzchar(sysname)) {
    os <- NULL
  } else {
    sysname_key <- tolower(sysname)
    os <- switch(
      sysname_key,
      linux = "linux",
      darwin = "darwin",
      windows = "win32"
    )
  }

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
  arch <- if (!is.null(normalized_machine)) arch_map[[normalized_machine]]

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
      sprintf(
        "Your platform is not supported by pre-compiled OSRM binaries. OS: %s, Arch: %s.",
        if (!is.null(override_os)) {
          override_os
        } else if (!is.null(sysname)) {
          sysname
        } else {
          "unknown"
        },
        if (!is.null(override_arch)) {
          override_arch
        } else if (!is.null(machine)) {
          machine
        } else {
          "unknown"
        }
      ),
      call. = FALSE
    )
  }

  list(os = os, arch = arch)
}

#' @noRd
get_macos_release_info <- function(sys_info) {
  if (is.null(sys_info)) {
    sys_info <- list()
  }
  sys_info <- as.list(sys_info)

  sysname <- sys_info[["sysname"]]
  if (!is.null(sysname) && length(sysname)) {
    sysname <- as.character(sysname)[1]
  }
  if (is.null(sysname) || tolower(sysname) != "darwin") {
    return(list(
      release = NA_character_,
      darwin_major = NA_integer_,
      marketing_version = NA_character_,
      marketing_codename = NA_character_,
      display_name = NULL,
      meets_v6_requirement = TRUE
    ))
  }

  release <- NA_character_
  raw_release <- sys_info[["release"]]
  if (!is.null(raw_release) && length(raw_release)) {
    release_candidate <- as.character(raw_release)[1]
    release_candidate <- trimws(release_candidate)
    if (nzchar(release_candidate)) {
      release <- release_candidate
    }
  }

  darwin_major <- NA_integer_
  meets_requirement <- NA
  if (!is.na(release)) {
    numeric_release <- tryCatch(
      suppressWarnings(as.numeric_version(release)),
      error = function(e) NULL
    )
    if (!is.null(numeric_release) && length(numeric_release)) {
      components <- unclass(numeric_release)[[1]]
      if (length(components)) {
        major_component <- components[1]
        if (!is.na(major_component)) {
          darwin_major <- major_component
          meets_requirement <- major_component >= 24
        }
      }
    }
  }

  darwin_map <- list(
    `14` = list(version = "10.10", codename = "Yosemite"),
    `15` = list(version = "10.11", codename = "El Capitan"),
    `16` = list(version = "10.12", codename = "Sierra"),
    `17` = list(version = "10.13", codename = "High Sierra"),
    `18` = list(version = "10.14", codename = "Mojave"),
    `19` = list(version = "10.15", codename = "Catalina"),
    `20` = list(version = "11", codename = "Big Sur"),
    `21` = list(version = "12", codename = "Monterey"),
    `22` = list(version = "13", codename = "Ventura"),
    `23` = list(version = "14", codename = "Sonoma"),
    `24` = list(version = "15", codename = "Sequoia")
  )

  marketing <- NULL
  if (!is.na(darwin_major)) {
    marketing <- darwin_map[[as.character(darwin_major)]]
  }

  marketing_label <- NULL
  if (!is.null(marketing)) {
    marketing_label <- paste0("macOS ", marketing$version)
    if (!is.null(marketing$codename) && nzchar(marketing$codename)) {
      marketing_label <- paste0(marketing_label, " ", marketing$codename)
    }
  }

  darwin_label <- NULL
  if (!is.na(release)) {
    darwin_label <- paste0("Darwin ", release)
  }

  display_name <- NULL
  if (!is.null(marketing_label) && !is.null(darwin_label)) {
    display_name <- paste0(marketing_label, " [", darwin_label, "]")
  } else if (!is.null(marketing_label)) {
    display_name <- marketing_label
  } else if (!is.null(darwin_label)) {
    display_name <- darwin_label
  }

  list(
    release = release,
    darwin_major = darwin_major,
    marketing_version = if (!is.null(marketing)) {
      marketing$version
    } else {
      NA_character_
    },
    marketing_codename = if (!is.null(marketing)) {
      marketing$codename
    } else {
      NA_character_
    },
    display_name = display_name,
    meets_v6_requirement = if (!is.na(darwin_major)) {
      darwin_major >= 24
    } else {
      meets_requirement
    }
  )
}

#' @noRd
github_api_request_with_retries <- function(url, error_message, verb = "GET") {
  # Base request with error handling and retry policy
  req <- httr2::request(url)
  req <- httr2::req_error(req, body = function(resp) {
    httr2::resp_body_json(resp)$message
  })
  # Retry on GitHub's rate-limiting status (403), as well as the
  # standard 429 (Too Many Requests) and 503 (Service Unavailable).
  req <- httr2::req_retry(
    req,
    max_tries = 4,
    is_transient = ~ httr2::resp_status(.x) %in% c(403, 429, 503)
  )

  # Check for a GitHub Personal Access Token to increase rate limits.
  # This is especially important for CI/CD environments.
  token <- Sys.getenv("GITHUB_PAT")
  if (nzchar(token)) {
    # Add authentication header if token is found
    req <- httr2::req_auth_bearer_token(req, token)
  }

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      stop(error_message, e$message, call. = FALSE)
    }
  )
  return(resp)
}

#' @noRd
get_all_releases <- function() {
  repo <- "Project-OSRM/osrm-backend"
  url <- paste0("https://api.github.com/repos/", repo, "/releases")

  resp <- github_api_request_with_retries(
    url,
    error_message = "GitHub API request failed: "
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

  resp <- github_api_request_with_retries(
    url,
    error_message = "GitHub API request failed: "
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
install_profiles_for_release <- function(
  release_info,
  dest_dir,
  quiet = FALSE
) {
  tarball_url <- release_info$tarball_url

  if (is.null(tarball_url) || !nzchar(tarball_url)) {
    if (!quiet) {
      warning(
        "Release metadata did not include a tarball URL. Skipping installation of profiles.",
        call. = FALSE
      )
    }
    return(invisible(NULL))
  }

  if (!quiet) {
    message("Downloading profiles from release tarball...")
  }
  tmp_tarball <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tmp_tarball), add = TRUE)

  tryCatch(
    {
      req <- httr2::req_retry(httr2::request(tarball_url), max_tries = 3)
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

  # Extract only the top-level 'profiles' subtree shipped with the release.
  top_level_dirs <- unique(sub("/.*$", "", tar_members))
  top_level_dirs <- top_level_dirs[nzchar(top_level_dirs)]
  top_level_dir <- top_level_dirs[[1]]

  prefix <- paste0(top_level_dir, "/")
  relative_members <- tar_members
  matches_prefix <- startsWith(relative_members, prefix)
  relative_members[matches_prefix] <- substr(
    relative_members[matches_prefix],
    nchar(prefix) + 1,
    nchar(relative_members[matches_prefix])
  )

  profile_mask <- startsWith(relative_members, "profiles/") |
    relative_members %in% c("profiles", "profiles/")

  profile_members <- tar_members[profile_mask]

  if (length(profile_members) == 0) {
    stop(
      "The release tarball did not contain a 'profiles' directory.",
      call. = FALSE
    )
  }

  profile_dirs <- unique(
    sub("^(.*?/profiles)(?:/.*)?$", "\\1", profile_members, perl = TRUE)
  )
  profile_dirs <- profile_dirs[
    nzchar(profile_dirs) &
      !grepl("[[:cntrl:]]", profile_dirs, perl = TRUE)
  ]
  extract_members <- profile_members
  extract_members <- extract_members[
    nzchar(extract_members) &
      !grepl("[[:cntrl:]]", extract_members, perl = TRUE)
  ]

  if (!quiet) {
    message("Extracting profiles...")
  }
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

  copied <- file.copy(
    from = profile_source,
    to = dest_dir,
    recursive = TRUE,
    overwrite = TRUE
  )

  if (!all(copied)) {
    stop(
      "Failed to copy the profiles directory to the destination.",
      call. = FALSE
    )
  }

  if (!quiet) {
    message("Installed profiles to ", dest_profiles_dir)
  }
  invisible(dest_profiles_dir)
}

#' @noRd
maybe_install_windows_v6_runtime <- function(
  version,
  platform,
  dest_dir,
  quiet = FALSE
) {
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

  install_windows_v6_runtime(dest_dir, quiet = quiet)
}

#' @noRd
maybe_install_linux_v6_runtime <- function(
  version,
  platform,
  dest_dir,
  quiet = FALSE
) {
  if (!identical(platform$os, "linux")) {
    return(invisible(NULL))
  }

  if (!version_at_least(version, "v6.0.0")) {
    return(invisible(NULL))
  }

  ensure_linux_tbb_runtime(dest_dir, platform, quiet = quiet)
}

#' @noRd
install_windows_v6_runtime <- function(dest_dir, quiet = FALSE) {
  if (!quiet) {
    message("Fetching additional Windows runtime dependencies (TBB, BZip2)...")
  }

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
  if (!quiet) {
    message("  - Installed tbb12.dll")
  }

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
  if (!quiet) {
    message("  - Installed bz2.dll")
  }

  invisible(dest_dir)
}

#' @noRd
maybe_install_macos_v6_runtime <- function(
  version,
  platform,
  dest_dir,
  quiet = FALSE
) {
  if (!identical(platform$os, "darwin")) {
    return(invisible(NULL))
  }

  if (!version_at_least(version, "v6.0.0")) {
    return(invisible(NULL))
  }

  ensure_macos_tbb_runtime(dest_dir, platform, quiet = quiet)
  patch_macos_bzip2_rpath(dest_dir, quiet = quiet)
}

#' @noRd
ensure_linux_tbb_runtime <- function(
  dest_dir,
  platform,
  reference_version = "v5.27.1",
  quiet = FALSE
) {
  required_libs <- c(
    "libtbbmalloc.so.2.3",
    "libtbbmalloc.so.2",
    "libtbbmalloc.so",
    "libtbbmalloc_proxy.so.2.3",
    "libtbbmalloc_proxy.so.2",
    "libtbbmalloc_proxy.so",
    "libtbb.so.12.3",
    "libtbb.so.12",
    "libtbb.so"
  )

  missing_libs <- required_libs[
    !file.exists(file.path(dest_dir, required_libs))
  ]
  if (length(missing_libs)) {
    if (!quiet) {
      message(
        "Fetching Linux TBB runtime components from OSRM release ",
        reference_version,
        "..."
      )
    }
  } else {
    if (!quiet) {
      message(
        "Refreshing Linux TBB runtime components from OSRM release ",
        reference_version,
        "..."
      )
    }
  }

  release_info <- get_release_by_tag(reference_version)
  asset_url <- find_asset_url(release_info, platform)

  tmp_tar <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tmp_tar), add = TRUE)

  tryCatch(
    {
      req <- httr2::req_retry(httr2::request(asset_url), max_tries = 3)
      httr2::req_perform(req, path = tmp_tar)
    },
    error = function(e) {
      stop(
        "Failed to download Linux TBB runtime from OSRM release ",
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
        " while retrieving Linux TBB runtime: ",
        e$message,
        call. = FALSE
      )
    }
  )

  extracted_files <- list.files(
    tmp_extract_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  locate_lib <- function(lib) {
    matches <- extracted_files[
      tolower(basename(extracted_files)) == tolower(lib)
    ]
    if (!length(matches)) {
      return(NA_character_)
    }
    matches[[1]]
  }

  lib_sources <- vapply(required_libs, locate_lib, FUN.VALUE = character(1))

  if (anyNA(lib_sources)) {
    missing <- required_libs[is.na(lib_sources)]
    stop(
      "Could not locate required Linux TBB libraries in OSRM release ",
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
      "Failed to copy one or more Linux TBB runtime libraries into ",
      dest_dir,
      call. = FALSE
    )
  }

  Sys.chmod(dest_paths, mode = "0644", use_umask = FALSE)

  invisible(dest_dir)
}

#' @noRd
ensure_macos_tbb_runtime <- function(
  dest_dir,
  platform,
  reference_version = "v5.27.1",
  quiet = FALSE
) {
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

  missing_libs <- required_libs[
    !file.exists(file.path(dest_dir, required_libs))
  ]
  if (length(missing_libs)) {
    if (!quiet) {
      message(
        "Fetching macOS TBB runtime components from OSRM release ",
        reference_version,
        "..."
      )
    }
  } else {
    if (!quiet) {
      message(
        "Refreshing macOS TBB runtime components from OSRM release ",
        reference_version,
        "..."
      )
    }
  }

  release_info <- get_release_by_tag(reference_version)
  asset_url <- find_asset_url(release_info, platform)

  tmp_tar <- tempfile(fileext = ".tar.gz")
  on.exit(unlink(tmp_tar), add = TRUE)

  tryCatch(
    {
      req <- httr2::req_retry(httr2::request(asset_url), max_tries = 3)
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

  extracted_files <- list.files(
    tmp_extract_dir,
    recursive = TRUE,
    full.names = TRUE
  )

  locate_lib <- function(lib) {
    matches <- extracted_files[
      tolower(basename(extracted_files)) == tolower(lib)
    ]
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

  if (!quiet) {
    message("Installed macOS TBB runtime libraries.")
  }
  invisible(dest_dir)
}

#' @noRd
patch_macos_bzip2_rpath <- function(dest_dir, quiet = FALSE) {
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
    if (!quiet) {
      warning(
        "No OSRM executables found to patch in ",
        dest_dir,
        call. = FALSE
      )
    }
    return(invisible(dest_dir))
  }

  if (!quiet) {
    message("Updating macOS BZip2 linkage...")
  }
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

  cmp <- tryCatch(
    suppressWarnings(utils::compareVersion(v_norm, min_norm)),
    error = function(e) NA_integer_
  )

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

  req <- httr2::req_retry(httr2::request(url), max_tries = 3)
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
    set_path_session(dir, quiet)
  } else if (action == "project") {
    set_path_project(dir, quiet)
  }
  # If "none", do nothing
}

#' @noRd
set_path_session <- function(dir, quiet = FALSE) {
  path_sep <- .Platform$path.sep
  current_path <- Sys.getenv("PATH")

  path_elements <- strsplit(current_path, path_sep)[[1]]

  # On Windows, use forward slashes for consistent comparison
  # This handles issues with mixed separators and 8.3 short names
  if (.Platform$OS.type == "windows") {
    normalized_dir <- normalizePath(dir, mustWork = FALSE, winslash = "/")
    normalized_paths <- normalizePath(path_elements, mustWork = FALSE, winslash = "/")
    cache_root <- normalizePath(osrm_default_install_root(), mustWork = FALSE, winslash = "/")
    # Use "/" for cache_root_prefix to match the normalized paths
    cache_root_prefix <- paste0(cache_root, "/")
  } else {
    normalized_dir <- normalizePath(dir, mustWork = FALSE)
    normalized_paths <- normalizePath(path_elements, mustWork = FALSE)
    cache_root <- normalizePath(osrm_default_install_root(), mustWork = FALSE)
    cache_root_prefix <- paste0(cache_root, .Platform$file.sep)
  }

  # Helper function for path comparison (case-insensitive on Windows)
  paths_equal <- function(path1, path2) {
    if (.Platform$OS.type == "windows") {
      tolower(path1) == tolower(path2)
    } else {
      path1 == path2
    }
  }

  # Remove any paths that point to other versions in our cache directory
  # but keep system installations (homebrew, manual installs, etc.)
  paths_to_keep <- vapply(seq_along(path_elements), function(i) {
    norm_path <- normalized_paths[i]
    # Keep this path if:
    # 1. It's the directory we're installing to, OR
    # 2. It's not under our cache root (i.e., it's a system/homebrew install)
    if (paths_equal(norm_path, normalized_dir)) {
      return(TRUE)
    }
    # On Windows, use case-insensitive comparison for path prefix check
    if (.Platform$OS.type == "windows") {
      !startsWith(tolower(norm_path), tolower(cache_root_prefix))
    } else {
      !startsWith(norm_path, cache_root_prefix)
    }
  }, logical(1))

  filtered_paths <- path_elements[paths_to_keep]

  # Always prepend the new installation directory to the front
  # (even if it was already in the PATH, we want it first)
  first_path_matches <- if (length(normalized_paths) > 0) {
    paths_equal(normalized_paths[1], normalized_dir)
  } else {
    FALSE
  }

  if (!first_path_matches) {
    # Remove the dir if it's already present (to avoid duplicates)
    kept_normalized <- normalized_paths[paths_to_keep]
    paths_not_matching <- !vapply(kept_normalized, paths_equal, logical(1), path2 = normalized_dir)
    filtered_paths <- filtered_paths[paths_not_matching]
    new_path <- paste(c(normalized_dir, filtered_paths), collapse = path_sep)
    Sys.setenv(PATH = new_path)
    if (!quiet) {
      message(sprintf("Added '%s' to PATH for this session.", normalized_dir))
    }
  } else if (!quiet) {
    message(sprintf("'%s' is already first in PATH.", normalized_dir))
  }
}

#' @noRd
set_path_project <- function(dir, quiet = FALSE) {
  r_profile_path <- file.path(getwd(), ".Rprofile")

  if (!quiet) {
    message("You chose to set the OSRM path for this project.")
    message("This will update the following file: ", r_profile_path)
  }

  comment_tag <- "#added-by-r-pkg-osrm.backend"
  line_to_add <- sprintf(
    'Sys.setenv(PATH = paste("%s", Sys.getenv("PATH"), sep = "%s")) %s',
    normalizePath(dir),
    .Platform$path.sep,
    comment_tag
  )

  if (!file.exists(r_profile_path)) {
    if (!quiet) {
      message("Creating new .Rprofile file.")
    }
    writeLines(line_to_add, r_profile_path)
  } else {
    lines <- readLines(r_profile_path, warn = FALSE)

    # Remove any existing lines with our tag (old cached versions)
    has_tag <- grepl(comment_tag, lines, fixed = TRUE)
    if (any(has_tag)) {
      # Check if the existing line is for the same directory
      existing_dirs <- vapply(lines[has_tag], function(line) {
        # Extract directory path from the line
        match <- regmatches(line, regexec('paste\\("([^"]+)"', line))
        if (length(match[[1]]) >= 2) {
          normalizePath(match[[1]][2], mustWork = FALSE)
        } else {
          ""
        }
      }, character(1))

      normalized_dir <- normalizePath(dir, mustWork = FALSE)
      if (any(existing_dirs == normalized_dir)) {
        if (!quiet) {
          message(
            ".Rprofile already contains this OSRM path configuration. No changes made."
          )
        }
        return(invisible())
      }

      if (!quiet) {
        message("Replacing old OSRM path configuration with new one.")
      }
      lines <- lines[!has_tag]
    } else {
      if (!quiet) {
        message("Appending configuration to existing .Rprofile.")
      }
    }

    # Add a newline for separation if the file doesn't end with one
    if (length(lines) > 0 && nzchar(lines[length(lines)])) {
      lines <- c(lines, "")
    }
    lines <- c(lines, line_to_add)
    writeLines(lines, r_profile_path)
  }

  if (!quiet) {
    message(
      "\nSuccessfully modified .Rprofile. Please restart R for the changes to take effect."
    )
  }
}
