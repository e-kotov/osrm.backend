#' Install OSRM Backend Binaries
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Downloads and installs pre-compiled binaries for the OSRM backend from the
#' official GitHub releases. The function automatically detects the user's
#' operating system and architecture to download the appropriate files. Only the
#' latest v5 release (`v5.27.1`), `v6.0.0`, `v26.4.0` and `v26.4.1` were manually
#' tested and are known to work well; other releases available on GitHub can be
#' installed but are not guranteed to function correctly.
#'
#' @details
#' The function performs the following steps:
#' 1.  Queries the GitHub API to find the specified release of
#'     `e-kotov/osrm-binaries`.
#' 2.  Identifies the correct binary (`.tar.gz` archive) for the user's OS
#'     (Linux, macOS, or Windows) and architecture (x64, arm64).
#' 3.  Downloads the archive to a temporary location.
#' 4.  Extracts the archive and locates the OSRM executables (e.g.,
#'     `osrm-routed`, `osrm-extract`).
#' 5.  Copies these executables to a local directory (defaults to
#'     `file.path(tools::R_user_dir("osrm.backend", which = "cache"), <version>)`).
#' 6.  Downloads the matching Lua profiles from the release tarball and installs
#'     them alongside the binaries.
#' 7.  Optionally modifies the `PATH` environment variable for the current
#'     session or project.
#'
#' \subsection{Binary Providers}{
#' The `osrm_binaries_provider` argument allows choosing between two release sources:
#' \itemize{
#'   \item \strong{`"default"` (e-kotov/osrm-binaries)}: This provider is highly recommended. It offers standalone, 
#'   immutable C++ binaries that are statically linked and patched during the build process to guarantee 
#'   maximum compatibility across older and newer operating systems. It explicitly bundles required 
#'   libraries (like Intel TBB), skips NodeJS bloat, and provides exclusive support for `linux-arm64` 
#'   (e.g., AWS Graviton, Raspberry Pi) and modern Apple Silicon Macs.
#'   \item \strong{`"official"` (Project-OSRM/osrm-backend)}: The upstream releases provided by the core 
#'   OSRM team. These binaries are wrapped inside `node_osrm` NodeJS tarballs and lack `linux-arm64` 
#'   builds. When installing v6.x or newer via the `"official"` provider on Windows or macOS, upstream 
#'   releases omit the Intel TBB runtime. In these cases, `osrm_install()` will automatically attempt 
#'   legacy runtime hacks (e.g. downloading oneTBB, patching `libbz2` linkage via `install_name_tool`) 
#'   to keep the binaries functional out-of-the-box.
#' }
#' }
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
#'   other than `v5.27.1`, `v6.0.0`, `v26.4.0`, `v26.4.1`, and `v26.5.0` will
#'   trigger a warning but are still attempted if binaries are available.
#' @param osrm_binaries_provider A string specifying the provider to download binaries from.
#'   Defaults to `"default"`, which pulls from `"e-kotov/osrm-binaries"` providing custom immutable builds 
#'   (statically linked) that fix glibc compatibility issues and bundle necessary libraries. 
#'   Set to `"official"` to download the upstream binaries from `"Project-OSRM/osrm-backend"`
#'   (which will automatically trigger legacy runtime hacks for macOS and Windows).
#'   Advanced users can override the provider completely by setting the R option
#'   `osrm.backend.custom_repository` to a custom GitHub repository (e.g. `"my-user/my-repo"`).
#' @param dest_dir A string specifying the directory where OSRM binaries should be
#'   installed. If `NULL` (the default), a user-friendly, persistent location is
#'   chosen via `tools::R_user_dir("osrm.backend", which = "cache")`, and the
#'   binaries are installed into a subdirectory named after the OSRM version
#'   (e.g. `.../cache/v26.4.1`).
#' @param force A logical value. If `TRUE`, reinstall OSRM even if it's already
#'   found in `dest_dir`. If `FALSE` (default), the function will stop if an
#'   existing installation is detected.
#' @param path_action A string specifying how to handle the system `PATH`. One of:
#'   \itemize{
#'     \item `"session"` (default): Adds the OSRM bin directory to the `PATH`
#'       for the current R session only.
#'     \item `"project"`: Modifies the `.Rprofile` in the current project to set
#'       the `PATH` for all future sessions in that project.
#'     \item `"none"`: Does not modify the `PATH`.
#'   }
#' @param quiet A logical value. If `TRUE`, suppresses installer messages and
#'   warnings. Defaults to `FALSE`.
#' @param check_tested A logical value. If `TRUE` (default), the function issues
#'   a warning if the requested OSRM version has not been explicitly validated
#'   by the package maintainers. If `FALSE`, it issues a status message instead.
#' @param download_url **Advanced usage only.** A direct URL to a `.tar.gz` archive containing OSRM binaries.
#'   If provided, `version` and `osrm_binaries_provider` are ignored.
#' @param file_path **Advanced usage only.** A local file path to a `.tar.gz` archive containing OSRM binaries.
#'   If provided, skips downloading entirely.
#' @return The path to the installation directory.
#' @export
#' @examples
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   old <- setwd(tempdir())
#'   on.exit(setwd(old), add = TRUE)
#'
#'   # Install the default stable version and set PATH for this session
#'   install_dir <- osrm_install(path_action = "session", quiet = TRUE)
#'
#'   # Install for a project non-interactively (e.g., in a script)
#'   osrm_install(path_action = "project", quiet = TRUE, force = TRUE)
#'
#'   # Clean up the project's .Rprofile and uninstall binaries
#'   osrm_clear_path(quiet = TRUE)
#'   osrm_uninstall(
#'     dest_dir = install_dir,
#'     clear_path = TRUE,
#'     force = TRUE,
#'     quiet = TRUE
#'   )
#' }
#' }
osrm_install <- function(
  version = "latest",
  osrm_binaries_provider = c("default", "official"),
  dest_dir = NULL,
  force = FALSE,
  path_action = c("session", "project", "none"),
  quiet = FALSE,
  check_tested = TRUE,
  download_url = NULL,
  file_path = NULL
) {
  osrm_binaries_provider <- match.arg(osrm_binaries_provider)
  repo <- getOption("osrm.backend.custom_repository")
  if (is.null(repo)) {
    repo <- if (osrm_binaries_provider == "default") "e-kotov/osrm-binaries" else "Project-OSRM/osrm-backend"
  }
  old_opts <- options(osrm.backend.repository = repo)
  on.exit(options(old_opts), add = TRUE)

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
  tested_versions <- c("v5.27.1", "v6.0.0", "v26.4.0", "v26.4.1", "v26.5.0")
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
      paste(
        "Version '%s' has not been validated by osrm.backend;",
        "only %s are tested."
      ),
      version,
      paste(tested_versions, collapse = ", ")
    )
    if (identical(requested_version, "latest")) {
      warning_message <- sprintf(
        paste(
          "Latest available release '%s' has not been validated by",
          "osrm.backend; only %s are tested."
        ),
        version,
        paste(tested_versions, collapse = ", ")
      )
    }
    if (isTRUE(check_tested)) {
      emit_warning(warning_message, call. = FALSE)
    } else {
      emit_message(warning_message)
    }
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
          paste(
            "Unable to verify requested version '%s' against",
            "available releases: %s"
          ),
          version,
          available_versions$message
        ),
        call. = FALSE
      )
    } else if (!version %in% available_versions) {
      stop(
        sprintf(
          paste(
            "Version '%s' is not available for this platform.",
            "Run osrm_check_available_versions(prereleases = TRUE) to list",
            "supported tags."
          ),
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

  manual_install <- !is.null(download_url) || !is.null(file_path)

  if (manual_install) {
    if (!is.null(file_path)) {
      if (!file.exists(file_path)) stop("Provided file_path does not exist: ", file_path)
      tmp_file <- file_path
      emit_message("Using local file: ", tmp_file)
    } else {
      tmp_file <- tempfile(fileext = ".tar.gz")
      on.exit(unlink(tmp_file), add = TRUE)
      emit_message("Downloading from ", download_url)
      tryCatch({
        req <- httr2::req_retry(httr2::request(download_url), max_tries = 3)
        httr2::req_perform(req, path = tmp_file)
      }, error = function(e) {
        stop("Failed to download file: ", e$message, call. = FALSE)
      })
    }
  } else {
    # --- 4. Fetch release metadata or construct directly ---
    if (identical(getOption("osrm.backend.repository"), "e-kotov/osrm-binaries")) {
      emit_message(sprintf("Detected platform: %s-%s", platform$os, platform$arch))
      asset_url <- sprintf("https://github.com/e-kotov/osrm-binaries/releases/download/%s/osrm-%s-%s-%s-Release.tar.gz", version, version, platform$os, platform$arch)
      emit_message("Found matching binary: ", basename(asset_url))
    } else {
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
    }

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
  }

  # Check if expert user has explicitly opted out
  skip_validation <- isTRUE(getOption("osrm.backend.skip_validation", FALSE))
  is_official_repo <- identical(getOption("osrm.backend.repository"), "e-kotov/osrm-binaries")

  if (skip_validation) {
    emit_message("Bypassing SHA-256 checksum validation as requested by user option.")
  } else if (!manual_install && is_official_repo) {
    expected_hash <- get_expected_hash(version, platform)
    if (!is.na(expected_hash)) {
      actual_hash <- digest::digest(tmp_file, algo = "sha256", file = TRUE)
      if (actual_hash != expected_hash) {
        stop(
          "SECURITY ERROR: The downloaded binary checksum does not match the known-good hash!\n",
          "Expected: ", expected_hash, "\n",
          "Actual:   ", actual_hash, "\n",
          "This could indicate a tampered release or a corrupted download. Aborting installation.\n",
          "To override this check, set options(osrm.backend.skip_validation = TRUE).",
          call. = FALSE
        )
      }
      emit_message("Checksum verified (", actual_hash, ")")
    } else {
      warning(
        "No known-good SHA-256 hash found for version '", version,
        "' on platform '", platform$os, "-", platform$arch, "'. Proceeding without validation.",
        call. = FALSE
      )
    }
  }

  tmp_extract_dir <- tempfile()
  dir.create(tmp_extract_dir)
  on.exit(unlink(tmp_extract_dir, recursive = TRUE), add = TRUE)
  emit_message("Extracting binaries...")
  status <- tryCatch(
    {
      if (quiet) {
        suppressWarnings(utils::untar(tmp_file, exdir = tmp_extract_dir))
      } else {
        utils::untar(tmp_file, exdir = tmp_extract_dir)
      }
    },
    error = function(e) {
      stop("Failed to extract archive: ", e$message, call. = FALSE)
    }
  )

  if (!is.null(status) && status != 0) {
    stop("Failed to extract archive: tar returned exit code ", status, call. = FALSE)
  }

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
      paste(
        "Could not find OSRM binaries in the downloaded archive.",
        "The archive structure may have changed."
      ),
      call. = FALSE
    )
  }

  bin_source_dir <- dirname(found_bins[1])
  files_to_copy <- list.files(bin_source_dir, full.names = TRUE)

  emit_message("Installing binaries to ", dest_dir)
  file.copy(from = files_to_copy, to = dest_dir, overwrite = TRUE, recursive = TRUE)

  # Check if we need to install supplementary libraries/dlls
  maybe_install_windows_v6_runtime(
    version,
    platform,
    dest_dir,
    quiet = quiet
  )
  maybe_install_linux_v6_runtime(
    version,
    platform,
    dest_dir,
    quiet = quiet
  )
  maybe_install_macos_v6_runtime(
    version,
    platform,
    dest_dir,
    quiet = quiet
  )

  # --- 9. Download and install Lua profiles ---
  # If we are using our custom binaries, the profiles directory is already bundled inside the tarball
  # and was copied recursively above. We only need to download them for official binaries.
  if (!identical(getOption("osrm.backend.repository"), "e-kotov/osrm-binaries") && !dir.exists(file.path(dest_dir, "profiles"))) {
    install_profiles_for_release(release_info, dest_dir, quiet = quiet)
  }

  # --- 10. Set permissions and update PATH ---
  installed_bins <- file.path(dest_dir, basename(found_bins))
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
      paste(
        "Installation completed, but 'osrm-routed' was not found on the",
        "PATH immediately. You may need to restart your R session."
      ),
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
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Queries the GitHub API to find the most recent stable (non-pre-release)
#' version tag for the OSRM backend that has binaries available for the current platform.
#'
#' @return A string containing the latest version tag (e.g., `"v26.4.1"`).
#' @export
#' @examples
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   # Get the latest stable version number of OSRM backend
#'   osrm_check_latest_version()
#' }
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
    paste(
      "No pre-v6 releases with compatible binaries were found for this",
      "platform. Upgrade macOS to install OSRM v6.x or later."
    ),
    call. = FALSE
  )
}

#' Check for Available OSRM Versions
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Queries the GitHub API to get a list of all available version tags for the
#' OSRM backend that have binaries for the current platform.
#'
#' @param prereleases A logical value. If `TRUE`, include pre-release versions
#'   in the returned list. Defaults to `FALSE`.
#' @return A character vector of available version tags.
#' @export
#' 
#' @examples
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   # Get all stable versions with binaries for this platform
#'   osrm_check_available_versions()
#' }
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
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   # Clean up a temporary project's .Rprofile
#'   old <- setwd(tempdir())
#'   on.exit(setwd(old), add = TRUE)
#'   writeLines(
#'     c(
#'       "#added-by-r-pkg-osrm.backend",
#'       'Sys.setenv(PATH = paste("dummy", Sys.getenv("PATH"), sep = .Platform$path.sep))'
#'     ),
#'     ".Rprofile"
#'   )
#'   osrm_clear_path(quiet = TRUE)
#'   unlink(".Rprofile")
#' }
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
get_all_releases <- function(repo = getOption("osrm.backend.repository", "e-kotov/osrm-binaries")) {
  url <- paste0("https://api.github.com/repos/", repo, "/releases")

  resp <- github_api_request_with_retries(
    url,
    error_message = "GitHub API request failed: "
  )

  httr2::resp_body_json(resp)
}

#' @noRd
get_release_by_tag <- function(version, repo = getOption("osrm.backend.repository", "e-kotov/osrm-binaries")) {
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
    if (quiet || suppress_tar_warnings) {
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
      {
        res <- run_untar(args)
        if (is.character(res)) {
          res
        } else {
          last_list_error <<- list(
            message = paste0("tar returned exit code ", res)
          )
          NULL
        }
      },
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
    msg <- if (is.list(last_list_error)) {
      last_list_error$message
    } else if (inherits(last_list_error, "error")) {
      conditionMessage(last_list_error)
    } else {
      "Unknown error"
    }

    stop(
      "Failed to inspect source tarball for profiles: ",
      msg,
      call. = FALSE
    )
  }

  # We verify that a 'profiles' directory exists in the members list
  # before attempting full extraction.
  profile_exists <- any(grepl("(^|/)profiles(/|$)", tar_members))

  if (!profile_exists) {
    stop(
      "The release tarball did not contain a 'profiles' directory.",
      call. = FALSE
    )
  }

  if (!quiet) {
    message("Extracting profiles...")
  }
  untar_args <- list(
    tarfile = tmp_tarball,
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
        status <- run_untar(args)
        # untar returns 0 for success, non-zero for failure
        if (is.null(status) || status == 0) {
          extracted <- TRUE
        } else {
          last_extract_error <<- list(
            message = paste0("tar returned exit code ", status)
          )
        }
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
    msg <- if (is.list(last_extract_error)) {
      last_extract_error$message
    } else if (inherits(last_extract_error, "error")) {
      conditionMessage(last_extract_error)
    } else {
      "Unknown error"
    }

    stop(
      "Failed to extract profiles archive: ",
      msg,
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

  # Pick the profiles directory closest to the root (fewest segments)
  # This correctly handles cases where nested 'profiles' directories exist (e.g. in tests)
  path_depths <- nchar(gsub("[^/]", "", profile_dirs_found))
  profile_source <- profile_dirs_found[[which.min(path_depths)]]

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

  if (identical(getOption("osrm.backend.repository"), "e-kotov/osrm-binaries")) {
    return(invisible(NULL))
  }

  if (is.null(version) || !nzchar(version)) {
    return(invisible(NULL))
  }

  if (!version_at_least(version, "v6.0.0")) {
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

  if (identical(getOption("osrm.backend.repository"), "e-kotov/osrm-binaries")) {
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
    sha256 = "e1b2373f25558bf47d16b4c89cf0a31e6689aaf7221400d209e8527afc7c9eee",
    quiet = quiet
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
    sha256 = "50340fece047960f49cf869034c778ff9f6af27dde2f1ea9773cd89ddb326254",
    quiet = quiet
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

  if (identical(getOption("osrm.backend.repository"), "e-kotov/osrm-binaries")) {
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

  release_info <- get_release_by_tag(reference_version, repo = "Project-OSRM/osrm-backend")
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

  status <- tryCatch(
    {
      if (quiet) {
        suppressWarnings(utils::untar(tmp_tar, exdir = tmp_extract_dir))
      } else {
        utils::untar(tmp_tar, exdir = tmp_extract_dir)
      }
    },
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

  if (!is.null(status) && status != 0) {
    stop(
      "Failed to extract OSRM release ",
      reference_version,
      " while retrieving Linux TBB runtime: tar returned exit code ",
      status,
      call. = FALSE
    )
  }

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

  release_info <- get_release_by_tag(reference_version, repo = "Project-OSRM/osrm-backend")
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

  status <- tryCatch(
    {
      if (quiet) {
        suppressWarnings(utils::untar(tmp_tar, exdir = tmp_extract_dir))
      } else {
        utils::untar(tmp_tar, exdir = tmp_extract_dir)
      }
    },
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

  if (!is.null(status) && status != 0) {
    stop(
      "Failed to extract OSRM release ",
      reference_version,
      " while retrieving macOS TBB runtime: tar returned exit code ",
      status,
      call. = FALSE
    )
  }

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
download_zip_asset <- function(url, member_path, dest_path, sha256 = NULL, quiet = FALSE) {
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

  contents <- if (quiet) {
    suppressWarnings(utils::unzip(tmp_zip, list = TRUE))
  } else {
    utils::unzip(tmp_zip, list = TRUE)
  }

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
    {
      if (quiet) {
        suppressWarnings(utils::unzip(tmp_zip, files = selected_member, exdir = tmp_extract))
      } else {
        utils::unzip(tmp_zip, files = selected_member, exdir = tmp_extract)
      }
    },
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
    normalized_paths <- normalizePath(
      path_elements,
      mustWork = FALSE,
      winslash = "/"
    )
    cache_root <- normalizePath(
      osrm_default_install_root(),
      mustWork = FALSE,
      winslash = "/"
    )
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
  paths_to_keep <- vapply(
    seq_along(path_elements),
    function(i) {
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
    },
    logical(1)
  )

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
    paths_not_matching <- !vapply(
      kept_normalized,
      paths_equal,
      logical(1),
      path2 = normalized_dir
    )
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

  # Use forward slashes for cross-platform compatibility in the .Rprofile
  path_for_r <- normalizePath(dir, mustWork = FALSE, winslash = "/")

  comment_tag <- "#added-by-r-pkg-osrm.backend"
  line_to_add <- sprintf(
    'Sys.setenv(PATH = paste("%s", Sys.getenv("PATH"), sep = "%s")) %s',
    path_for_r,
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
    has_tag <- grepl(comment_tag, lines, fixed = TRUE)

    if (any(has_tag)) {
      existing_dirs <- vapply(
        lines[has_tag],
        function(line) {
          match <- regmatches(line, regexec('paste\\("([^"]+)"', line))
          if (length(match[[1]]) >= 2) {
            # Normalize with forward slashes for consistent comparison
            normalizePath(match[[1]][2], mustWork = FALSE, winslash = "/")
          } else {
            ""
          }
        },
        character(1)
      )

      # Case-insensitive comparison on Windows
      is_same_path <- if (.Platform$OS.type == "windows") {
        tolower(path_for_r) %in% tolower(existing_dirs)
      } else {
        path_for_r %in% existing_dirs
      }

      if (any(is_same_path)) {
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

#' @noRd
get_expected_hash <- function(version, platform) {
  key <- paste0(version, "_", platform$os, "-", platform$arch)
  
  # A hardcoded list of known-good SHA-256 hashes for immutable releases.
  KNOWN_HASHES <- list(
    "v26.7.2_darwin-arm64" = "e5bdb15d1bec0d8cf7f5bf8916dd943e211492c648896603f35b74f78fd63fa9",
    "v26.7.2_darwin-x64" = "a0f92f7112642e6b8746d00f3d6e786462972df5b934b26ae562040af895d47b",
    "v26.7.2_linux-arm64" = "83d89bf2d30dbdda459c3a57e877c9456705911d1b3afcf442d6484b50ef2345",
    "v26.7.2_linux-x64" = "17593eb763f3a82019765bc84c7b6f98771fd19d3d6a8300d900137bc0fefe85",
    "v26.7.2_win32-x64" = "7293435c0587b68054439f7c60f52cfcc1d385299ac074f8117985bfb4cb9167",
    "v26.7.1_darwin-arm64" = "aa8ba4a5968f635f968579d616ed84a974828b9233e198c5e0279049227d1b87",
    "v26.7.1_darwin-x64" = "24f98904c57dd3da6990a37d339696aa2ced62198ae29bde0fec485c649e603d",
    "v26.7.1_linux-arm64" = "9bbf7fa7aa2f7b845dd60a82610f4c441facbac72c7dbe5e05fe32d6fd43edb9",
    "v26.7.1_linux-x64" = "eb8649e84ccfe37cd5603237fda4415b8ec1b2fb759e1e6b4e8aed7af95f81a5",
    "v26.7.1_win32-x64" = "80e4e0860893ccc4288642524a70f4455883cc978f5bb37f9de7daad255e7fdd",
    "v26.7.0_darwin-arm64" = "054673b52388e2f056458934ac2f63234ce02e20689c4916621bc1062601ce01",
    "v26.7.0_darwin-x64" = "6087e4409735dfefaaa6c5e1660230cab1653b4d206cf471c06a5a95ac20cc80",
    "v26.7.0_linux-arm64" = "9c0e548c8e42020612858c80c4378ec5344ba417983fba2cfb2baa4908ba258d",
    "v26.7.0_linux-x64" = "5aed8af06947fe73cd2f17f9f0c5adcfce8151d54a8ceb9c701b4d398c3add0e",
    "v26.7.0_win32-x64" = "8d8dca94bccc6b7e5ca6e45be43736e636f3326a6f7d012c8dd75150179a2096",
    "v26.6.5_darwin-arm64" = "3e204e46bc6913276b9fc6669ab0757dd4b9e94854f605ad19873ef5a3865b41",
    "v26.6.5_darwin-x64" = "344bf12dbc18e20673e7014b67a3e6e4b61b37cc1a4da03496e8c3d02d355339",
    "v26.6.5_linux-arm64" = "e6d96d4d0253c2c332750e81c8700ff16dcf610540e9ae6c7949b88d87fcaf66",
    "v26.6.5_linux-x64" = "7ec062b4e579a4c3a5dbddbbe5080af185b3a288ca53bb37175595dcc1b8ebcb",
    "v26.6.5_win32-x64" = "01d80998bc41c66aad59df9198c9efd71bf2ed672398f92562d270e9d41f096c",
    "v26.6.4_darwin-arm64" = "9dc7d6cdb9d053bcb6215ce0670b97204e9ba5f4211a99d02411397c6a28c95e",
    "v26.6.4_darwin-x64" = "0800941aa632738c99d05ffebb08a0fd667109bc3ffb6f7e12a99957347e1660",
    "v26.6.4_linux-arm64" = "375ce78b7aa0831b1baeb0ed9b245226baa6748564dff4e67673fea125dce317",
    "v26.6.4_linux-x64" = "bbb8cc5b2ce29ec2688045ee0586a0cde99dacbe8eb8beaca7958103ca7a19c2",
    "v26.6.4_win32-x64" = "ca01d2039d56aa04511565c7fd205296fd9b6fc12821ee54b4df3fb153a1810a",
    "v26.6.3_darwin-arm64" = "22d66e4ca58f914cf1dc082422974c0fb22192b18035f4acfda9ddddf6ae2e48",
    "v26.6.3_darwin-x64" = "58cc538ea18ef84712fceb457298d7aa6dee602742beb8adbdbca4d5367cd592",
    "v26.6.3_linux-arm64" = "a8ed0f307bdab1e12aba4d653954aa2a3d8942835c54a781a7ae48ae9af1d8e3",
    "v26.6.3_linux-x64" = "80e0f0ee08c0a51c54b5fcd9ee0f5f5e258d4d0ada4165622ccfe0e36369410a",
    "v26.6.3_win32-x64" = "572d73bb48aff33a162fcb940f481c30f39b8a8e1050312a2769476367eafff1",
    "v26.6.2_darwin-arm64" = "f352bdb27db28d42c7d15d9d2363478dc2e83344357e4c96b06726a20136f885",
    "v26.6.2_darwin-x64" = "8ebc41a15cb124d817e781b1c29b9b8fe8481c263fbf042e3746b16a627637be",
    "v26.6.2_linux-arm64" = "36b7c033c33f05763c71f3137a881163a095d13fdca1bcb9e5e743c2c056cad0",
    "v26.6.2_linux-x64" = "7767b91ff81bb1034be54e93cb78e09b1dfebea12549a248610cf17ebf4c0c8c",
    "v26.6.2_win32-x64" = "d37344e6b15c4302e392d63206a5f271c0792097a9b5216c4da843971c2780e6",
    "v26.6.1_darwin-arm64" = "5e2a64e921c249ea4c52a70500441b796d6a6bb39986435d50c1f6c5b8083339",
    "v26.6.1_darwin-x64" = "ece966dc65d4cb4a1102a082c36afb8075de434dbd3034d2f9e05ac17ef9b7c1",
    "v26.6.1_linux-arm64" = "fe39fe5d7a72bffc1b5e188315875f70245ea98c1c1b835225a549508e8db299",
    "v26.6.1_linux-x64" = "88916ef8e9ebd6529299b03b11c8bb124301350b8ef6b53a86d9621c2b5155fb",
    "v26.6.1_win32-x64" = "aefb8c905b8cf7d80403f3a04994d34985548570fd6b5ca7c872c05f2401a278",
    "v26.6.0_darwin-arm64" = "c78e32c466c009dee1c60b1a0791231d4b7e5e29178da988596bbd79f4c98f72",
    "v26.6.0_darwin-x64" = "f4d63a46f4c1417a43765988bba0be21e10aba9a74bc0f9b3498f70be717f944",
    "v26.6.0_linux-arm64" = "c59f5354fd877dd119eb58ea672ef8c81bf3dbda18731765f8c9e632728bd92c",
    "v26.6.0_linux-x64" = "b9d5732800e8e0132793e9133c57aace76dfd0fed78bb35bedbde81999af09a7",
    "v26.6.0_win32-x64" = "d8624cfd2dad0e6d85607e91b0815d3d9f84b6c5089acd1ce6945ec5677844b5",
    "v26.5.0_darwin-arm64" = "16b6a10bf7f3d7a884468933a6ec2aecab7c02e2a7af0590044f6ff3ae7a2955",
    "v26.5.0_darwin-x64" = "35555724647cb2921768a84de7215032c17a0c0a17d6aff6ad245ee002921bbb",
    "v26.5.0_linux-arm64" = "069e9ab25cac4f2b305036bc53ea29972f5e8da6678a8fa3ea26027895468bad",
    "v26.5.0_linux-x64" = "fd629f45fd452e71a78cffa69cc164621919b838c8549dffac88ab9dc1afd589",
    "v26.5.0_win32-x64" = "28c4574932af01fbe64e1ec6dbfbe967686563da38b1a9d7189082c0d48cc7b4",
    "v26.4.1_darwin-arm64" = "089a826616bd103485801ee57535c9ecfcddd1b553d95d54158fcbe0d5578bf2",
    "v26.4.1_darwin-x64" = "177543c1fb053f286c253416a54afc800c9f621221a536228960de1e80e9326c",
    "v26.4.1_linux-arm64" = "9a558680fefd1930207aaed5be25e90e89a0bbe2cf85fccec7e44eca99b24af8",
    "v26.4.1_linux-x64" = "f45c663a3b247458b7cb3c978315582caef84912cde348939ef27e297ac2928e",
    "v26.4.1_win32-x64" = "29a81582997722a6faf67d47a2e9450d5d9b4168904d285b608702a723eeef51",
    "v26.4.0_darwin-arm64" = "655343e19e7d65a7decf65602d8dadcb18a5d2c6ae5de04e2d54e111d6d8bd20",
    "v26.4.0_darwin-x64" = "0162fc874e9a4822cef2d3640ff5ae0a5c5522ca69eb8eec7e5eb1fa615489f8",
    "v26.4.0_linux-arm64" = "2369a0bd60eb35a4fc07756c0ca5fdd00daa8af0a0b5398ce79d2dec01681d8d",
    "v26.4.0_linux-x64" = "f2e527133f532dee8a82db6efac82d6a31825281d2e1fffdfc03b855d9649912",
    "v26.4.0_win32-x64" = "d56cfabb9018f5f3fb58f9fdcf278696e1220605ff62c6758da56202bc6d06c3",
    "v6.0.0_darwin-arm64" = "0b8a0fdeb43577e77da6324dca9136965d99492d37fbc319afe990dee19d632a",
    "v6.0.0_darwin-x64" = "5784f0a35983c9ff1a4804b5c6c959dd3ba7304e83785cdb710714f1467fdfc3",
    "v6.0.0_linux-arm64" = "f181b6e1d35475b123d6617436e08986693c9ad7be2269f48aa959d79b7deee4",
    "v6.0.0_linux-x64" = "673a94e1f4a82169cf6d8186458e6476eb91c6f3f2cc0d7d27c8f0b08eed0e98",
    "v6.0.0_win32-x64" = "ad0e78aab2f0ff1686c495f6c0876afde7087a586d0154825c396f93ee7c4bbf",
    "v5.27.1_darwin-arm64" = "7fbc441a42feb414f247e8df1b99f839fe3b0e2433ac66e55cebd547c84592ff",
    "v5.27.1_darwin-x64" = "4d6b09a0da37b1c7b63e1420f899a90896354e2e22827613a827e35736ce906d",
    "v5.27.1_linux-arm64" = "5e023e34af3c71326fbb32fbbd354ccf7262a878e1340c2bbf3bc72832c005c7",
    "v5.27.1_linux-x64" = "851461c905a1529706b856ca0e750944dd07e3a0b0109f3b66d06a719e3e450b",
    "v5.27.1_win32-x64" = "f4de1ab2317adf74f02b5f71648086f06b725f2fd57c4e2b1fbc0a14a75959db"
  )

  if (key %in% names(KNOWN_HASHES)) {
    return(KNOWN_HASHES[[key]])
  }
  
  # Fallback: attempt to fetch checksums.txt from GitHub release
  if (identical(getOption("osrm.backend.repository"), "e-kotov/osrm-binaries")) {
    checksum_url <- sprintf("https://github.com/e-kotov/osrm-binaries/releases/download/%s/checksums.txt", version)
    tryCatch({
      lines <- readLines(checksum_url, warn = FALSE)
      # Search for the hash matching our expected filename
      tar_filename <- sprintf("osrm-%s-%s-%s-Release.tar.gz", version, platform$os, platform$arch)
      # Handle legacy naming conventions in older scripts if necessary
      if (platform$os == "win32" && platform$arch == "x64") {
        tar_filename <- sprintf("osrm-%s-win32-x64-Release.tar.gz", version)
      }
      for (line in lines) {
        if (grepl(tar_filename, line, fixed = TRUE)) {
          hash <- strsplit(trimws(line), "\\s+")[[1]][1]
          return(hash)
        }
      }
    }, error = function(e) NULL)
  }
  
  return(NA_character_)
}
