# Install OSRM Backend Binaries

**\[stable\]**

Downloads and installs pre-compiled binaries for the OSRM backend. By
default, `osrm_install()` uses the `e-kotov/osrm-binaries` release
repository, which publishes immutable backend command-line archives
primarily for this R package. The upstream `Project-OSRM/osrm-backend`
release repository can still be used with
`osrm_binaries_provider = "official"`.

The function automatically detects the user's operating system and
architecture to download the appropriate files. Validated versions are
maintained by the package's live integration tests; see the OSRM live
tests workflow and per-OS badges on GitHub Actions for the current
validated versions. Other releases available on GitHub can be installed
but are not guaranteed to function correctly.

## Usage

``` r
osrm_install(
  version = "latest",
  osrm_binaries_provider = c("default", "official"),
  dest_dir = NULL,
  force = FALSE,
  path_action = c("session", "project", "none"),
  quiet = FALSE,
  check_tested = TRUE,
  download_url = NULL,
  file_path = NULL
)
```

## Arguments

- version:

  A string specifying the OSRM version tag to install. Defaults to
  `"latest"`. Use `"latest"` to automatically find the most recent
  stable version (internally calls
  [`osrm_check_latest_version()`](https://www.ekotov.pro/osrm.backend/reference/osrm_check_latest_version.md)).
  Versions published in `e-kotov/osrm-binaries` and validated by this
  package are installed without warnings; other available versions are
  still attempted with a warning.

- osrm_binaries_provider:

  A string specifying the provider to download binaries from. Defaults
  to `"default"`, which pulls from `"e-kotov/osrm-binaries"` and is the
  primary supported provider for this package. Set to `"official"` to
  download upstream binaries from `"Project-OSRM/osrm-backend"` where
  available. Advanced users can override the provider completely by
  setting the R option `osrm.backend.custom_repository` to a custom
  GitHub repository (e.g. `"my-user/my-repo"`).

- dest_dir:

  A string specifying the directory where OSRM binaries should be
  installed. If `NULL` (the default), a user-friendly, persistent
  location is chosen via
  `tools::R_user_dir("osrm.backend", which = "cache")`, and the binaries
  are installed into a subdirectory named after the OSRM version (e.g.
  `.../cache/v26.4.1`).

- force:

  A logical value. If `TRUE`, reinstall OSRM even if it's already found
  in `dest_dir`. If `FALSE` (default), the function will stop if an
  existing installation is detected.

- path_action:

  A string specifying how to handle the system `PATH`. One of:

  - `"session"` (default): Adds the OSRM bin directory to the `PATH` for
    the rest of the current R session. This intentionally changes the
    session environment and is not reset automatically.

  - `"project"`: Modifies the `.Rprofile` in the current project to set
    the `PATH` for future sessions in that project. Use
    [`osrm_clear_path()`](https://www.ekotov.pro/osrm.backend/reference/osrm_clear_path.md)
    to remove lines added by `osrm.backend`.

  - `"none"`: Does not modify the `PATH`.

- quiet:

  A logical value. If `TRUE`, suppresses installer messages and
  warnings. Defaults to `FALSE`.

- check_tested:

  A logical value. If `TRUE` (default), the function emits a status
  message pointing to the live integration-test badges that report
  currently validated OSRM versions. If `FALSE`, this status message is
  suppressed.

- download_url:

  **Advanced usage only.** A direct URL to a `.tar.gz` archive
  containing OSRM binaries. If provided, `version` and
  `osrm_binaries_provider` are ignored. The archive must be structured
  similarly to the default releases, containing the required OSRM
  executables (at least `osrm-routed` or `osrm-routed.exe`) either
  directly at the root of the archive or nested under a single directory
  level. Supporting libraries and Lua profiles placed in the same folder
  will be installed alongside.

- file_path:

  **Advanced usage only.** A local file path to a `.tar.gz` archive
  containing OSRM binaries. If provided, skips downloading entirely. The
  archive structure expectations are identical to those of
  `download_url`.

## Value

The path to the installation directory.

## Details

The function performs the following steps:

1.  Queries the GitHub API to find the specified release of
    `e-kotov/osrm-binaries`.

2.  Identifies the correct binary (`.tar.gz` archive) for the user's OS
    (Linux, macOS, or Windows) and architecture (x64, arm64).

3.  Downloads the archive to a temporary location.

4.  Extracts the archive and locates the OSRM executables (e.g.,
    `osrm-routed`, `osrm-extract`).

5.  Copies these executables to a local directory (defaults to
    `file.path(tools::R_user_dir("osrm.backend", which = "cache"), <version>)`).

6.  Downloads the matching Lua profiles from the release tarball and
    installs them alongside the binaries.

7.  Optionally modifies the `PATH` environment variable for the current
    session or project.

### Binary Providers

The `osrm_binaries_provider` argument allows choosing between two
release sources:

- **`"default"` (e-kotov/osrm-binaries)**: This provider is highly
  recommended and is the primary supported path for this R package. It
  offers standalone, immutable OSRM backend archives with predictable
  asset names, bundled runtime libraries where practical, SHA-256
  verification, and no Node.js wrapper artifacts. It also provides
  native `linux-arm64` and `darwin-arm64` archives.

- **`"official"` (Project-OSRM/osrm-backend)**: The upstream releases
  provided by the core OSRM team. These releases are intended primarily
  for upstream OSRM's Node.js distribution and are packaged as
  `node_osrm` archives. Their platform coverage and bundled runtime
  libraries may differ across OSRM versions, and recent upstream
  binaries may depend on runtime libraries from newer build
  environments. `osrm_install()` keeps compatibility code for this
  provider, but it is a secondary path rather than the package's main
  installation source.

Power users (including package authors running cross-platform tests) can
override the auto-detected platform by setting the R options
`osrm.backend.override_os` and `osrm.backend.override_arch` (e.g.,
`options(osrm.backend.override_os = "linux", osrm.backend.override_arch = "arm64")`)
before calling `osrm_install()`. Overrides allow requesting binaries for
any OS and CPU combination that exists on the GitHub releases.

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)

  # Install the default stable version and set PATH for this session
  install_dir <- osrm_install(path_action = "session", quiet = TRUE)

  # Install for a project non-interactively (e.g., in a script)
  osrm_install(path_action = "project", quiet = TRUE, force = TRUE)

  # Clean up the project's .Rprofile and uninstall binaries
  osrm_clear_path(quiet = TRUE)
  osrm_uninstall(
    dest_dir = install_dir,
    clear_path = TRUE,
    force = TRUE,
    quiet = TRUE
  )
}
# }
```
