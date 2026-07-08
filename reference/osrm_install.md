# Install OSRM Backend Binaries

**\[stable\]**

Downloads and installs pre-compiled binaries for the OSRM backend from
the official GitHub releases. The function automatically detects the
user's operating system and architecture to download the appropriate
files. Only the latest v5 release (`v5.27.1`), `v6.0.0`, `v26.4.0` and
`v26.4.1` were manually tested and are known to work well; other
releases available on GitHub can be installed but are not guranteed to
function correctly.

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
  Versions other than `v5.27.1`, `v6.0.0`, `v26.4.0`, `v26.4.1`, and
  `v26.5.0` will trigger a warning but are still attempted if binaries
  are available.

- osrm_binaries_provider:

  A string specifying the provider to download binaries from. Defaults
  to `"default"`, which pulls from `"e-kotov/osrm-binaries"` providing
  custom immutable builds (statically linked) that fix glibc
  compatibility issues and bundle necessary libraries. Set to
  `"official"` to download the upstream binaries from
  `"Project-OSRM/osrm-backend"` (which will automatically trigger legacy
  runtime hacks for macOS and Windows). Advanced users can override the
  provider completely by setting the R option
  `osrm.backend.custom_repository` to a custom GitHub repository (e.g.
  `"my-user/my-repo"`).

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
    the current R session only.

  - `"project"`: Modifies the `.Rprofile` in the current project to set
    the `PATH` for all future sessions in that project.

  - `"none"`: Does not modify the `PATH`.

- quiet:

  A logical value. If `TRUE`, suppresses installer messages and
  warnings. Defaults to `FALSE`.

- check_tested:

  A logical value. If `TRUE` (default), the function issues a warning if
  the requested OSRM version has not been explicitly validated by the
  package maintainers. If `FALSE`, it issues a status message instead.

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
  recommended. It offers standalone, immutable C++ binaries that are
  statically linked and patched during the build process to guarantee
  maximum compatibility across older and newer operating systems. It
  explicitly bundles required libraries (like Intel TBB), skips NodeJS
  bloat, and provides exclusive support for `linux-arm64` (e.g., AWS
  Graviton, Raspberry Pi) and modern Apple Silicon Macs.

- **`"official"` (Project-OSRM/osrm-backend)**: The upstream releases
  provided by the core OSRM team. These binaries are wrapped inside
  `node_osrm` NodeJS tarballs and lack `linux-arm64` builds. When
  installing v6.x or newer via the `"official"` provider on Windows or
  macOS, upstream releases omit the Intel TBB runtime. In these cases,
  `osrm_install()` will automatically attempt legacy runtime hacks (e.g.
  downloading oneTBB, patching `libbz2` linkage via `install_name_tool`)
  to keep the binaries functional out-of-the-box.

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
