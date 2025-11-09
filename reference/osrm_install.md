# Install OSRM Backend Binaries

Downloads and installs pre-compiled binaries for the OSRM backend from
the official GitHub releases. The function automatically detects the
user's operating system and architecture to download the appropriate
files. Only the latest v5 release (`v5.27.1`) and `v6.0.0` were manually
tested and are known to work well; other releases available on GitHub
can be installed but are not guranteed to function correctly.

## Usage

``` r
osrm_install(
  version = "v5.27.1",
  dest_dir = NULL,
  force = FALSE,
  path_action = c("session", "project", "none"),
  quiet = FALSE
)
```

## Arguments

- version:

  A string specifying the OSRM version tag to install. Defaults to
  `"v5.27.1"`. Use `"latest"` to automatically find the most recent
  stable version by calling
  [`osrm_check_latest_version()`](https://www.ekotov.pro/osrm.backend/reference/osrm_check_latest_version.md).
  Versions other than `v5.27.1` and `v6.0.0` will trigger a warning but
  are still attempted if binaries are available.

- dest_dir:

  A string specifying the directory where OSRM binaries should be
  installed. If `NULL` (the default), a user-friendly, persistent
  location is chosen via
  `tools::R_user_dir("osrm.backend", which = "cache")`, and the binaries
  are installed into a subdirectory named after the OSRM version (e.g.
  `.../cache/v6.0.0`).

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

  A logical value. If `TRUE`, suppresses messages. Defaults to `FALSE`.
  Defaults to `FALSE`.

## Value

The path to the installation directory, invisibly.

## Details

The function performs the following steps:

1.  Queries the GitHub API to find the specified release of
    `Project-OSRM/osrm-backend`.

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

macOS users should note that upstream OSRM v6.x binaries are built for
macOS 15.0 (Sequoia, Darwin 24.0.0) or newer. `osrm_install()`
automatically blocks v6 installs on older macOS releases and, when
`version = "latest"`, selects the most recent v5 build instead while
warning about the requirement. Warnings include both the marketing
version and Darwin kernel so you'll see messages like
`macOS 13 Ventura [Darwin 22.6.0]`.

When installing OSRM v6.x for Windows, the upstream release omits the
Intel Threading Building Blocks (TBB) runtime and a compatible `bz2`
DLL. To keep the executables runnable out of the box, `osrm_install()`
fetches TBB from [oneTBB
v2022.3.0](https://github.com/uxlfoundation/oneTBB/releases/tag/v2022.3.0)
and the BZip2 runtime from [bzip2-windows
v1.0.8.0](https://github.com/philr/bzip2-windows/releases/tag/v1.0.8.0),
verifying their SHA-256 checksums before extraction. Without these extra
libraries, the OSRM v6 binaries shipped for Windows cannot start.

On macOS, OSRM v6.x binaries also miss the bundled TBB runtime. The
installer reuses the libraries from release `v5.27.1` to keep the
binaries functional and patches their `libbz2` linkage using
`install_name_tool` so that they load the system-provided BZip2 runtime.

Power users (including package authors running cross-platform tests) can
override the auto-detected platform by setting the R options
`osrm.backend.override_os` and `osrm.backend.override_arch` (e.g.,
`options(osrm.backend.override_os = "linux", osrm.backend.override_arch = "arm64")`)
before calling `osrm_install()`. Overrides allow requesting binaries for
any OS and CPU combination that exists on the GitHub releases.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install the default stable version and set PATH for this session
osrm_install()

# Install for a project non-interactively (e.g., in a script)
osrm_install(path_action = "project", quiet = TRUE)

# Clean up the project's .Rprofile
osrm_clear_path()
} # }
```
