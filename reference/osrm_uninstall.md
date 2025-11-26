# Uninstall OSRM Backend Binaries

Removes the OSRM backend binaries and optionally clears the `PATH`
configuration from the project's `.Rprofile`.

## Usage

``` r
osrm_uninstall(
  dest_dir = NULL,
  clear_path = TRUE,
  quiet = FALSE,
  all = FALSE,
  force = FALSE
)
```

## Arguments

- dest_dir:

  A string specifying the directory from which to remove OSRM binaries.
  If `NULL` (the default), the function looks for an installation in the
  per-version subdirectories inside
  `tools::R_user_dir("osrm.backend", which = "cache")` and removes it.
  When multiple versions are installed, interactive sessions that are
  not `quiet` will be prompted (with a numbered menu and `0` to cancel)
  to choose a directory; otherwise, `dest_dir` must be supplied. Ignored
  if `all = TRUE`.

- clear_path:

  A logical value. If `TRUE` (default), also removes the `PATH`
  configuration from the project's `.Rprofile` by calling
  [`osrm_clear_path()`](https://www.ekotov.pro/osrm.backend/reference/osrm_clear_path.md).

- quiet:

  A logical value. If `TRUE`, suppresses informational messages and
  confirmation prompts. Defaults to `FALSE`.

- all:

  A logical value. If `TRUE`, removes all OSRM installations found in
  the default cache directory. Will prompt for confirmation unless
  `force = TRUE`. Defaults to `FALSE`. When `TRUE`, the `dest_dir`
  parameter is ignored.

- force:

  A logical value. If `TRUE`, skips all confirmation prompts, enabling
  non-interactive usage. Defaults to `FALSE`.

## Value

`TRUE` if one or more directories were successfully removed, and `FALSE`
otherwise.

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  # Install OSRM temporarily
  install_dir <- osrm_install(path_action = "session", quiet = TRUE)

  # Uninstall that specific version and clear PATH changes
  osrm_uninstall(
    dest_dir = install_dir,
    clear_path = TRUE,
    force = TRUE,
    quiet = TRUE
  )

  # If multiple installs exist, remove them all
  osrm_uninstall(all = TRUE, force = TRUE, quiet = TRUE)
}
# }
```
