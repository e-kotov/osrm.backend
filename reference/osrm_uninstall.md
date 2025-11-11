# Uninstall OSRM Backend Binaries

Removes the OSRM backend binaries and optionally clears the `PATH`
configuration from the project's `.Rprofile`.

## Usage

``` r
osrm_uninstall(dest_dir = NULL, clear_path = TRUE, quiet = FALSE)
```

## Arguments

- dest_dir:

  A string specifying the directory from which to remove OSRM binaries.
  If `NULL` (the default), the function looks for an installation in the
  per-version subdirectories inside
  `tools::R_user_dir("osrm.backend", which = "cache")` and removes it.
  When multiple versions are installed, interactive sessions that are
  not `quiet` will be prompted (with a numbered menu and `0` to cancel)
  to choose a directory; otherwise, `dest_dir` must be supplied.

- clear_path:

  A logical value. If `TRUE` (default), also removes the `PATH`
  configuration from the project's `.Rprofile` by calling
  [`osrm_clear_path()`](https://www.ekotov.pro/osrm.backend/reference/osrm_clear_path.md).

- quiet:

  A logical value. If `TRUE`, suppresses informational messages and
  confirmation prompts. Defaults to `FALSE`.

## Value

Invisibly returns `TRUE` if the directory was successfully removed, and
`FALSE` otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
# Uninstall OSRM and clear .Rprofile (will ask for confirmation)
osrm_uninstall()

# Only uninstall binaries, leave .Rprofile untouched
osrm_uninstall(clear_path = FALSE)
} # }
```
