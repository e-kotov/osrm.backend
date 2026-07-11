# Clear OSRM Path from Project's .Rprofile

Scans the `.Rprofile` file in the current project's root directory and
removes lines that were added by `osrm_install(path_action = "project")`
to modify the `PATH` for future R sessions in that project.

## Usage

``` r
osrm_clear_path(quiet = FALSE)
```

## Arguments

- quiet:

  A logical value. If `TRUE`, suppresses messages. Defaults to `FALSE`.

## Value

`TRUE` if the file was modified, `FALSE` otherwise.

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  # Clean up a temporary project's .Rprofile
  old <- setwd(tempdir())
  on.exit(setwd(old), add = TRUE)
  writeLines(
    c(
      "#added-by-r-pkg-osrm.backend",
      'Sys.setenv(PATH = paste("dummy", Sys.getenv("PATH"), sep = .Platform$path.sep))'
    ),
    ".Rprofile"
  )
  osrm_clear_path(quiet = TRUE)
  unlink(".Rprofile")
}
# }
```
