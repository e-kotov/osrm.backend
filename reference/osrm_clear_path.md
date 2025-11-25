# Clear OSRM Path from Project's .Rprofile

Scans the `.Rprofile` file in the current project's root directory and
removes any lines that were added by
[`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)
to modify the `PATH`.

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
if (FALSE) { # \dontrun{
# Clean up the project's .Rprofile
osrm_clear_path()
} # }
```
