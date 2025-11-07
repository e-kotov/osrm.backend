# Check for Available OSRM Versions

Queries the GitHub API to get a list of all available version tags for
the OSRM backend that have binaries for the current platform.

## Usage

``` r
osrm_check_available_versions(prereleases = FALSE)
```

## Arguments

- prereleases:

  A logical value. If `TRUE`, include pre-release versions in the
  returned list. Defaults to `FALSE`.

## Value

A character vector of available version tags.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get all stable versions with binaries for this platform
stable_versions <- osrm_check_available_versions()
head(stable_versions)

# Get all versions, including pre-releases, with binaries
all_versions <- osrm_check_available_versions(prereleases = TRUE)
head(all_versions)
} # }
```
