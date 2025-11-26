# Check for the Latest Stable OSRM Version

Queries the GitHub API to find the most recent stable (non-pre-release)
version tag for the OSRM backend that has binaries available for the
current platform.

## Usage

``` r
osrm_check_latest_version()
```

## Value

A string containing the latest version tag (e.g., `"v5.27.1"`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Get the latest stable version number of OSRM backend
osrm_check_latest_version()
} # }
```
