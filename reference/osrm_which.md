# Locate the OSRM Installation Used by `osrm.backend`

Resolves the `osrm-routed` executable available on the current `PATH`
(or the override provided via `options(osrm.routed.exec)`). Runs
`osrm-routed --version` to verify availability, then prints the
directory containing the executable together with the backend version
reported by `osrm-routed` so you know what will be used in the current
session.

## Usage

``` r
osrm_which(quiet = FALSE)
```

## Arguments

- quiet:

  Logical; if `FALSE` (default), prints information about the located
  installation. If `TRUE`, suppresses printed output and only returns
  the information as a list.

## Value

A list with components `executable` (full path to `osrm-routed`),
`directory` (its parent folder), `osrm_version` (character vector of
non-empty lines emitted by `osrm-routed --version`), and the raw
[`processx::run`](http://processx.r-lib.org/reference/run.md) result in
`logs`.

## Examples

``` r
if (FALSE) { # \dontrun{
# check which OSRM installation will be used
osrm_which()
} # }
```
