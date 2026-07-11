# Fetch validated OSRM versions from per-OS badge JSON

The package's live integration workflow publishes per-OS JSON "badges"
(for example `osrm_versions_ubuntu.json`) to the `badges` branch. These
files contain a short message describing the validated range or latest
validated versions. This helper fetches that JSON and (optionally)
expands a range to the list of release tags available in the
`e-kotov/osrm-binaries` repository.

## Usage

``` r
osrm_validated_versions(
  os = c("linux", "macos", "windows", "all"),
  repo = "e-kotov/osrm.backend",
  branch = "badges",
  expand = TRUE,
  gh_token = NULL
)
```

## Arguments

- os:

  Character, one of `"linux"`, `"macos"`, `"windows"`, or `"all"`.
  `"linux"` reads the `osrm_versions_ubuntu.json` badge. `"all"` returns
  a named list with entries for each OS.

- repo:

  Character, repository hosting the badges. Defaults to
  `"e-kotov/osrm.backend"`.

- branch:

  Character, the branch where badges are stored. Defaults to `"badges"`.

- expand:

  Logical, if `TRUE` and the badge message encodes a simple range (e.g.
  `v5.27.1 - v26.7.3`), attempt to expand the range to the list of
  available release tags from `e-kotov/osrm-binaries`. If expansion
  fails, the function falls back to returning the raw badge message.

- gh_token:

  Optional GitHub token for API requests. By default the function does
  not use authentication (public raw.githubusercontent URLs).

## Value

If `os != "all"`: a list with elements `label`, `message`, `raw` (the
parsed JSON). If `expand = TRUE` and expansion succeeded, returns a
character vector of tag names (newest first). If `os = "all"`, returns a
named list with one entry per OS.

## Examples

``` r
if (FALSE) { # \dontrun{
osrm_validated_versions("linux")
osrm_validated_versions("all", expand = FALSE)
} # }
```
