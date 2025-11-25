# Locate an OSRM Lua profile (e.g. car.lua) in a host installation

By default OSRM ships profiles for "car", "bike" and "foot" in a
`profiles/` directory alongside the binaries. This function will try to
locate `osrm-routed` on the `PATH`, resolve symlinks, and look first for
a `profiles/` directory next to the binary (as placed there by
[`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)).
If that fails, it looks for sibling directories `share/osrm/profiles`
and `share/osrm-backend/profiles`. IF that fails, it will try to fall
back on
`/usr/local/share/osrm/profiles`,`/usr/local/share/osrm-backend/profiles`,
`/usr/share/osrm/profiles`, and `/usr/share/osrm-backend/profiles`.

## Usage

``` r
osrm_find_profile(profile = "car.lua")
```

## Arguments

- profile:

  A single string, the name of the Lua profile file (e.g. `"car.lua"`).
  Defaults to `"car.lua"`.

## Value

The normalized filesystem path to the profile.

## Examples

``` r
if (FALSE) { # \dontrun{
# make sure osrm backend is installed and on PATH
osrm_install()
osrm_find_profile("car.lua")
} # }
```
