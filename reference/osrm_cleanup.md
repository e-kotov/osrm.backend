# Clean Up OSRM Files in a Directory

Remove OSRM-generated files from a directory. This is useful when
switching between algorithms (CH and MLD) or when you want to start
fresh.

## Usage

``` r
osrm_cleanup(path, keep_osm = TRUE, dry_run = FALSE, quiet = FALSE)
```

## Arguments

- path:

  A string. Path to an OSRM file or directory containing OSRM files. If
  a file path is provided (e.g., `data.osm.pbf` or `data.osrm.hsgr`),
  the base name will be extracted and all related `.osrm.*` files will
  be removed.

- keep_osm:

  Logical. If `TRUE` (default), keeps the original `.osm.pbf` (or
  `.osm`, `.osm.bz2`) file. If `FALSE`, removes it as well.

- dry_run:

  Logical. If `TRUE`, shows what would be deleted without actually
  deleting. Default is `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses messages. Default is `FALSE`.

## Value

Invisibly returns a character vector of removed file paths.

## Details

OSRM creates many `.osrm.*` files during the extract, contract,
partition, and customize stages. This function helps clean up these
files.

**Important:** The CH and MLD algorithms cannot safely coexist in the
same directory because the MLD partition stage modifies some
extract-stage files. Use this function to clean up before switching
algorithms.

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  install_dir <- osrm_install(
    version = "latest",
    path_action = "session",
    quiet = TRUE
  )

  # Stage a temporary workspace with placeholder OSRM files
  pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
  dir.create(osrm_dir, recursive = TRUE)
  tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
  file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
  file.create(
    file.path(osrm_dir, "cur.osrm.timestamp"),
    file.path(osrm_dir, "cur.osrm.hsgr"),
    file.path(osrm_dir, "cur.osrm.mldgr"),
    file.path(osrm_dir, "cur.osrm.partition")
  )

  # Preview what would be deleted
  osrm_cleanup(osrm_dir, dry_run = TRUE)

  # Clean up OSRM artifacts (keep the OSM file)
  osrm_cleanup(osrm_dir)

  # Remove everything including the OSM source
  osrm_cleanup(osrm_dir, keep_osm = FALSE)

  osrm_uninstall(
    dest_dir = install_dir,
    clear_path = TRUE,
    force = TRUE,
    quiet = TRUE
  )
  unlink(osrm_dir, recursive = TRUE)
}
# }
```
