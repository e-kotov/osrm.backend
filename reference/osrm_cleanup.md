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
if (FALSE) { # \dontrun{
# Clean up all OSRM files in a directory (keeps .osm.pbf)
osrm_cleanup("/path/to/osrm/files")

# Clean up including the OSM file
osrm_cleanup("/path/to/osrm/files", keep_osm = FALSE)

# Preview what would be deleted
osrm_cleanup("/path/to/osrm/files", dry_run = TRUE)

# Clean up specific base name
osrm_cleanup("/path/to/data.osm.pbf")
osrm_cleanup("/path/to/data.osrm.hsgr")
} # }
```
