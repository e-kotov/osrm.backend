# Print summary for an `osrm_job` object

Displays pipeline state, outputs, and next steps for an `osrm_job`
returned by functions such as
[`osrm_extract()`](https://www.ekotov.pro/osrm.backend/reference/osrm_extract.md),
[`osrm_prepare_graph()`](https://www.ekotov.pro/osrm.backend/reference/osrm_prepare_graph.md),
[`osrm_partition()`](https://www.ekotov.pro/osrm.backend/reference/osrm_partition.md),
or
[`osrm_contract()`](https://www.ekotov.pro/osrm.backend/reference/osrm_contract.md).

## Usage

``` r
# S3 method for class 'osrm_job'
print(x, ...)
```

## Arguments

- x:

  An `osrm_job` object.

- ...:

  Passed to methods; currently ignored.

## Value

Invisibly returns `x`.

## Examples

``` r
# \donttest{
if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
  install_dir <- osrm_install(
    version = "latest",
    path_action = "session",
    quiet = TRUE
  )

  # copy example OSM PBF into a temporary workspace to avoid polluting pkg data
  pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
  dir.create(osrm_dir, recursive = TRUE)
  tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
  file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)

  job <- osrm_prepare_graph(tmp_pbf, overwrite = TRUE, threads = 1L)
  print(job)

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
