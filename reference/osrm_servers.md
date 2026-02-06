# List OSRM servers started via this package

Returns a snapshot of servers registered by
[`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md)
or
[`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md).
You can stop one by passing its `id`, `port`, or `pid` to
[`osrm_stop()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop.md).

## Usage

``` r
osrm_servers()
```

## Value

A data.frame with columns: `id`, `pid`, `port`, `algorithm`,
`started_at`, `alive`, `has_handle`, `log`.

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
  graph <- osrm_prepare_graph(tmp_pbf, overwrite = TRUE, threads = 1L)

  srv <- osrm_start_server(graph$osrm_job_artifact, port = 6000, threads = 1L)
  osrm_servers()
  osrm_stop(srv)

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
