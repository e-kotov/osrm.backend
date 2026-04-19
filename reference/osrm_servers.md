# List OSRM servers

**\[stable\]**

Lists `osrm-routed` processes. By default, it returns a snapshot of
servers started by the current R session (registered via
[`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md)
or
[`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md)).
You can optionally list all `osrm-routed` processes running on the
system, including those started by other sessions or manually.

You can stop a server by passing its `id`, `port`, or `pid` to
[`osrm_stop()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop.md).

## Usage

``` r
osrm_servers(include_all = FALSE, output = c("data.frame", "list"))
```

## Arguments

- include_all:

  Logical; if `TRUE`, scans the system process table for all
  `osrm-routed` processes, including those not started by this package
  in the current session. Default is `FALSE`.

- output:

  Character string specifying the return format. Either `"data.frame"`
  (the default) which returns a tabular summary with a custom print
  method, or `"list"` which returns a detailed list of server metadata
  objects.

## Value

If `output = "data.frame"`, returns a `data.frame` (class
`osrm_server_list`) of OSRM job processes with columns: `id`, `pid`,
`port`, `algorithm`, `started_at`, `alive`, `has_handle`, `log`,
`input_osm`, `center_lon`, `center_lat`. External servers will have `id`
prefixed with `sys-` and `log` set to `<external>`. If
`output = "list"`, returns a named list of server metadata.

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
