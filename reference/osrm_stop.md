# Stop an OSRM Server

Terminates an `osrm-routed` process launched by
[`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md)
or
[`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md).

## Usage

``` r
osrm_stop(
  server = NULL,
  id = NULL,
  port = NULL,
  pid = NULL,
  wait = 1000L,
  quiet = FALSE
)
```

## Arguments

- server:

  Optional
  [`processx::process`](http://processx.r-lib.org/reference/process.md)
  object returned by
  [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md).

- id:

  Optional character id from
  [`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md).

- port:

  Optional integer TCP port.

- pid:

  Optional integer process id.

- wait:

  Integer milliseconds to wait for clean shutdown (default `1000`).

- quiet:

  Logical; suppress messages (default `FALSE`).

## Value

A list with fields `id`, `pid`, `port`, `stopped` (logical).

## Details

This function provides a flexible way to stop a running OSRM process. If
no arguments are specified, it defaults to stopping the most recently
started server that is still alive.

You can also stop a specific server by providing:

- The
  [`processx::process`](http://processx.r-lib.org/reference/process.md)
  object returned by
  [`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md)
  or
  [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md).

- The server's `id`, `port`, or `pid` (use
  [`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md)
  to find these).

## See also

[`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md),
[`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md),
[`osrm_stop_all()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop_all.md)

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

  # Stop by passing the process object
  osrm_stop(srv)

  # Or stop by port after the process is registered
  osrm_stop(port = 6000)

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
