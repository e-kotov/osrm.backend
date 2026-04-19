# Stop an OSRM Server

Terminates an `osrm-routed` process launched by
[`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md)
or
[`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md).
Can also stop external servers by PID or ID.

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

  Optional OSRM job process (an `osrm_server` object inheriting from
  [`processx::process`](http://processx.r-lib.org/reference/process.md))
  returned by
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
started server that is still alive in the current session.

You can also stop a specific server by providing:

- The OSRM job process (a
  [`processx::process`](http://processx.r-lib.org/reference/process.md)
  object) returned by
  [`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md)
  or
  [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md).

- The server's `id`, `port`, or `pid` (use
  [`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md)
  to find these).

**Advanced Use:** You can stop an external `osrm-routed` process (one
not started by the current R session) by passing its PID, or by finding
it via `osrm_servers(include_all = TRUE)` and passing its `id` or
`port`. This requires permission to signal the process.

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

if (FALSE) { # \dontrun{
  # Advanced: Stop an external server by PID
  # 1. Find the PID of an external server
  srvs <- osrm_servers(include_all = TRUE)
  # 2. Stop it by PID
  if (nrow(srvs) > 0) {
    osrm_stop(pid = srvs$pid[1])
  }
  
  # Or stop by its external ID (e.g., "sys-12345")
  osrm_stop(id = "sys-12345")
} # }
# }
```
