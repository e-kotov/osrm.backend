# Launch a GUI to View and Debug OSRM Routing

**\[experimental\]**

Launches a lightweight Shiny application to interactively visualize
routing on a local OSRM server. This interface mimics the `r5rgui`
experience, supporting left-click for start, right-click for end, and
draggable markers.

## Usage

``` r
osrm_gui(
  input_osrm = NULL,
  port = "auto",
  style = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
  center = NULL,
  zoom = NULL,
  autozoom = TRUE,
  update_while_drag = FALSE,
  debug = FALSE
)
```

## Arguments

- input_osrm:

  Optional. Can be:

  - An OSRM job process (an `osrm_server` object inheriting from
    [`processx::process`](http://processx.r-lib.org/reference/process.md))
    returned by
    [`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md)
    or
    [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md).
    When providing a process, you must also specify `port` explicitly.

  - A path string to an `.osrm.hsgr` or `.osrm.mldgr` file.

  - A path string to an `.osm.pbf` file (will be prepared and started).

  - `NULL` (default): Auto-detects a running OSRM server using
    [`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md).
    Errors if no servers are running.

- port:

  Integer or `"auto"`. The port the server is running on (or should run
  on). Defaults to `"auto"`, which attempts to auto-detect a running
  OSRM server using
  [`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md).
  If multiple servers are running, the most recent one is selected with
  a warning. If no servers are running, an error is raised.

- style:

  Character. Map style for `mapgl`. Defaults to
  "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json".

- center:

  Numeric vector of length 2 (`c(lng, lat)`), or named list
  (`list(lng = ..., lat = ...)`), or `NULL` (default). Initial map
  center. If `NULL` and `input_osrm` is a `.osm.pbf` file, attempts to
  auto-center on the PBF extent. Priority is given to a fast pure R
  header parser and `osmium fileinfo` (fast); otherwise estimates the
  extent by sampling a small number of features via
  [`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html)
  (for example, reading with a `LIMIT 10` query).

- zoom:

  Numeric. Initial zoom level. If `NULL` (default) and center is
  auto-detected from PBF, defaults to 9. Otherwise uses map default.

- autozoom:

  Logical. Whether to enable auto-zoom by default. Defaults to `TRUE`.

- update_while_drag:

  Logical. Whether to enable live tracking mode by default (updates
  route while dragging). Defaults to `FALSE`.

- debug:

  Logical. Whether to enable debug mode (prints OSRM requests to
  console). Defaults to `FALSE`.

## Value

No return value; launches a Shiny Gadget.

## Details

The function checks for optional dependencies `shiny`, `mapgl`, `osrm`,
`sf`, and `DT`. If missing, it prompts the user to install them.

It attempts to detect an active OSRM server. If an OSRM job process
(from
[`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md))
is passed, it uses that configuration. If a path is passed, it will
start a temporary server for the session.

## Examples

``` r
if (interactive()) {
  # 1. Auto-detect running server (errors if none running):
  osrm_gui()

  # 2. Connect to specific port:
  # osrm_gui(port = 5001)

  # 3. Start from a graph file (auto-center on PBF):
  # osrm_gui("berlin.osrm.mldgr")

  # 4. Start from PBF with auto-center:
  # osrm_gui("berlin.osm.pbf")

  # 5. Explicit center and zoom:
  # osrm_gui(port = 5001, center = c(13.4, 52.5), zoom = 12)

  # 6. Use an existing process (must specify port):
  # srv <- osrm_start("graph.osrm.mldgr", port = 6000)
  # osrm_gui(srv, port = 6000)

  # 7. Enable debug mode:
  # osrm_gui(debug = TRUE)
}
```
