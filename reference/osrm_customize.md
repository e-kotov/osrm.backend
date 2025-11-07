# Customize OSRM Graph for Multi-Level Dijkstra (MLD)

Run the `osrm-customize` tool to customize a partitioned OSRM graph for
the MLD pipeline. After running, a companion `<base>.osrm.mldgr` file
must exist to confirm success.

## Usage

``` r
osrm_customize(
  input_osrm,
  threads = 8L,
  verbosity = c("INFO", "NONE", "ERROR", "WARNING", "DEBUG"),
  segment_speed_file = NULL,
  turn_penalty_file = NULL,
  edge_weight_updates_over_factor = 0,
  parse_conditionals_from_now = 0,
  time_zone_file = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
)
```

## Arguments

- input_osrm:

  A string. Base path to the partitioned `.osrm` files (without
  extension).

- threads:

  An integer. Number of threads to use; default `8` (osrm-customize's
  default).

- verbosity:

  A string. Log verbosity level passed to `-l/--verbosity` (one of
  `"NONE","ERROR","WARNING","INFO","DEBUG"`); default `"INFO"`.

- segment_speed_file:

  A string or `NULL`. Path to nodeA,nodeB,speed CSV; default `NULL`.

- turn_penalty_file:

  A string or `NULL`. Path to from\_,to\_,via_nodes,penalties CSV;
  default `NULL`.

- edge_weight_updates_over_factor:

  A numeric. Factor threshold for logging large weight updates; default
  `0`.

- parse_conditionals_from_now:

  A numeric. UTC timestamp from which to evaluate conditional turn
  restrictions; default `0`.

- time_zone_file:

  A string or `NULL`. GeoJSON file with time zone boundaries; default
  `NULL`.

- echo_cmd:

  A logical. Print each command before running; default `FALSE`.

- echo:

  A logical. Stream stdout/stderr; default `TRUE`.

- spinner:

  A logical. Show spinner instead of live logs; default `TRUE`.

## Value

A list with elements:

- osrm_path:

  The normalized path to the input `.osrm` base (invisibly). This can be
  passed over to
  [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md).

- logs:

  The [`processx::run`](http://processx.r-lib.org/reference/run.md)
  result object.
