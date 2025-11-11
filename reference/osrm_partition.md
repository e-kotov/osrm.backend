# Partition OSRM Graph for Multi-Level Dijkstra (MLD)

Run the `osrm-partition` tool to partition an OSRM graph for the MLD
pipeline. After running, a companion `<base>.osrm.partition` file must
exist to confirm success.

## Usage

``` r
osrm_partition(
  input_osrm,
  threads = 8L,
  verbosity = c("INFO", "NONE", "ERROR", "WARNING", "DEBUG"),
  balance = 1.2,
  boundary = 0.25,
  optimizing_cuts = 10L,
  small_component_size = 1000L,
  max_cell_sizes = c(128, 4096, 65536, 2097152),
  quiet = FALSE,
  verbose = FALSE,
  spinner = TRUE,
  echo_cmd = FALSE
)
```

## Arguments

- input_osrm:

  A string. Base path to the `.osrm` files (without extension).

- threads:

  An integer. Number of threads to use; default `8` (osrm-partition's
  default).

- verbosity:

  A string. Log verbosity level passed to `-l/--verbosity` (one of
  `"NONE","ERROR","WARNING","INFO","DEBUG"`); default `"INFO"`.

- balance:

  A numeric. Balance for left and right side in single bisection;
  default `1.2`.

- boundary:

  A numeric. Percentage of embedded nodes to contract as sources and
  sinks; default `0.25`.

- optimizing_cuts:

  An integer. Number of cuts to use for optimizing a single bisection;
  default `10`.

- small_component_size:

  An integer. Size threshold for small components; default `1000`.

- max_cell_sizes:

  A numeric vector. Maximum cell sizes starting from level 1; default
  `c(128,4096,65536,2097152)`.

- quiet:

  A logical. Master switch that suppresses package messages and process
  output when `TRUE`; default `FALSE`.

- verbose:

  A logical. When `TRUE` and `quiet = FALSE`, streams stdout and stderr
  from the underlying
  [`processx::run`](http://processx.r-lib.org/reference/run.md) calls.

- spinner:

  A logical. When `TRUE` and `quiet = FALSE`, shows a spinner instead of
  live logs; default `TRUE`.

- echo_cmd:

  A logical. When `TRUE` and `quiet = FALSE`, prints each command before
  running; default `FALSE`.

## Value

A list with elements:

- osrm_path:

  The normalized path to the input `.osrm` base (invisibly).

- logs:

  The [`processx::run`](http://processx.r-lib.org/reference/run.md)
  result object.
