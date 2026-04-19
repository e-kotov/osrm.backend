# Retrieve the OSRM Profile for a Running Server

**\[stable\]**

Attempts to determine the profile (e.g., "car", "bike", "foot") used by
an OSRM server. It follows a priority list:

1.  Checks the active server registry for the given port or ID.

2.  Checks for a `dataset.meta.json` file in the directory of the graph
    file.

3.  Checks the graph filename for hints (e.g. `berlin-car.osrm`).

4.  Falls back to `getOption("osrm.profile")`.

## Usage

``` r
osrm_get_server_profile(input_osrm = NULL, port = NULL)
```

## Arguments

- input_osrm:

  Optional. Can be an OSRM job process (an `osrm_server` object
  inheriting from
  [`processx::process`](http://processx.r-lib.org/reference/process.md)),
  a path string, or NULL.

- port:

  Optional integer. The port of the server.

## Value

A character string representing the profile name (default "car").
