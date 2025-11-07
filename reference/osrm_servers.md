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
`started_at`, `alive`, `has_handle`.
