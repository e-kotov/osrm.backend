# Get PBF Bounding Box with Fallbacks

Tries multiple methods to extract the bounding box from an OSM PBF file:

1.  Pure R header parsing (fastest, no dependencies)

2.  osmium CLI (if available in PATH)

3.  sf sampling (if sf is installed, reads first 10 features)

## Usage

``` r
.get_pbf_extent(pbf_file)
```

## Arguments

- pbf_file:

  Path to an .osm.pbf file

## Value

Named list with `center` (c(lng, lat)) and `bbox` (c(lng_min, lat_min,
lng_max, lat_max)) or NULL if bbox could not be determined
