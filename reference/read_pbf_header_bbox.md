# Read Bounding Box from OSM PBF Header

Extracts the bbox from the HeaderBlock of an OSM PBF file using pure R.
This reads only the first header blob (OSMHeader), not the data blocks.

## Usage

``` r
read_pbf_header_bbox(pbf_file)
```

## Arguments

- pbf_file:

  Path to an .osm.pbf file

## Value

Named list with `center` (c(lng, lat)) and `bbox` (c(lng_min, lat_min,
lng_max, lat_max)) or NULL if bbox not found or file invalid
