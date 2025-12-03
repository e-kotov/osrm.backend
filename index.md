# osrm.backend

[![DOI](https://zenodo.org/badge/DOI/10.32614/CRAN.package.osrm.backend.svg)](https://doi.org/10.32614/CRAN.package.osrm.backend)

![](https://www.ekotov.pro/osrm.backend/reference/figures/card.png)

The goal of `osrm.backend` is to be a companion to
[osrm](https://github.com/riatelab/osrm) R package
<https://github.com/riatelab/osrm>:

- provide single line quick start with `osmr_start()` function that does
  everything automatically to run local OSRM backend server, no other
  setup is required;

- easily install `osrm.backend` on major operating systems (Linux,
  Windows, MacOS);

- provide wrapper functions to prepare data for `osrm` routing;

- provide wrapper to start/stop local OSRM backend server.

## Installation

Install the latest stable release of `osrm.backend` from CRAN with:

``` r
# soon on CRAN, watch this space!
```

You can install the development version of `osrm.backend` from R
Universe with:

``` r
install.packages('osrm.backend',
 repos = c('https://e-kotov.r-universe.dev', 'https://cloud.r-project.org')
)
```

or from [GitHub repo](https://github.com/e-kotov/osrm.backend) with:

``` r
# install.packages("pak")
pak::pak("e-kotov/osrm.backend")
```

You can prepare the OpenStreetMap data for routing with one function
`osrm_start(<path_to_osm_file_or_folder_with_osm_file>)`, set the
`osrm.server` option to `http://localhost:5001/` and use all the
[osrm](https://github.com/riatelab/osrm) functions as usual. You do not
need to have `osrm-backend` installed or have `Docker` to run
`osrm-backend` from a container, everything is handled automatically an
all major operating systems. Just follow the [Get
started](https://www.ekotov.pro/osrm.backend/articles/osrm-backend.html)
guide. For advanced control over each step of the process, see the full
[function
reference](https://www.ekotov.pro/osrm.backend/reference/index.html).
