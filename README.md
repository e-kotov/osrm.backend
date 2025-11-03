

<!-- README.md is generated from README.Rmd. Please edit that file -->

# osrm.backend

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/osrm.backend)](https://CRAN.R-project.org/package=osrm.backend)
[![gridmaker status
badge](https://e-kotov.r-universe.dev/gridmaker/badges/version.png)](https://e-kotov.r-universe.dev/osrm.backend)
[![R-CMD-check](https://github.com/e-kotov/osrm.backend/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/e-kotov/osrm.backend/actions/workflows/R-CMD-check.yaml)
[![pkgcheck](https://github.com/e-kotov/gridmaker/workflows/pkgcheck/badge.svg)](https://github.com/e-kotov/osrm.backend/actions?query=workflow%3Apkgcheck)
[![Codecov test
coverage](https://codecov.io/gh/e-kotov/osrm.backend/graph/badge.svg)](https://app.codecov.io/gh/e-kotov/osrm.backend)
<!-- badges: end -->

The goal of `osrm.backend` is to be a companion to `{osrm}` R package
<https://github.com/riatelab/osrm>:

- easily install `osrm.backend` on major operating systems (Linux,
  Windows, MacOS);

- provide wrapper functions to prepare data for `osrm` routing;

- provide wrapper to start/stop local OSRM backend server.

Essentially, you can prepare the OpenStreetMap data for routing with one
function `osrm_start(<path_to_osm_file_or_folder_with_osm_file>)`, set
the `osrm.server` option to `http://localhost:5001/` and use all the
`{osrm}` functions as usual. You do not need to have `osrm-backend`
installed or have `Docker` to run `osrm-backend` from a container,
everything is handled automatically an all major operating systems. Just
follow the [`Get started`](vignettes/osrm-backend.qmd) guide. There are
options for advanced users too who want to control every step of the
process, just see the full [function
reference](./docs/reference/index.html).
