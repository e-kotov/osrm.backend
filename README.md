

<!-- README.md is generated from README.qmd. Please edit that file -->

# osrm.backend <a href="https://www.ekotov.pro/osrm.backend/"><img src="man/figures/logo.png" align="right" width="200" alt="r5rgui website" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/osrm.backend)](https://CRAN.R-project.org/package=osrm.backend)
<a href="https://CRAN.R-project.org/package=osrm.backend"
target="_blank"><img
src="https://cranlogs.r-pkg.org/badges/grand-total/osrm.backend?color=blue"
alt="CRAN/METACRAN Total downloads" /></a>
<a href="https://CRAN.R-project.org/package=osrm.backend"
target="_blank"><img
src="https://cranlogs.r-pkg.org/badges/osrm.backend?color=blue"
alt="CRAN/METACRAN Downloads per month" /></a> [![osrm.backend status
badge](https://e-kotov.r-universe.dev/osrm.backend/badges/version)](https://e-kotov.r-universe.dev/osrm.backend)
[![OSRM tested
(Linux)](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/e-kotov/osrm.backend/badges/osrm_versions_ubuntu.json)](https://github.com/e-kotov/osrm.backend/actions/workflows/osrm-live-tests.yml)
[![OSRM tested
(macOS)](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/e-kotov/osrm.backend/badges/osrm_versions_macos.json)](https://github.com/e-kotov/osrm.backend/actions/workflows/osrm-live-tests.yml)
[![OSRM tested
(Windows)](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/e-kotov/osrm.backend/badges/osrm_versions_windows.json)](https://github.com/e-kotov/osrm.backend/actions/workflows/osrm-live-tests.yml)
[![R-CMD-check](https://github.com/e-kotov/osrm.backend/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/e-kotov/osrm.backend/actions/workflows/R-CMD-check.yaml)
[![pkgcheck](https://github.com/e-kotov/osrm.backend/workflows/pkgcheck/badge.svg)](https://github.com/e-kotov/osrm.backend/actions?query=workflow%3Apkgcheck)
[![Codecov test
coverage](https://codecov.io/gh/e-kotov/osrm.backend/graph/badge.svg)](https://app.codecov.io/gh/e-kotov/osrm.backend)

[![DOI](https://zenodo.org/badge/DOI/10.32614/CRAN.package.osrm.backend.svg)](https://doi.org/10.32614/CRAN.package.osrm.backend)
<!-- badges: end -->

![](https://www.ekotov.pro/osrm.backend/reference/figures/card.png)

The goal of `osrm.backend` is to be a companion to `{osrm}` R package
<https://github.com/riatelab/osrm>:

- provide single line quick start with `osrm_start()` function that does
  everything automatically to run local OSRM backend server, no other
  setup is required;

- easily install `osrm.backend` on major operating systems (Linux,
  Windows, MacOS);

- provide wrapper functions to prepare data for `osrm` routing;

- provide wrapper to start/stop local OSRM backend server.

## OSRM backend binaries

By default, `osrm.backend` installs OSRM from
[`e-kotov/osrm-binaries`](https://github.com/e-kotov/osrm-binaries).
These are immutable OSRM backend binary releases built from upstream
OSRM source tags primarily for this R package.

The default binary releases are different from the official upstream
OSRM release assets:

- they contain backend command-line executables such as `osrm-extract`,
  `osrm-contract`, `osrm-customize`, `osrm-partition`, and
  `osrm-routed`;
- they do not include upstream `node_osrm` Node.js package artifacts;
- they use predictable archive names for Linux, macOS, and Windows;
- they bundle runtime libraries where practical, so the archives are
  more portable for R users than relying on a local OSRM build
  toolchain;
- they are live-tested by this package on Linux, Linux arm64, macOS, and
  Windows.

The official upstream binaries from
[`Project-OSRM/osrm-backend`](https://github.com/Project-OSRM/osrm-backend)
are still available with:

``` r
osrm_install(osrm_binaries_provider = "official")
```

That provider is kept as a compatibility option. The main supported and
tested installation path is the default `e-kotov/osrm-binaries`
provider. See the [OSRM binary providers
vignette](https://www.ekotov.pro/osrm.backend/articles/binary-providers.html)
for details.

## Installation

Install the latest stable release of `osrm.backend` from CRAN with:

``` r
install.packages("osrm.backend")
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
`{osrm}` functions as usual. You do not need to have `osrm-backend`
installed or have `Docker` to run `osrm-backend` from a container,
everything is handled automatically on all major operating systems. Just
follow the [Get
started](https://www.ekotov.pro/osrm.backend/articles/osrm-backend.html)
guide. For advanced control over each step of the process, see the full
[function
reference](https://www.ekotov.pro/osrm.backend/reference/index.html).

## OSRM Graphical User Interface demo

![osrm GUI demo
1](https://github.com/user-attachments/assets/d7f4c671-10f1-4373-9786-1356a8942a94)

![osrm GUI demo
2](https://github.com/user-attachments/assets/f0bd80e8-632e-47f1-9800-2131d5002204)

![osrm GUI demo
3](https://github.com/user-attachments/assets/7b524d02-4e28-4f22-8c1b-221c2e9c7204)





<!-- AUTO_TESTED_VERSIONS_START -->

<div class='auto-tested-versions'>
<h3>Auto-tested OSRM releases (generated)</h3>
<table>
<thead><tr><th>OS</th><th>Badge</th><th>Validated releases</th><th>Notes</th></tr></thead>
<tbody>
<tr style="background-color:#4c1"><td><strong>LINUX</strong></td><td><img src="https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fe-kotov%2Fosrm.backend%2Fbadges%2Fosrm_versions_ubuntu.json" alt="LINUX badge" /></td><td><a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.3"><code>v26.7.3</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.2"><code>v26.7.2</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.1"><code>v26.7.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.0"><code>v26.7.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.5"><code>v26.6.5</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.4"><code>v26.6.4</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.3"><code>v26.6.3</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.2"><code>v26.6.2</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.1"><code>v26.6.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.0"><code>v26.6.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.5.0"><code>v26.5.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.1"><code>v26.4.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.0"><code>v26.4.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v6.0.0"><code>v6.0.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v5.27.1"><code>v5.27.1</code></a></td><td>v5.27.1 - v26.7.3</td></tr>
<tr style="background-color:#4c1"><td><strong>MACOS</strong></td><td><img src="https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fe-kotov%2Fosrm.backend%2Fbadges%2Fosrm_versions_macos.json" alt="MACOS badge" /></td><td><a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.3"><code>v26.7.3</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.2"><code>v26.7.2</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.1"><code>v26.7.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.0"><code>v26.7.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.5"><code>v26.6.5</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.4"><code>v26.6.4</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.3"><code>v26.6.3</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.2"><code>v26.6.2</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.1"><code>v26.6.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.0"><code>v26.6.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.5.0"><code>v26.5.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.1"><code>v26.4.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.0"><code>v26.4.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v6.0.0"><code>v6.0.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v5.27.1"><code>v5.27.1</code></a></td><td>v5.27.1 - v26.7.3</td></tr>
<tr style="background-color:#4c1"><td><strong>WINDOWS</strong></td><td><img src="https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fe-kotov%2Fosrm.backend%2Fbadges%2Fosrm_versions_windows.json" alt="WINDOWS badge" /></td><td><a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.3"><code>v26.7.3</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.2"><code>v26.7.2</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.1"><code>v26.7.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.0"><code>v26.7.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.5"><code>v26.6.5</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.4"><code>v26.6.4</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.3"><code>v26.6.3</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.2"><code>v26.6.2</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.1"><code>v26.6.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.0"><code>v26.6.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.5.0"><code>v26.5.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.1"><code>v26.4.1</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.0"><code>v26.4.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v6.0.0"><code>v6.0.0</code></a>, <a href="https://github.com/e-kotov/osrm-binaries/releases/tag/v5.27.1"><code>v5.27.1</code></a></td><td>v5.27.1 - v26.7.3</td></tr>
</tbody></table>
<p>Badge source: <code>badges</code> branch — raw JSON files (e.g. <code>osrm_versions_ubuntu.json</code>).</p>
</div>

<!-- AUTO_TESTED_VERSIONS_END -->



