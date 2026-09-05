# OSRM binary providers

``` r

knitr::opts_chunk$set(eval = FALSE)
library(osrm.backend)
```

`osrm.backend` can install OSRM backend executables from two GitHub
release repositories:

- `e-kotov/osrm-binaries`, selected by
  `osrm_binaries_provider = "default"`.
- `Project-OSRM/osrm-backend`, selected by
  `osrm_binaries_provider = "official"`.

The default provider is the primary supported path for this R package.
The official upstream provider remains available for users who
specifically need the upstream release artifacts.

## Default provider

The default provider downloads from
[`e-kotov/osrm-binaries`](https://github.com/e-kotov/osrm-binaries):

``` r

install_dir <- osrm_install(osrm_binaries_provider = "default")
```

These releases are built from upstream OSRM source tags, but they are
packaged for the needs of this R package:

- archives contain the OSRM backend command-line executables, not
  upstream `node_osrm` Node.js package artifacts;
- asset names are predictable across Linux, macOS, and Windows;
- release assets are immutable once published;
- SHA-256 checksums are available through GitHub release asset digests,
  with `checksums.txt` kept as a compatibility fallback;
- Lua profiles are bundled with the archive;
- runtime libraries are bundled where practical so users do not need to
  install the full OSRM build toolchain.

The Linux archives are not fully static binaries. They still depend on
core system libraries such as glibc and the platform loader supplied by
the host operating system. For recent releases, the binary repository
documents the effective `GLIBC_*` compatibility floor in its README. For
example, the published `v26.7.3` Linux `x64` and `arm64` archives
require symbols up to `GLIBC_2.38`.

## Official upstream provider

The official provider downloads from
[`Project-OSRM/osrm-backend`](https://github.com/Project-OSRM/osrm-backend):

``` r

install_dir <- osrm_install(osrm_binaries_provider = "official")
```

These are the upstream OSRM project release artifacts. They are the
right choice when you intentionally want to use the official upstream
package layout.

For this R package, they are a secondary compatibility path. The
upstream archives are packaged around the `node_osrm` Node.js
distribution and may differ from the default provider in archive layout,
platform coverage, bundled runtime libraries, and Linux compatibility
with older glibc versions. When possible,
[`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)
applies compatibility workarounds for official releases, but it cannot
make all upstream artifacts portable on every platform.

## Live test coverage

The package has a scheduled live integration workflow that downloads,
installs, and runs OSRM against a small test map on Linux, Linux arm64,
macOS, and Windows.

The default provider is tested as the main supported provider. The live
tests install all available supported versions for each platform, start
`osrm-routed`, prepare a graph, and verify that routing works through
both the OSRM HTTP API and, where available, the
[osrm](https://github.com/riatelab/osrm) R package.

### Auto-tested OSRM releases (generated)

| OS | Badge | Validated releases | Notes |
|----|----|----|----|
| **LINUX** | ![LINUX badge](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fe-kotov%2Fosrm.backend%2Fbadges%2Fosrm_versions_ubuntu.json) | [`v26.7.3`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.3), [`v26.7.2`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.2), [`v26.7.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.1), [`v26.7.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.0), [`v26.6.5`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.5), [`v26.6.4`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.4), [`v26.6.3`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.3), [`v26.6.2`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.2), [`v26.6.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.1), [`v26.6.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.0), [`v26.5.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.5.0), [`v26.4.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.1), [`v26.4.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.0), [`v6.0.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v6.0.0), [`v5.27.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v5.27.1) | v5.27.1 - v26.7.3 |
| **MACOS** | ![MACOS badge](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fe-kotov%2Fosrm.backend%2Fbadges%2Fosrm_versions_macos.json) | [`v26.7.3`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.3), [`v26.7.2`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.2), [`v26.7.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.1), [`v26.7.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.0), [`v26.6.5`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.5), [`v26.6.4`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.4), [`v26.6.3`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.3), [`v26.6.2`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.2), [`v26.6.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.1), [`v26.6.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.0), [`v26.5.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.5.0), [`v26.4.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.1), [`v26.4.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.0), [`v6.0.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v6.0.0), [`v5.27.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v5.27.1) | v5.27.1 - v26.7.3 |
| **WINDOWS** | ![WINDOWS badge](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fe-kotov%2Fosrm.backend%2Fbadges%2Fosrm_versions_windows.json) | [`v26.7.3`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.3), [`v26.7.2`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.2), [`v26.7.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.1), [`v26.7.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.7.0), [`v26.6.5`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.5), [`v26.6.4`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.4), [`v26.6.3`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.3), [`v26.6.2`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.2), [`v26.6.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.1), [`v26.6.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.6.0), [`v26.5.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.5.0), [`v26.4.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.1), [`v26.4.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v26.4.0), [`v6.0.0`](https://github.com/e-kotov/osrm-binaries/releases/tag/v6.0.0), [`v5.27.1`](https://github.com/e-kotov/osrm-binaries/releases/tag/v5.27.1) | v5.27.1 - v26.7.3 |

Badge source: `badges` branch — raw JSON files
(e.g. `osrm_versions_ubuntu.json`).

The official provider is also exercised through a cross-provider
compatibility test where a version is available from both repositories
and the official artifact can prepare the test graph. This is
intentionally conditional: official upstream artifacts do not behave as
a fully portable all-platform provider for this package. If an upstream
artifact cannot be installed or cannot prepare the graph on a platform,
the compatibility comparison is skipped for that version rather than
treated as a failure of the default provider.

In practice, this means:

- default provider success is the main signal that `osrm.backend` can
  install and run OSRM on a platform;
- official provider compatibility is best-effort and platform-dependent;
- a green live-test workflow does not mean the official upstream
  provider works for every OSRM version on every platform.

## Which provider should I use?

Most users should use the default provider:

``` r

osrm_install()
```

Use the official provider only when you have a specific reason to test
or depend on upstream OSRM’s official release artifacts:

``` r

osrm_install(osrm_binaries_provider = "official")
```

For details about how the default binaries are built and published, see
the [`e-kotov/osrm-binaries`](https://github.com/e-kotov/osrm-binaries)
repository.
