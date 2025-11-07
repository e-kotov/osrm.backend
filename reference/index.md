# Package index

## Quick Start

Automate installation, graph preparation, and server launch.

- [`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md)
  : Start an OSRM Server with Automatic Setup
- [`osrm_stop()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop.md)
  : Stop an OSRM Server

## Install and update OSRM binaries

Discover, install, or remove OSRM backend releases and keep your PATH
tidy.

- [`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)
  : Install OSRM Backend Binaries

- [`osrm_uninstall()`](https://www.ekotov.pro/osrm.backend/reference/osrm_uninstall.md)
  : Uninstall OSRM Backend Binaries

- [`osrm_which()`](https://www.ekotov.pro/osrm.backend/reference/osrm_which.md)
  :

  Locate the OSRM Installation Used by `osrm.backend`

- [`osrm_clear_path()`](https://www.ekotov.pro/osrm.backend/reference/osrm_clear_path.md)
  : Clear OSRM Path from Project's .Rprofile

- [`osrm_check_latest_version()`](https://www.ekotov.pro/osrm.backend/reference/osrm_check_latest_version.md)
  : Check for the Latest Stable OSRM Version

- [`osrm_check_available_versions()`](https://www.ekotov.pro/osrm.backend/reference/osrm_check_available_versions.md)
  : Check for Available OSRM Versions

## Build routing graphs

Prepare OSRM graphs from raw OSM extracts or run specific pipeline
stages when you need finer control.

- [`osrm_prepare_graph()`](https://www.ekotov.pro/osrm.backend/reference/osrm_prepare_graph.md)
  : Prepare OSRM Graph for Routing (Extract + Partition/Contract)
- [`osrm_extract()`](https://www.ekotov.pro/osrm.backend/reference/osrm_extract.md)
  : Extract OSM into OSRM Graph Files
- [`osrm_partition()`](https://www.ekotov.pro/osrm.backend/reference/osrm_partition.md)
  : Partition OSRM Graph for Multi-Level Dijkstra (MLD)
- [`osrm_customize()`](https://www.ekotov.pro/osrm.backend/reference/osrm_customize.md)
  : Customize OSRM Graph for Multi-Level Dijkstra (MLD)
- [`osrm_contract()`](https://www.ekotov.pro/osrm.backend/reference/osrm_contract.md)
  : Contract OSRM Graph for Contraction Hierarchies (CH)
- [`osrm_find_profile()`](https://www.ekotov.pro/osrm.backend/reference/osrm_find_profile.md)
  : Locate an OSRM Lua profile (e.g. car.lua) in a host installation

## Run and monitor servers

Launch `osrm-routed` manually, inspect registered processes, and stop
them cleanly.

- [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md)
  :

  Start an OSRM MLD/CH server with `osrm-routed`

- [`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md)
  : List OSRM servers started via this package

- [`osrm_stop()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop.md)
  : Stop an OSRM Server

- [`osrm_stop_all()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop_all.md)
  : Stop all running OSRM servers started via this package
