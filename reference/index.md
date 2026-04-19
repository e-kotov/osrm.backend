# Package index

## Quick Start

Quickly start/stop/list servers and launch the GUI to inspect routes

- [`osrm_start()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start.md)
  **\[stable\]** : Start an OSRM Server with Automatic Setup
- [`osrm_gui()`](https://www.ekotov.pro/osrm.backend/reference/osrm_gui.md)
  **\[experimental\]** : Launch a GUI to View and Debug OSRM Routing
- [`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md)
  **\[stable\]** : List OSRM servers
- [`osrm_stop()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop.md)
  **\[stable\]** : Stop an OSRM Server

## Install and update OSRM binaries

Discover, install, or remove OSRM backend releases and keep your PATH
tidy.

- [`osrm_install()`](https://www.ekotov.pro/osrm.backend/reference/osrm_install.md)
  **\[stable\]** : Install OSRM Backend Binaries

- [`osrm_uninstall()`](https://www.ekotov.pro/osrm.backend/reference/osrm_uninstall.md)
  **\[stable\]** : Uninstall OSRM Backend Binaries

- [`osrm_which()`](https://www.ekotov.pro/osrm.backend/reference/osrm_which.md)
  **\[stable\]** :

  Locate the OSRM Installation Used by `osrm.backend`

- [`osrm_clear_path()`](https://www.ekotov.pro/osrm.backend/reference/osrm_clear_path.md)
  : Clear OSRM Path from Project's .Rprofile

- [`osrm_check_latest_version()`](https://www.ekotov.pro/osrm.backend/reference/osrm_check_latest_version.md)
  **\[stable\]** : Check for the Latest Stable OSRM Version

- [`osrm_check_available_versions()`](https://www.ekotov.pro/osrm.backend/reference/osrm_check_available_versions.md)
  **\[stable\]** : Check for Available OSRM Versions

## Build routing graphs

Prepare OSRM graphs from raw OSM extracts or run specific pipeline
stages when you need finer control.

- [`osrm_prepare_graph()`](https://www.ekotov.pro/osrm.backend/reference/osrm_prepare_graph.md)
  **\[stable\]** : Prepare OSRM Graph for Routing (Extract +
  Partition/Contract)
- [`osrm_extract()`](https://www.ekotov.pro/osrm.backend/reference/osrm_extract.md)
  **\[stable\]** : Extract OSM into OSRM Graph Files
- [`osrm_partition()`](https://www.ekotov.pro/osrm.backend/reference/osrm_partition.md)
  **\[stable\]** : Partition OSRM Graph for Multi-Level Dijkstra (MLD)
- [`osrm_customize()`](https://www.ekotov.pro/osrm.backend/reference/osrm_customize.md)
  **\[stable\]** : Customize OSRM Graph for Multi-Level Dijkstra (MLD)
- [`osrm_contract()`](https://www.ekotov.pro/osrm.backend/reference/osrm_contract.md)
  **\[stable\]** : Contract OSRM Graph for Contraction Hierarchies (CH)
- [`osrm_cleanup()`](https://www.ekotov.pro/osrm.backend/reference/osrm_cleanup.md)
  **\[stable\]** : Clean Up OSRM Files in a Directory
- [`osrm_find_profile()`](https://www.ekotov.pro/osrm.backend/reference/osrm_find_profile.md)
  **\[stable\]** : Locate an OSRM Lua profile (e.g. car.lua) in a host
  installation

## Run and monitor servers

Launch `osrm-routed` manually, inspect registered processes, and stop
them cleanly.

- [`osrm_start_server()`](https://www.ekotov.pro/osrm.backend/reference/osrm_start_server.md)
  **\[stable\]** :

  Start an OSRM MLD/CH server with `osrm-routed`

- [`osrm_servers()`](https://www.ekotov.pro/osrm.backend/reference/osrm_servers.md)
  **\[stable\]** : List OSRM servers

- [`osrm_get_server_profile()`](https://www.ekotov.pro/osrm.backend/reference/osrm_get_server_profile.md)
  **\[stable\]** : Retrieve the OSRM Profile for a Running Server

- [`osrm_stop()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop.md)
  **\[stable\]** : Stop an OSRM Server

- [`osrm_stop_all()`](https://www.ekotov.pro/osrm.backend/reference/osrm_stop_all.md)
  **\[stable\]** : Stop all running OSRM servers started via this
  package
