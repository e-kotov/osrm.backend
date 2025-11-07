# osrm.backend

\![[https://www.ekotov.pro/osrm.backend/reference/figures/card.png\\](https://www.ekotov.pro/osrm.backend/reference/figures/card.png%5C)\]

The goal of `osrm.backend` is to be a companion to
[osrm](https://github.com/riatelab/osrm) R package
<https://github.com/riatelab/osrm>:

- easily install `osrm.backend` on major operating systems (Linux,
  Windows, MacOS);

- provide wrapper functions to prepare data for `osrm` routing;

- provide wrapper to start/stop local OSRM backend server.

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
