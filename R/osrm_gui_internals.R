# Internal helpers for osrm_gui
# These functions are not exported

#' Check for required GUI dependencies
#' @noRd
gui_check_dependencies <- function() {
  required_pkgs <- c("shiny", "mapgl", "osrm", "sf", "DT", "viridisLite")
  missing_pkgs <- required_pkgs[
    !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required for the GUI but are not installed: ",
      paste(missing_pkgs, collapse = ", "),
      ".
",
      "Please install them using install.packages(c(",
      paste(sprintf("'%s'", missing_pkgs), collapse = ", "),
      "))",
      call. = FALSE
    )
  }
}

#' Manage OSRM Server Lifecycle for GUI
#' @noRd
gui_setup_server <- function(input_osrm, port) {
  server_process <- NULL
  kill_on_exit <- FALSE
  host <- "http://127.0.0.1"
  active_port <- port

  if (inherits(input_osrm, "process")) {
    if (!input_osrm$is_alive()) {
      stop("The provided OSRM server process is not running.", call. = FALSE)
    }
    if (identical(port, "auto")) {
      stop(
        "When providing a process object, you must also specify the port explicitly.
",
        "Example: osrm_gui(my_process, port = 5001)",
        call. = FALSE
      )
    }
    active_port <- as.integer(port)
    message("Using provided OSRM server process on port ", active_port)
  } else if (is.character(input_osrm)) {
    message("Starting temporary OSRM server for ", basename(input_osrm), "...")
    server_port <- if (identical(port, "auto")) 5001L else as.integer(port)
    server_process <- osrm_start(
      path = input_osrm,
      port = server_port,
      quiet = FALSE
    )
    if (!server_process$is_alive()) {
      stop("Failed to start OSRM server. Check logs.", call. = FALSE)
    }
    active_port <- server_port
    kill_on_exit <- TRUE
  } else if (identical(port, "auto")) {
    servers <- osrm_servers()
    alive_servers <- servers[servers$alive, ]

    if (nrow(alive_servers) == 0) {
      stop(
        "No running OSRM servers detected. ",
        "Start a server with osrm_start() or specify an explicit port.",
        call. = FALSE
      )
    } else if (nrow(alive_servers) == 1) {
      active_port <- alive_servers$port[1]
      message("Connected to OSRM server on port ", active_port)
    } else {
      most_recent <- alive_servers[which.max(alive_servers$started_at), ]
      active_port <- most_recent$port

      warning(
        "Multiple OSRM servers running (ports: ",
        paste(alive_servers$port, collapse = ", "),
        "). Using most recent (port ",
        active_port,
        ", started at ",
        format(most_recent$started_at),
        "). ",
        "Specify 'port' explicitly to select a different server.",
        call. = FALSE
      )
    }
  } else {
    active_port <- as.integer(port)
    message(
      "Attempting to connect to existing OSRM server at ",
      host,
      ":",
      active_port
    )
  }

  cleanup_fn <- function() {
    if (kill_on_exit && !is.null(server_process) && server_process$is_alive()) {
      message("Stopping temporary OSRM server...")
      server_process$kill()
    }
  }

  list(
    active_port = active_port,
    host = host,
    server_process = server_process,
    cleanup_fn = cleanup_fn
  )
}

#' Resolve Map Center and Zoom
#' @noRd
gui_resolve_map_view <- function(center, zoom, input_osrm) {
  auto_center <- NULL
  auto_zoom <- NULL

  if (is.null(center)) {
    pbf_path <- NULL
    if (
      is.character(input_osrm) &&
        grepl("\\.osm\\.pbf$", input_osrm, ignore.case = TRUE)
    ) {
      pbf_path <- input_osrm
    } else if (is.null(input_osrm) || inherits(input_osrm, "process")) {
      srv_info <- osrm_servers()
      alive_srv <- srv_info[srv_info$alive, ]
      if (nrow(alive_srv) > 0) {
        selected <- alive_srv[which.max(alive_srv$started_at), ]
        if (nzchar(selected$input_osm)) pbf_path <- selected$input_osm
      }
    }

    if (!is.null(pbf_path) && file.exists(pbf_path)) {
      pbf_info <- .get_pbf_extent(pbf_path)
      if (!is.null(pbf_info)) {
        auto_center <- pbf_info$center
        auto_zoom <- if (is.null(zoom)) 9 else zoom
        message(
          "Auto-centered map on PBF extent: ",
          paste(round(auto_center, 4), collapse = ", ")
        )
      }
    }
  }

  # Normalize
  final_center <- center %||% auto_center
  final_zoom <- zoom %||% auto_zoom

  if (!is.null(final_center)) {
    if (is.list(final_center)) {
      final_center <- c(
        final_center$lng %||% final_center$lon %||% final_center$x,
        final_center$lat %||% final_center$y
      )
    }
    final_center <- as.numeric(final_center)
    if (length(final_center) != 2 || any(is.na(final_center))) {
      stop(
        "'center' must be a numeric vector of length 2 (lng, lat) or a named list.",
        call. = FALSE
      )
    }
  }

  list(center = final_center, zoom = final_zoom)
}

#' Robustly Parse Isochrone Breaks from Text Input
#' @noRd
gui_parse_breaks <- function(input_str, default = c(5, 10, 15)) {
  if (is.null(input_str) || !nzchar(trimws(input_str))) {
    return(default)
  }
  
  # Split, trim, and convert
  parts <- unlist(strsplit(input_str, ","))
  nums <- suppressWarnings(as.numeric(trimws(parts)))
  
  # Clean up
  nums <- nums[!is.na(nums) & nums > 0]
  
  if (length(nums) == 0) {
    return(default)
  }
  
  sort(unique(nums))
}

#' UI Resources (CSS/JS)
#' @noRd
gui_ui_resources <- function() {
  css <- "
      html, body { height: 100%; margin: 0; overflow: hidden; }
      .container-fluid { height: 100%; display: flex; flex-direction: column; }
      #shiny-notification-panel { top: 70px; right: 10px; left: auto; bottom: auto; }
      
      .sidebar-layout { flex: 1; display: flex; overflow: hidden; min-height: 0; }
      .sidebar-panel { 
        height: 100%; 
        overflow-y: auto; 
        padding: 15px;
        background-color: #f8f9fa;
        border-right: 1px solid #dee2e6;
      }
      .main-panel { 
        height: 100%; 
        display: flex; 
        flex-direction: column; 
        padding: 15px; 
        overflow: hidden;
      }
      
      .map-wrapper { position: relative; flex: 3; min-height: 300px; }
      #map { height: 100% !important; }
      
      .table-wrapper { 
        flex: 2; 
        overflow-y: auto; 
        margin-top: 10px; 
        border-top: 1px solid #eee;
        padding-top: 10px;
      }
      
      .route-stats-overlay {
        position: absolute;
        top: 10px;
        left: 10px;
        z-index: 1000;
        background: rgba(245, 245, 245, 0.9);
        padding: 8px 12px;
        border-radius: 4px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.15);
        pointer-events: none;
        display: flex;
        gap: 15px;
        font-size: 14px;
        border: 1px solid #ccc;
      }
      .stat-val { font-weight: bold; }

      .trip-marker-label {
        background-color: #984ea3;
        color: white;
        border: 2px solid white;
        border-radius: 50%;
        width: 24px;
        height: 24px;
        display: flex;
        justify-content: center;
        align-items: center;
        font-weight: bold;
        font-size: 12px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.3);
        cursor: pointer;
      }

      .exec-time-overlay {
        position: absolute;
        top: 10px;
        right: 50px; /* Offset to avoid overlap with standard zoom/fullscreen controls */
        z-index: 1000;
        background: rgba(255, 255, 255, 0.8);
        padding: 2px 6px;
        border-radius: 4px;
        font-size: 11px;
        color: #777;
        pointer-events: none;
        border: 1px solid rgba(0,0,0,0.1);
      }
      
      .segments-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 5px;
      }
      
      /* Mobile/Responsive Styles */
      .hamburger-btn { display: none; margin-right: 10px; font-size: 20px; cursor: pointer; }
      
      /* Remove slider fill for discrete look */
      .irs-bar, .irs-bar-edge {
        background: none !important;
        border: none !important;
      }
      .irs-single {
        background: #555 !important;
      }
      
      @media (max-width: 768px) {
        .sidebar-panel {
          position: absolute;
          left: -100%;
          top: 50px; /* Below header */
          bottom: 0;
          width: 80% !important;
          z-index: 2000;
          transition: left 0.3s ease;
          box-shadow: 2px 0 5px rgba(0,0,0,0.2);
        }
        .sidebar-panel.show-sidebar {
          left: 0;
        }
        .main-panel {
          width: 100% !important;
        }
        .hamburger-btn {
          display: inline-block;
        }
        /* Overlay to close sidebar when clicking outside */
        .sidebar-overlay {
          display: none;
          position: fixed;
          top: 0; left: 0; right: 0; bottom: 0;
          background: rgba(0,0,0,0.5);
          z-index: 1999;
        }
        .sidebar-overlay.show-overlay {
          display: block;
        }
        .route-stats-overlay {
          flex-direction: column;
          gap: 2px;
        }
      }
    "

  js <- "
    $(document).on('click', '#hamburger_btn', function() {
      $('.sidebar-panel').toggleClass('show-sidebar');
      $('.sidebar-overlay').toggleClass('show-overlay');
    });
    
    $(document).on('click', '.sidebar-overlay', function() {
      $('.sidebar-panel').removeClass('show-sidebar');
      $('.sidebar-overlay').removeClass('show-overlay');
    });

    function initializeMapListeners(mapId) {
      const mapElement = document.getElementById(mapId);
      if (!mapElement) return;

      const observer = new MutationObserver((mutations, obs) => {
        const map = mapElement.map;
        if (map) {
          // Disable default context menu to allow right-click
          map.getCanvas().addEventListener('contextmenu', (e) => e.preventDefault());

          map.on('contextmenu', (e) => {
            Shiny.setInputValue('js_right_click', {
              lng: e.lngLat.lng,
              lat: e.lngLat.lat,
              nonce: Math.random()
            });
          });

          let startMarker = null;
          let endMarker = null;
          let isoStartMarker = null;
          const tripMarkers = {};

          Shiny.addCustomMessageHandler('updateMarker', function(message) {
            const lngLat = [message.lng, message.lat];
            const markerId = message.id;

            const createDragEndCallback = (id) => {
              return (marker) => {
                const coords = marker.getLngLat();
                Shiny.setInputValue('marker_dragged', {
                  id: id,
                  lng: coords.lng,
                  lat: coords.lat,
                  nonce: Math.random()
                });
              };
            };
            
            const createDragCallback = (id) => {
              return (marker) => {
                const coords = marker.getLngLat();
                Shiny.setInputValue('marker_moving', {
                  id: id,
                  lng: coords.lng,
                  lat: coords.lat,
                  nonce: Math.random()
                });
              };
            };

            if (markerId === 'start') {
              if (!startMarker) {
                startMarker = new maplibregl.Marker({ draggable: true, color: '#009E73' })
                  .setLngLat(lngLat)
                  .addTo(map);
                startMarker.on('dragend', () => createDragEndCallback('start')(startMarker));
                startMarker.on('drag', () => createDragCallback('start')(startMarker));
              } else {
                startMarker.setLngLat(lngLat);
              }
            } else if (markerId === 'end') {
              if (!endMarker) {
                endMarker = new maplibregl.Marker({ draggable: true, color: '#D55E00' })
                  .setLngLat(lngLat)
                  .addTo(map);
                endMarker.on('dragend', () => createDragEndCallback('end')(endMarker));
                endMarker.on('drag', () => createDragCallback('end')(endMarker));
              } else {
                endMarker.setLngLat(lngLat);
              }
            } else if (markerId === 'iso_start') {
              if (!isoStartMarker) {
                isoStartMarker = new maplibregl.Marker({ draggable: true, color: '#CC79A7' })
                  .setLngLat(lngLat)
                  .addTo(map);
                isoStartMarker.on('dragend', () => createDragEndCallback('iso_start')(isoStartMarker));
                isoStartMarker.on('drag', () => createDragCallback('iso_start')(isoStartMarker));
              } else {
                isoStartMarker.setLngLat(lngLat);
              }
            }
          });
          
          Shiny.addCustomMessageHandler('updateTripMarker', function(message) {
            const id = message.id;
            
            if (message.action === 'add') {
               const lngLat = [message.lng, message.lat];
               
               const el = document.createElement('div');
               el.className = 'trip-marker-label';
               el.innerText = '?'; // Default until optimized
               
               const marker = new maplibregl.Marker({ draggable: true, element: el })
                 .setLngLat(lngLat)
                 .addTo(map);
                 
               marker.on('dragend', () => {
                 const coords = marker.getLngLat();
                 Shiny.setInputValue('move_trip_point', {
                   id: id,
                   lng: coords.lng,
                   lat: coords.lat,
                   nonce: Math.random()
                 });
               });
               
               marker.on('drag', () => {
                 const coords = marker.getLngLat();
                 Shiny.setInputValue('marker_moving', {
                   id: id,
                   lng: coords.lng,
                   lat: coords.lat,
                   nonce: Math.random()
                 });
               });
               
               // Add click listener to element for removal
               el.addEventListener('click', (e) => {
                 e.stopPropagation(); // Prevent map click
                 Shiny.setInputValue('remove_trip_point', {id: id, nonce: Math.random()});
               });
               
               tripMarkers[id] = marker;
               
            } else if (message.action === 'remove') {
               if (tripMarkers[id]) {
                 tripMarkers[id].remove();
                 delete tripMarkers[id];
               }
            } else if (message.action === 'clear') {
               for (const id in tripMarkers) {
                 tripMarkers[id].remove();
               }
               // Clear object
               for (const key in tripMarkers) delete tripMarkers[key];
            }
          });

          Shiny.addCustomMessageHandler('updateTripLabels', function(message) {
            // message is an array of {id: ..., label: ...}
            message.forEach(item => {
              const marker = tripMarkers[item.id];
              if (marker) {
                const el = marker.getElement();
                if (el) el.innerText = item.label;
              }
            });
          });

          Shiny.addCustomMessageHandler('clearAllMarkers', function(message) {
              if(startMarker) {
                  startMarker.remove();
                  startMarker = null;
              }
              if(endMarker) {
                  endMarker.remove();
                  endMarker = null;
              }
              if(isoStartMarker) {
                  isoStartMarker.remove();
                  isoStartMarker = null;
              }
              // Also clear trip markers
              for (const id in tripMarkers) {
                 tripMarkers[id].remove();
              }
              for (const key in tripMarkers) delete tripMarkers[key];
          });

          obs.disconnect();
        }
      });

      observer.observe(mapElement, { childList: true, subtree: true });
    }

    $(document).on('shiny:connected', () => {
      initializeMapListeners('map');
      
      // Customize the Isochrone slider labels to show real values
      // We wait briefly to ensure the slider is initialized
      setTimeout(function() {
        [\"#iso_res\", \"#iso_live_res\"].forEach(function(id) {
          const $el = $(id);
          const slider = $el.data(\"ionRangeSlider\");
          if (slider) {
            const valMap = {
              1: '100', 2: '200', 3: '500', 4: '1k',
              5: '2k', 6: '5k', 7: '10k', 8: '20k', 9: '50k'
            };
            slider.update({
              prettify: function(n) {
                return valMap[Math.round(n)] || n;
              }
            });
          }
        });
      }, 500);
    });
  "
  list(css = css, js = js)
}

#' Construct Main UI
#' @noRd
gui_ui_layout <- function() {
  res <- gui_ui_resources()

  shiny::fluidPage(
    # Header
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0; flex-wrap: wrap; gap: 10px;",
      shiny::div(
        style = "display: flex; align-items: center;",
        shiny::HTML(
          '<div id="hamburger_btn" class="hamburger-btn">&#9776;</div>'
        ),
        shiny::h3(
          shiny::HTML("<b>osrm.backend</b> GUI"),
          style = "margin: 0;"
        )
      ),
      shiny::div(
        style = "display: flex; gap: 10px; align-items: center;",
        shiny::uiOutput("mode_button_ui", inline = TRUE),
        shiny::uiOutput("autozoom_button_ui", inline = TRUE),
        shiny::uiOutput("tracking_button_ui", inline = TRUE),
        shiny::actionButton(
          "quit_app",
          "Quit",
          style = "background-color: #d9534f; color: white; border-width: 0px;"
        )
      )
    ),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(res$css)),
      shiny::tags$script(shiny::HTML(res$js))
    ),

    shiny::div(
      class = "sidebar-overlay"
    ),

    shiny::div(
      class = "sidebar-layout",
      shiny::div(
        class = "sidebar-panel",
        style = "width: 25%;",
        shiny::h4("OSRM Controls"),
        shiny::selectInput(
          "mode",
          "Analysis Mode:",
          choices = c("Route" = "route", "Isochrone" = "iso", "Trip" = "trip")
        ),

        shiny::conditionalPanel(
          condition = "input.mode == 'iso'",
          shiny::textInput(
            "iso_breaks",
            "Time Breaks (min, comma sep):",
            value = "5, 10, 15"
          ),
          shiny::sliderInput(
            "iso_res",
            "Resolution (Detail):",
            min = 1,
            max = 9,
            value = 3,
            step = 1,
            ticks = TRUE
          ),
          shiny::sliderInput(
            "iso_live_res",
            "Live Resolution (Drag):",
            min = 1,
            max = 9,
            value = 2,
            step = 1,
            ticks = TRUE
          )
        ),

        shiny::hr(),
        shiny::h4("Locations"),
        shiny::helpText(
          shiny::conditionalPanel(
            "input.mode != 'trip'",
            "Left-click map: Start point",
            shiny::tags$br(),
            "Right-click map: End point (Route mode)",
            shiny::tags$br(),
            "Drag markers to adjust."
          ),
          shiny::conditionalPanel(
            "input.mode == 'trip'",
            "Left-click map: Add waypoints",
            shiny::tags$br(),
            "Click marker: Remove waypoint",
            shiny::tags$br(),
            "Drag markers to adjust."
          )
        ),
        shiny::actionButton(
          "reset",
          "Reset / Clear",
          style = "width: 100%; margin-bottom: 10px;"
        ),

        shiny::textInput(
          "start_coords_input",
          "Start (Lat, Lon)",
          placeholder = "-30.03, -51.22"
        ),
        shiny::textInput(
          "end_coords_input",
          "End (Lat, Lon)",
          placeholder = "-30.05, -51.18"
        )
      ),

      shiny::div(
        class = "main-panel",
        style = "width: 75%;",
        shiny::div(
          class = "map-wrapper",
          shiny::conditionalPanel(
            condition = "input.mode == 'route' || input.mode == 'trip' || input.mode == 'iso'",
            shiny::uiOutput("route_stats")
          ),
          shiny::uiOutput("map_edit_controls"),
          shiny::uiOutput("exec_time_overlay"),
          mapgl::maplibreOutput("map")
        ),
        shiny::div(
          class = "table-wrapper",
          shiny::conditionalPanel(
            condition = "input.mode == 'route'",
            shiny::div(
              class = "segments-header",
              shiny::h4("Route Segments", style = "margin: 0;"),
              shiny::actionButton(
                "clear_selection",
                "Deselect All",
                class = "btn-xs btn-default",
                style = "font-size: 11px; padding: 2px 8px;"
              )
            ),
            DT::dataTableOutput("itinerary_table")
          ),
          shiny::conditionalPanel(
            condition = "input.mode == 'trip'",
            shiny::h4("Trip Sequence"),
            DT::dataTableOutput("trip_table")
          )
        )
      )
    )
  )
}

#' Fetch Detailed Route Steps
#' @noRd
api_fetch_route_detailed <- function(
  src,
  dst,
  overview = "false",
  debug = FALSE
) {
  server_url <- getOption("osrm.server")
  profile <- getOption("osrm.profile")

  # Use geometries=geojson to get coordinate arrays directly
  url <- sprintf(
    "%sroute/v1/%s/%f,%f;%f,%f?steps=true&overview=%s&geometries=geojson",
    server_url,
    profile,
    src$lng,
    src$lat,
    dst$lng,
    dst$lat,
    overview
  )

  if (debug) {
    message("DEBUG [Detailed Route]: Fetching URL: ", url)
  }

  tryCatch(
    {
      req <- httr2::request(url)
      resp <- httr2::req_perform(req)
      httr2::resp_body_json(resp)
    },
    error = function(e) NULL
  )
}

#' Fetch Trip via Direct HTTP Request
#' @description Direct HTTP request to OSRM trip endpoint, bypassing osrm::osrmTrip.
#' This is a workaround for issues where osrmTrip fails inside Shiny reactive contexts.
#' @param pts_df data.frame with lon and lat columns
#' @param debug Logical, print debug messages
#' @return List with trip (sf LINESTRING) and summary, or NULL on error
#' @noRd
api_fetch_trip <- function(pts_df, debug = FALSE) {
  server_url <- getOption("osrm.server")
  profile <- getOption("osrm.profile")

  # Build coordinate string: "lon1,lat1;lon2,lat2;..."
  coords_str <- paste(
    sprintf("%.6f,%.6f", pts_df$lon, pts_df$lat),
    collapse = ";"
  )

  url <- sprintf(
    "%strip/v1/%s/%s?steps=false&geometries=geojson&overview=full&generate_hints=false",
    server_url,
    profile,
    coords_str
  )

  if (debug) {
    message("DEBUG [Trip API]: Fetching URL: ", url)
  }

  tryCatch(
    {
      req <- httr2::request(url)
      resp <- httr2::req_perform(req)
      res <- httr2::resp_body_json(resp)

      if (is.null(res$trips) || length(res$trips) == 0) {
        if (debug) {
          message("DEBUG [Trip API]: No trips in response")
        }
        return(NULL)
      }

      # Parse the first trip
      trip_data <- res$trips[[1]]

      # Extract waypoint order (which input index maps to which trip position)
      # res$waypoints is in the order of input coordinates.
      # res$waypoints[[i]]$waypoint_index is the position in the optimized trip.
      waypoint_order <- vapply(res$waypoints, function(w) as.integer(w$waypoint_index) + 1L, integer(1))

      # Extract overall geometry (full route as one line)
      coords <- trip_data$geometry$coordinates
      if (length(coords) < 2) {
        if (debug) {
          message("DEBUG [Trip API]: Not enough coordinates for LINESTRING")
        }
        return(NULL)
      }

      coord_matrix <- do.call(
        rbind,
        lapply(coords, as.numeric)
      )

      # Validate matrix dimensions and contents to prevent segfaults in st_linestring
      if (
        is.null(coord_matrix) ||
          !is.matrix(coord_matrix) ||
          nrow(coord_matrix) < 2 ||
          ncol(coord_matrix) != 2 ||
          any(is.na(coord_matrix))
      ) {
        if (debug) {
          message("DEBUG [Trip API]: Invalid coordinate matrix")
        }
        return(NULL)
      }

      # Build SINGLE-ROW sf LINESTRING for mapping (most stable)
      trip_line <- sf::st_linestring(coord_matrix)
      trip_sfc <- sf::st_sfc(trip_line, crs = 4326)
      trip_sf_map <- sf::st_sf(geometry = trip_sfc)

      # 2. Extract Legs for Table Display (Pure data, no geometry)
      legs <- trip_data$legs
      n_legs <- length(legs)

      if (n_legs > 0) {
        legs_df <- data.frame(
          Start = seq_len(n_legs),
          End = c(seq(2, n_legs), 1), # Circular: last leg returns to start
          `Duration (min)` = round(
            unname(vapply(
              legs,
              function(l) as.numeric(l$duration) / 60,
              numeric(1)
            )),
            2
          ),
          `Distance (km)` = round(
            unname(vapply(
              legs,
              function(l) as.numeric(l$distance) / 1000,
              numeric(1)
            )),
            3
          ),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        # Fallback: single entry
        legs_df <- data.frame(
          Start = 1,
          End = 2,
          `Duration (min)` = round(as.numeric(trip_data$duration) / 60, 2),
          `Distance (km)` = round(as.numeric(trip_data$distance) / 1000, 3),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }

      summary <- list(
        duration = as.numeric(trip_data$duration) / 60,
        distance = as.numeric(trip_data$distance) / 1000
      )

      list(trip = trip_sf_map, legs = legs_df, summary = summary, waypoint_order = waypoint_order)
    },
    error = function(e) {
      if (debug) {
        message("DEBUG [Trip API]: Error: ", e$message)
      }
      NULL
    }
  )
}
