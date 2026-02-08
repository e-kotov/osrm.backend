#' Launch a GUI to View and Debug OSRM Routing
#'
#' Launches a lightweight Shiny application to interactively visualize routing
#' on a local OSRM server. This interface mimics the `r5rgui` experience,
#' supporting left-click for start, right-click for end, and draggable markers.
#'
#' @details
#' The function checks for optional dependencies `shiny`, `mapgl`, `osrm`, `sf`, and `DT`.
#' If missing, it prompts the user to install them.
#'
#' It attempts to detect an active OSRM server. If an `osrm_job` object (from
#' `osrm_start()`) is passed, it uses that configuration. If a path is passed,
#' it will start a temporary server for the session.
#'
#' @param input_osrm Optional. Can be:
#'   \itemize{
#'     \item An `osrm_job` object (process) returned by `osrm_start()` or `osrm_start_server()`.
#'       When providing a process, you must also specify `port` explicitly.
#'     \item A path string to an `.osrm.hsgr` or `.osrm.mldgr` file.
#'     \item A path string to an `.osm.pbf` file (will be prepared and started).
#'     \item `NULL` (default): Auto-detects a running OSRM server using [osrm_servers()].
#'       Errors if no servers are running.
#'   }
#' @param port Integer or `"auto"`. The port the server is running on (or should run on).
#'   Defaults to `"auto"`, which attempts to auto-detect a running OSRM server using
#'   [osrm_servers()]. If multiple servers are running, the most recent one is selected
#'   with a warning. If no servers are running, an error is raised.
#' @param style Character. Map style for `mapgl`. Defaults to "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json".
#' @param center Numeric vector of length 2 (`c(lng, lat)`), or named list 
#'   (`list(lng = ..., lat = ...)`), or `NULL` (default). Initial map center. 
#'   If `NULL` and `input_osrm` is a `.osm.pbf` file, attempts to auto-center 
#'   on the PBF extent. Uses `osmium fileinfo` (fast, all file sizes) if 
#'   available, otherwise falls back to GDAL or sampling features (< 50 MB).
#' @param zoom Numeric. Initial zoom level. If `NULL` (default) and center is 
#'   auto-detected from PBF, defaults to 9. Otherwise uses map default.
#' @return No return value; launches a Shiny Gadget.
#' @export
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # 1. Auto-detect running server (errors if none running):
#'   osrm_gui()
#'
#'   # 2. Connect to specific port:
#'   # osrm_gui(port = 5001)
#'
#'   # 3. Start from a graph file (auto-center on PBF):
#'   # osrm_gui("berlin.osrm.mldgr")
#'
#'   # 4. Start from PBF with auto-center:
#'   # osrm_gui("berlin.osm.pbf")
#'
#'   # 5. Explicit center and zoom:
#'   # osrm_gui(port = 5001, center = c(13.4, 52.5), zoom = 12)
#'
#'   # 6. Use an existing process (must specify port):
#'   # srv <- osrm_start("graph.osrm.mldgr", port = 6000)
#'   # osrm_gui(srv, port = 6000)
#' }
#' }
osrm_gui <- function(
  input_osrm = NULL,
  port = "auto",
  style = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
  center = NULL,
  zoom = NULL
) {
  # 1. Check Dependencies
  required_pkgs <- c("shiny", "mapgl", "osrm", "sf", "DT")
  missing_pkgs <- required_pkgs[
    !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required for the GUI but are not installed: ",
      paste(missing_pkgs, collapse = ", "),
      ".\n",
      "Please install them using install.packages(c(",
      paste(sprintf("'%s'", missing_pkgs), collapse = ", "),
      "))",
      call. = FALSE
    )
  }

  # 2. Manage OSRM Server
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
        "When providing a process object, you must also specify the port explicitly.\n",
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
        "). Using most recent (port ", active_port, ", started at ",
        format(most_recent$started_at), "). ",
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

  # 3. Auto-center on PBF extent if applicable
  auto_center <- NULL
  auto_zoom <- NULL
  if (is.null(center)) {
    # Determine the PBF path for extent detection
    pbf_path <- NULL
    if (is.character(input_osrm) &&
        grepl("\\.osm\\.pbf$", input_osrm, ignore.case = TRUE)) {
      pbf_path <- input_osrm
    } else if (is.null(input_osrm) || inherits(input_osrm, "process")) {
      # Auto-detect or process mode: look up input_osm from registry
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
        message("Auto-centered map on PBF extent: ", paste(round(auto_center, 4), collapse = ", "))
      }
    }
  }
  
  # Normalize center parameter
  map_center <- center %||% auto_center
  map_zoom <- zoom %||% auto_zoom
  if (!is.null(map_center)) {
    if (is.list(map_center)) {
      map_center <- c(map_center$lng %||% map_center$lon %||% map_center$x,
                      map_center$lat %||% map_center$y)
    }
    map_center <- as.numeric(map_center)
    if (length(map_center) != 2 || any(is.na(map_center))) {
      stop("'center' must be a numeric vector of length 2 (lng, lat) or a named list.", call. = FALSE)
    }
  }

  on.exit(
    {
      if (
        kill_on_exit && !is.null(server_process) && server_process$is_alive()
      ) {
        message("Stopping temporary OSRM server...")
        server_process$kill()
      }
    },
    add = TRUE
  )

  # Configure 'osrm' package options for this session context
  old_opts <- options(
    osrm.server = paste0(host, ":", active_port, "/"),
    osrm.profile = "car"
  )
  on.exit(options(old_opts), add = TRUE)

  # 3. Define UI
  ui <- shiny::fluidPage(
    # Header
    shiny::div(
      style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 0;",
      shiny::div(
        style = "display: flex; align-items: center;",
        shiny::h3(
          shiny::HTML("<b>osrm.backend</b> GUI"),
          style = "margin: 0;"
        )
      ),
      shiny::actionButton(
        "quit_app",
        "Quit",
        style = "background-color: #d9534f; color: white; border-width: 0px;"
      )
    ),
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        "
      #shiny-notification-panel { top: 70px; right: 10px; left: auto; bottom: auto; }
      .map-wrapper { position: relative; }
    "
      )),
      # Inject custom JS for right-click and marker dragging
      shiny::tags$script(shiny::HTML(
        "
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

            if (markerId === 'start') {
              if (!startMarker) {
                startMarker = new maplibregl.Marker({ draggable: true, color: '#009E73' })
                  .setLngLat(lngLat)
                  .addTo(map);
                startMarker.on('dragend', () => createDragEndCallback('start')(startMarker));
              } else {
                startMarker.setLngLat(lngLat);
              }
            } else if (markerId === 'end') {
              if (!endMarker) {
                endMarker = new maplibregl.Marker({ draggable: true, color: '#D55E00' })
                  .setLngLat(lngLat)
                  .addTo(map);
                endMarker.on('dragend', () => createDragEndCallback('end')(endMarker));
              } else {
                endMarker.setLngLat(lngLat);
              }
            }
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
          });

          obs.disconnect();
        }
      });

      observer.observe(mapElement, { childList: true, subtree: true });
    }

    $(document).on('shiny:connected', () => {
      initializeMapListeners('map');
    });
    "
      ))
    ),

    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("OSRM Controls"),
        shiny::selectInput(
          "mode",
          "Analysis Mode:",
          choices = c("Route" = "route", "Isochrone" = "iso")
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
            "Resolution:",
            min = 10,
            max = 100,
            value = 30
          )
        ),

        shiny::hr(),
        shiny::h4("Locations"),
        shiny::helpText(
          "Left-click map: Start point",
          shiny::tags$br(),
          "Right-click map: End point (Route mode)",
          shiny::tags$br(),
          "Drag markers to adjust."
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

      shiny::mainPanel(
        width = 9,
        shiny::tags$style(
          type = "text/css",
          "#map {height: calc(60vh) !important;}"
        ),
        shiny::div(
          class = "map-wrapper",
          mapgl::maplibreOutput("map")
        ),
        shiny::hr(),
        shiny::conditionalPanel(
          condition = "input.mode == 'route'",
          shiny::h4("Route Details"),
          DT::dataTableOutput("itinerary_table")
        )
      )
    )
  )

  # 4. Define Server
  server <- function(input, output, session) {
    # Initialize Map
    output$map <- mapgl::renderMaplibre({
      map_args <- list(style = style)
      if (!is.null(map_center)) {
        map_args$center <- map_center
        map_args$zoom <- map_zoom %||% 9
      }
      do.call(mapgl::maplibre, map_args) |>
        mapgl::add_navigation_control() |>
        mapgl::add_fullscreen_control() |>
        mapgl::add_scale_control()
    })

    # State
    locations <- shiny::reactiveValues(start = NULL, end = NULL)
    init <- shiny::reactiveValues(route = FALSE, iso = FALSE)

    # --- Marker Helpers ---
    update_start <- function(lng, lat) {
      coords <- list(lat = round(lat, 5), lon = round(lng, 5))
      locations$start <- coords
      session$sendCustomMessage(
        'updateMarker',
        list(id = 'start', lng = lng, lat = lat)
      )
      shiny::updateTextInput(
        session,
        "start_coords_input",
        value = paste(coords$lat, coords$lon, sep = ", ")
      )
    }

    update_end <- function(lng, lat) {
      coords <- list(lat = round(lat, 5), lon = round(lng, 5))
      locations$end <- coords
      session$sendCustomMessage(
        'updateMarker',
        list(id = 'end', lng = lng, lat = lat)
      )
      shiny::updateTextInput(
        session,
        "end_coords_input",
        value = paste(coords$lat, coords$lon, sep = ", ")
      )
    }

    # --- Interaction Handlers ---
    # Left Click -> Start
    shiny::observeEvent(input$map_click, {
      shiny::req(input$map_click)
      update_start(input$map_click$lng, input$map_click$lat)
    })

    # Right Click -> End (Only in route mode)
    shiny::observeEvent(input$js_right_click, {
      shiny::req(input$js_right_click)
      if (input$mode == 'route') {
        update_end(input$js_right_click$lng, input$js_right_click$lat)
      }
    })

    # Drag
    shiny::observeEvent(input$marker_dragged, {
      drag <- input$marker_dragged
      if (drag$id == "start") {
        update_start(drag$lng, drag$lat)
      } else if (drag$id == "end") {
        update_end(drag$lng, drag$lat)
      }
    })

    # Reset
    shiny::observeEvent(input$reset, {
      locations$start <- NULL
      locations$end <- NULL
      session$sendCustomMessage('clearAllMarkers', 'clear')
      shiny::updateTextInput(session, "start_coords_input", value = "")
      shiny::updateTextInput(session, "end_coords_input", value = "")

      proxy <- mapgl::maplibre_proxy("map")
      # We cannot remove sources easily, so we hide layers or clear data
      if (init$route) {
        # Hide route layer
        mapgl::set_layout_property(proxy, "route_layer", "visibility", "none")
      }
      if (init$iso) {
        # Hide iso layer
        mapgl::set_layout_property(proxy, "iso_layer", "visibility", "none")
      }
      mapgl::clear_legend(proxy)
    })

    # --- Calculation Logic ---

    # Route
    shiny::observe({
      shiny::req(input$mode == "route")
      shiny::req(locations$start, locations$end)

      shiny::withProgress(message = 'Calculating Route...', {
        tryCatch(
          {
            route <- osrm::osrmRoute(
              src = c(locations$start$lon, locations$start$lat),
              dst = c(locations$end$lon, locations$end$lat),
              overview = "full"
            )

            if (!is.null(route)) {
              proxy <- mapgl::maplibre_proxy("map")

              if (!init$route) {
                # Initial Add
                mapgl::add_source(proxy, id = "route_source", data = route)
                mapgl::add_line_layer(
                  proxy,
                  id = "route_layer",
                  source = "route_source",
                  line_color = "#3b82f6",
                  line_width = 5,
                  line_opacity = 0.8
                )
                init$route <- TRUE
              } else {
                # Update
                mapgl::set_source(
                  proxy,
                  layer_id = "route_layer",
                  source = route
                )
                mapgl::set_layout_property(
                  proxy,
                  "route_layer",
                  "visibility",
                  "visible"
                )
              }
              
              # Combined bounds: include both the route geometry and the user-selected points.
              # This ensures 'wiggly' routes aren't cut off while guaranteeing markers are visible.
              # Padding increased to 150px for a less aggressive zoom.
              pts_sf <- sf::st_as_sf(
                data.frame(
                  lon = c(locations$start$lon, locations$end$lon),
                  lat = c(locations$start$lat, locations$end$lat)
                ),
                coords = c("lon", "lat"),
                crs = 4326
              )
              # Combine into a single sf object with a geometry column named 'geometry'
              combined_sf <- rbind(
                sf::st_sf(geometry = sf::st_geometry(route)),
                sf::st_sf(geometry = sf::st_geometry(pts_sf))
              )
              mapgl::fit_bounds(proxy, combined_sf, animate = TRUE, padding = 150)
            }
          },
          error = function(e) {
            shiny::showNotification(
              paste("Routing failed:", e$message),
              type = "error"
            )
          }
        )
      })
    })

    # Isochrone
    shiny::observe({
      shiny::req(input$mode == "iso")
      shiny::req(locations$start) # Only start needed

      # If we switch to iso, end marker might be confusing, but we leave it for now or user resets
      breaks <- tryCatch(
        sort(as.numeric(unlist(strsplit(input$iso_breaks, ",")))),
        error = function(e) c(5, 10, 15)
      )
      if (length(breaks) == 0) {
        breaks <- c(5, 10, 15)
      }

      shiny::withProgress(message = 'Computing Isochrones...', {
        tryCatch(
          {
            iso <- osrm::osrmIsochrone(
              loc = c(locations$start$lon, locations$start$lat),
              breaks = breaks,
              n = input$iso_res
            )

            if (!is.null(iso) && nrow(iso) > 0) {
              proxy <- mapgl::maplibre_proxy("map")

              if (!init$iso) {
                mapgl::add_source(proxy, id = "iso_source", data = iso)
                mapgl::add_fill_layer(
                  proxy,
                  id = "iso_layer",
                  source = "iso_source",
                  fill_color = mapgl::interpolate(
                    column = "isomax",
                    values = c(0, max(breaks)),
                    stops = c("#fde725", "#440154")
                  ),
                  fill_opacity = 0.5,
                  fill_outline_color = "white"
                )
                init$iso <- TRUE
              } else {
                mapgl::set_source(proxy, layer_id = "iso_layer", source = iso)
                mapgl::set_layout_property(
                  proxy,
                  "iso_layer",
                  "visibility",
                  "visible"
                )
                # Update paint properties if breaks changed max
                mapgl::set_paint_property(
                  proxy,
                  layer_id = "iso_layer",
                  name = "fill-color",
                  value = mapgl::interpolate(
                    column = "isomax",
                    values = c(0, max(breaks)),
                    stops = c("#fde725", "#440154")
                  )
                )
              }
              mapgl::fit_bounds(proxy, iso, animate = TRUE, padding = 50)
            }
          },
          error = function(e) {
            shiny::showNotification(
              paste("Isochrone failed:", e$message),
              type = "error"
            )
          }
        )
      })
    })

    # --- Table Output ---
    output$itinerary_table <- DT::renderDataTable({
      shiny::req(input$mode == "route", locations$start, locations$end)
      # We re-calculate purely for the table data display (quick enough for OSRM)
      # or ideally store reactive. For simplicity re-running osrmRoute overview=FALSE
      res <- tryCatch(
        {
          osrm::osrmRoute(
            src = c(locations$start$lon, locations$start$lat),
            dst = c(locations$end$lon, locations$end$lat),
            overview = FALSE
          )
        },
        error = function(e) NULL
      )

      if (!is.null(res)) {
        df <- data.frame(
          Parameter = c("Duration (min)", "Distance (km)"),
          Value = c(round(res["duration"], 2), round(res["distance"], 2))
        )
        DT::datatable(df, options = list(dom = 't'), rownames = FALSE)
      }
    })

    # Quit
    shiny::observeEvent(input$quit_app, {
      shiny::stopApp()
    })
  }

  # 5. Run Gadget
  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
