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
#' @param autozoom Logical. Whether to enable auto-zoom by default. Defaults to `TRUE`.
#' @param tracking Logical. Whether to enable live tracking mode by default (updates route while dragging). Defaults to `FALSE`.
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
  zoom = NULL,
  autozoom = TRUE,
  tracking = FALSE
) {
  # 1. Check Dependencies
  gui_check_dependencies()

  # 2. Manage OSRM Server
  srv_context <- gui_setup_server(input_osrm, port)
  on.exit(srv_context$cleanup_fn(), add = TRUE)

  # 3. Configure Map View
  view <- gui_resolve_map_view(center, zoom, input_osrm)

  # 4. Configure 'osrm' package options for this session context
  old_opts <- options(
    osrm.server = paste0(srv_context$host, ":", srv_context$active_port, "/"),
    osrm.profile = "car"
  )
  on.exit(options(old_opts), add = TRUE)

  # 5. Define UI
  ui <- gui_ui_layout()

  # 6. Define Server
  # We define this factory-style to pass in static arguments
  server <- function(input, output, session) {
    # Initialize Map
    output$map <- mapgl::renderMaplibre({
      map_args <- list(style = style)
      if (!is.null(view$center)) {
        map_args$center <- view$center
        map_args$zoom <- view$zoom %||% 9
      }
      do.call(mapgl::maplibre, map_args) |>
        mapgl::add_navigation_control() |>
        mapgl::add_fullscreen_control() |>
        mapgl::add_scale_control()
    })

    # State
    locations <- shiny::reactiveValues(start = NULL, end = NULL, trip = list())
    init <- shiny::reactiveValues(
      route = FALSE,
      iso = FALSE,
      highlight = FALSE,
      trip = FALSE
    )
    autozoom_enabled <- shiny::reactiveVal(autozoom)
    tracking_enabled <- shiny::reactiveVal(tracking)

    # Store latest route summary for the sidebar
    route_summary <- shiny::reactiveVal(NULL)
    # Store current route steps for highlighting
    current_steps <- shiny::reactiveVal(NULL)
    # Store intermediate coords for tracking
    tracking_coords <- shiny::reactiveValues(start = NULL, end = NULL)
    # Store trip result for table
    trip_result <- shiny::reactiveVal(NULL)

    # --- UI Helpers ---
    output$autozoom_button_ui <- shiny::renderUI({
      state <- autozoom_enabled()
      label <- if (state) "Autozoom: ON" else "Autozoom: OFF"
      color <- if (state) "#5cb85c" else "#777"
      shiny::actionButton(
        "toggle_autozoom",
        label,
        style = sprintf(
          "background-color: %s; color: white; border-width: 0px;",
          color
        )
      )
    })

    output$tracking_button_ui <- shiny::renderUI({
      state <- tracking_enabled()
      label <- if (state) "Tracking: ON" else "Tracking: OFF"
      color <- if (state) "#5cb85c" else "#777"
      shiny::actionButton(
        "toggle_tracking",
        label,
        style = sprintf(
          "background-color: %s; color: white; border-width: 0px;",
          color
        )
      )
    })

    output$route_stats <- shiny::renderUI({
      stats <- route_summary()
      if (is.null(stats)) {
        return(NULL)
      }
      shiny::div(
        class = "route-stats-overlay",
        shiny::div(
          shiny::tags$b("Duration: "),
          shiny::span(
            paste(round(stats$duration, 1), "min"),
            class = "stat-val",
            style = "color: #007bff;"
          )
        ),
        shiny::div(
          shiny::tags$b("Distance: "),
          shiny::span(
            paste(round(stats$distance, 2), "km"),
            class = "stat-val",
            style = "color: #28a745;"
          )
        )
      )
    })

    shiny::observeEvent(input$toggle_autozoom, {
      autozoom_enabled(!autozoom_enabled())
    })
    shiny::observeEvent(input$toggle_tracking, {
      tracking_enabled(!tracking_enabled())
    })

    # --- Marker Helpers ---
    update_start <- function(lng, lat) {
      coords <- list(lat = round(lat, 5), lng = round(lng, 5))
      locations$start <- coords
      tracking_coords$start <- coords
      session$sendCustomMessage(
        'updateMarker',
        list(id = 'start', lng = lng, lat = lat)
      )
      shiny::updateTextInput(
        session,
        "start_coords_input",
        value = paste(coords$lat, coords$lng, sep = ", ")
      )
      route_summary(NULL)
      current_steps(NULL)
    }

    update_end <- function(lng, lat) {
      coords <- list(lat = round(lat, 5), lng = round(lng, 5))
      locations$end <- coords
      tracking_coords$end <- coords
      session$sendCustomMessage(
        'updateMarker',
        list(id = 'end', lng = lng, lat = lat)
      )
      shiny::updateTextInput(
        session,
        "end_coords_input",
        value = paste(coords$lat, coords$lng, sep = ", ")
      )
      route_summary(NULL)
      current_steps(NULL)
    }

    add_trip_point <- function(lng, lat) {
      id <- paste0("trip_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
      locations$trip[[id]] <- list(id = id, lat = lat, lng = lng)
      session$sendCustomMessage(
        'updateTripMarker',
        list(action = 'add', id = id, lng = lng, lat = lat)
      )
    }

    remove_trip_point <- function(id) {
      locations$trip[[id]] <- NULL
      tracking_coords[[id]] <- NULL
      session$sendCustomMessage(
        'updateTripMarker',
        list(action = 'remove', id = id)
      )
    }

    move_trip_point <- function(id, lng, lat) {
      if (!is.null(locations$trip[[id]])) {
        locations$trip[[id]]$lat <- lat
        locations$trip[[id]]$lng <- lng
      }
    }

    # --- Interaction Handlers ---
    shiny::observeEvent(input$map_click, {
      shiny::req(input$map_click)
      if (input$mode == 'trip') {
        add_trip_point(input$map_click$lng, input$map_click$lat)
      } else {
        update_start(input$map_click$lng, input$map_click$lat)
      }
    })

    shiny::observeEvent(input$js_right_click, {
      shiny::req(input$js_right_click)
      if (input$mode == 'route') {
        update_end(input$js_right_click$lng, input$js_right_click$lat)
      }
    })

    shiny::observeEvent(input$marker_dragged, {
      drag <- input$marker_dragged
      if (drag$id == "start") {
        update_start(drag$lng, drag$lat)
      } else if (drag$id == "end") {
        update_end(drag$lng, drag$lat)
      }
    })

    shiny::observeEvent(input$marker_moving, {
      moving <- input$marker_moving
      tracking_coords[[moving$id]] <- list(
        lat = round(moving$lat, 5),
        lng = round(moving$lng, 5)
      )
    })

    shiny::observeEvent(input$remove_trip_point, {
      remove_trip_point(input$remove_trip_point$id)
    })

    shiny::observeEvent(input$move_trip_point, {
      mv <- input$move_trip_point
      move_trip_point(mv$id, mv$lng, mv$lat)
    })

    # Reset
    shiny::observeEvent(input$reset, {
      locations$start <- NULL
      locations$end <- NULL
      locations$trip <- list()
      for (n in names(tracking_coords)) {
        tracking_coords[[n]] <- NULL
      }
      route_summary(NULL)
      current_steps(NULL)
      trip_result(NULL)
      session$sendCustomMessage("clearAllMarkers", "clear")
      shiny::updateTextInput(session, "start_coords_input", value = "")
      shiny::updateTextInput(session, "end_coords_input", value = "")

      proxy <- mapgl::maplibre_proxy("map")
      if (init$route) {
        mapgl::set_layout_property(proxy, "route_layer", "visibility", "none")
      }
      if (init$iso) {
        mapgl::set_layout_property(proxy, "iso_layer", "visibility", "none")
      }
      if (init$trip) {
        mapgl::set_layout_property(proxy, "trip_layer", "visibility", "none")
      }
      if (init$highlight) {
        mapgl::set_layout_property(
          proxy,
          "highlight_layer",
          "visibility",
          "none"
        )
      }
      mapgl::clear_legend(proxy)
    })

    # --- Calculation Logic ---

    active_coords <- shiny::throttle(
      shiny::reactive({
        res <- list(
          start = locations$start,
          end = locations$end,
          trip = locations$trip
        )
        if (!is.null(tracking_coords$start)) {
          res$start <- tracking_coords$start
        }
        if (!is.null(tracking_coords$end)) {
          res$end <- tracking_coords$end
        }
        for (id in names(tracking_coords)) {
          if (
            startsWith(id, "trip_") &&
              !is.null(res$trip[[id]]) &&
              !is.null(tracking_coords[[id]])
          ) {
            res$trip[[id]]$lat <- tracking_coords[[id]]$lat
            res$trip[[id]]$lng <- tracking_coords[[id]]$lng
          }
        }
        res
      }),
      150
    )

    # Route Calculation: Live Tracking
    shiny::observe({
      shiny::req(tracking_enabled(), input$mode == "route")
      coords <- active_coords()
      shiny::req(coords$start, coords$end)
      tryCatch(
        {
          route <- osrm::osrmRoute(
            src = c(coords$start$lng, coords$start$lat),
            dst = c(coords$end$lng, coords$end$lat),
            overview = "full"
          )
          if (!is.null(route)) {
            route_summary(list(
              duration = route$duration[1],
              distance = route$distance[1]
            ))
            proxy <- mapgl::maplibre_proxy("map")
            if (!init$route) {
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
              mapgl::set_source(proxy, layer_id = "route_layer", source = route)
            }
          }
        },
        error = function(e) NULL
      )
    })

    # Route Calculation: Stable Updates
    shiny::observe({
      shiny::req(input$mode == "route", locations$start, locations$end)
      calc_route <- function() {
        tryCatch(
          {
            route <- osrm::osrmRoute(
              src = c(locations$start$lng, locations$start$lat),
              dst = c(locations$end$lng, locations$end$lat),
              overview = "full"
            )
            if (!is.null(route)) {
              route_summary(list(
                duration = route$duration[1],
                distance = route$distance[1]
              ))
              proxy <- mapgl::maplibre_proxy("map")
              if (!init$route) {
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
              if (shiny::isolate(autozoom_enabled())) {
                pts_sf <- sf::st_as_sf(
                  data.frame(
                    lon = c(locations$start$lng, locations$end$lng),
                    lat = c(locations$start$lat, locations$end$lat)
                  ),
                  coords = c("lon", "lat"),
                  crs = 4326
                )
                combined_sf <- rbind(
                  sf::st_sf(geometry = sf::st_geometry(route)),
                  sf::st_sf(geometry = sf::st_geometry(pts_sf))
                )
                map_width <- session$clientData$output_map_width %||% 1000
                padding <- if (map_width < 768) 50 else 150
                mapgl::fit_bounds(
                  proxy,
                  combined_sf,
                  animate = TRUE,
                  padding = padding
                )
              }
            }
          },
          error = function(e) {
            shiny::showNotification(
              paste("Routing failed:", e$message),
              type = "error"
            )
          }
        )
      }
      shiny::withProgress(message = "Calculating Route...", calc_route())
    })

    # Trip Calculation: Live Tracking
    shiny::observe({
      shiny::req(tracking_enabled(), input$mode == "trip")
      coords_data <- active_coords()
      trip_pts <- coords_data$trip
      lons <- numeric(0)
      lats <- numeric(0)
      for (p in trip_pts) {
        if (!is.null(p$lng) && !is.null(p$lat)) {
          lons <- c(lons, as.numeric(p$lng))
          lats <- c(lats, as.numeric(p$lat))
        }
      }
      shiny::req(length(lons) >= 2)
      pts_df <- data.frame(lon = lons, lat = lats)
      tryCatch(
        {
          trip <- osrm::osrmTrip(loc = pts_df)
          if (!is.null(trip) && length(trip) > 0 && !is.null(trip[[1]]$trip)) {
            trip_geom <- trip[[1]]$trip
            route_summary(list(
              duration = trip[[1]]$summary$duration,
              distance = trip[[1]]$summary$distance
            ))
            proxy <- mapgl::maplibre_proxy("map")
            if (!init$trip) {
              mapgl::add_source(proxy, id = "trip_source", data = trip_geom)
              mapgl::add_line_layer(
                proxy,
                id = "trip_layer",
                source = "trip_source",
                line_color = "#984ea3",
                line_width = 5,
                line_opacity = 0.8
              )
              init$trip <- TRUE
            } else {
              mapgl::set_source(
                proxy,
                layer_id = "trip_layer",
                source = trip_geom
              )
            }
            # ... (Autozoom logic similar to stable can be added if robust enough for live)
          }
        },
        error = function(e) NULL
      )
    })

    # Trip Calculation: Stable Updates
    shiny::observe({
      shiny::req(input$mode == "trip")
      trip_pts <- locations$trip
      lons <- numeric(0)
      lats <- numeric(0)
      for (p in trip_pts) {
        if (!is.null(p$lng) && !is.null(p$lat)) {
          lons <- c(lons, as.numeric(p$lng))
          lats <- c(lats, as.numeric(p$lat))
        }
      }
      shiny::req(length(lons) >= 2)
      pts_df <- data.frame(lon = lons, lat = lats)

      calc_trip <- function() {
        tryCatch(
          {
            trip <- osrm::osrmTrip(loc = pts_df)
            if (
              !is.null(trip) && length(trip) > 0 && !is.null(trip[[1]]$trip)
            ) {
              trip_geom <- trip[[1]]$trip
              trip_result(trip[[1]])
              route_summary(list(
                duration = trip[[1]]$summary$duration,
                distance = trip[[1]]$summary$distance
              ))
              proxy <- mapgl::maplibre_proxy("map")
              if (!init$trip) {
                mapgl::add_source(proxy, id = "trip_source", data = trip_geom)
                mapgl::add_line_layer(
                  proxy,
                  id = "trip_layer",
                  source = "trip_source",
                  line_color = "#984ea3",
                  line_width = 5,
                  line_opacity = 0.8
                )
                init$trip <- TRUE
              } else {
                mapgl::set_source(
                  proxy,
                  layer_id = "trip_layer",
                  source = trip_geom
                )
                mapgl::set_layout_property(
                  proxy,
                  "trip_layer",
                  "visibility",
                  "visible"
                )
              }
              if (shiny::isolate(autozoom_enabled())) {
                bbox_trip <- sf::st_bbox(trip_geom)
                bbox_pts <- tryCatch(
                  sf::st_bbox(sf::st_as_sf(
                    pts_df,
                    coords = c("lon", "lat"),
                    crs = 4326
                  )),
                  error = function(e) NULL
                )
                combined_bbox <- bbox_trip
                if (!is.null(bbox_pts)) {
                  try(
                    {
                      combined_bbox <- sf::st_bbox(
                        c(
                          xmin = min(bbox_trip["xmin"], bbox_pts["xmin"]),
                          ymin = min(bbox_trip["ymin"], bbox_pts["ymin"]),
                          xmax = max(bbox_trip["xmax"], bbox_pts["xmax"]),
                          ymax = max(bbox_trip["ymax"], bbox_pts["ymax"])
                        ),
                        crs = 4326
                      )
                    },
                    silent = TRUE
                  )
                }
                map_width <- session$clientData$output_map_width %||% 1000
                padding <- if (map_width < 768) 50 else 150
                mapgl::fit_bounds(
                  proxy,
                  combined_bbox,
                  animate = TRUE,
                  padding = padding
                )
              }
            }
          },
          error = function(e) {
            shiny::showNotification(
              paste("Trip failed:", e$message),
              type = "error"
            )
          }
        )
      }
      shiny::withProgress(message = "Calculating Trip...", calc_trip())
    })

    # Isochrone
    shiny::observe({
      shiny::req(input$mode == "iso", locations$start)
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
          # Valid 'n' values for osrmIsochrone
          n_vals <- c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000)
          
          # Use slider index (1-9) to pick value, default to 500 (index 3) if out of bounds
          n_val <- tryCatch(n_vals[as.integer(input$iso_res)], error = function(e) 500)
          if (is.na(n_val)) n_val <- 500

            iso <- osrm::osrmIsochrone(
              loc = c(locations$start$lng, locations$start$lat),
              breaks = breaks,
              n = n_val
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
              if (shiny::isolate(autozoom_enabled())) {
                map_width <- session$clientData$output_map_width %||% 1000
                padding <- if (map_width < 768) 20 else 50
                mapgl::fit_bounds(proxy, iso, animate = TRUE, padding = padding)
              }
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
      res <- api_fetch_route_detailed(locations$start, locations$end)

      if (!is.null(res) && length(res$routes) > 0) {
        steps <- res$routes[[1]]$legs[[1]]$steps
        current_steps(steps)
        df <- do.call(
          rbind,
          lapply(steps, function(s) {
            instr <- s$maneuver$type
            if (!is.null(s$maneuver$modifier)) {
              instr <- paste(instr, s$maneuver$modifier)
            }
            data.frame(
              Instruction = instr,
              Road = if (is.null(s$name) || s$name == "") "-" else s$name,
              `Distance (km)` = round(s$distance / 1000, 3),
              `Duration (min)` = round(s$duration / 60, 2),
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          })
        )
        DT::datatable(
          df,
          selection = "multiple",
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = FALSE
        )
      }
    })

    shiny::observeEvent(input$clear_selection, {
      DT::selectRows(DT::dataTableProxy("itinerary_table"), NULL)
    })

    shiny::observeEvent(
      input$itinerary_table_rows_selected,
      {
        proxy <- mapgl::maplibre_proxy("map")
        idx <- input$itinerary_table_rows_selected
        steps <- current_steps()
        if (is.null(idx) || length(idx) == 0 || is.null(steps)) {
          if (init$highlight) {
            mapgl::set_layout_property(
              proxy,
              "highlight_layer",
              "visibility",
              "none"
            )
          }
          return()
        }
        segment_list <- lapply(idx, function(i) {
          step <- steps[[i]]
          coords <- matrix(
            unlist(step$geometry$coordinates),
            ncol = 2,
            byrow = TRUE
          )
          sf::st_linestring(coords)
        })
        segment_sfc <- sf::st_sfc(segment_list, crs = 4326)
        combined_geom <- sf::st_combine(segment_sfc)
        segment_sf <- sf::st_sf(geometry = combined_geom)
        if (!init$highlight) {
          mapgl::add_source(proxy, id = "highlight_source", data = segment_sf)
          mapgl::add_line_layer(
            proxy,
            id = "highlight_layer",
            source = "highlight_source",
            line_color = "#facc15",
            line_width = 8,
            line_opacity = 0.9
          )
          init$highlight <- TRUE
        } else {
          mapgl::set_source(
            proxy,
            layer_id = "highlight_layer",
            source = segment_sf
          )
          mapgl::set_layout_property(
            proxy,
            "highlight_layer",
            "visibility",
            "visible"
          )
        }
      },
      ignoreNULL = FALSE
    )

    output$trip_table <- DT::renderDataTable({
      shiny::req(input$mode == "trip")
      trip_data <- trip_result()
      shiny::req(trip_data)
      trip_geom <- trip_data$trip
      df <- sf::st_drop_geometry(trip_geom)
      if ("duration" %in% names(df)) {
        df$duration <- round(df$duration, 2)
      }
      if ("distance" %in% names(df)) {
        df$distance <- round(df$distance, 2)
      }
      colnames(df) <- c("From", "To", "Duration (min)", "Distance (km)")
      DT::datatable(
        df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    shiny::observeEvent(input$quit_app, {
      shiny::stopApp()
    })
  }

  # 7. Run Gadget
  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}
