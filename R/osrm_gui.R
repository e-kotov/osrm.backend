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
#' @param update_while_drag Logical. Whether to enable live tracking mode by default (updates route while dragging). Defaults to `FALSE`.
#' @param debug Logical. Whether to enable debug mode (prints OSRM requests to console). Defaults to `FALSE`.
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
#'
#'   # 7. Enable debug mode:
#'   # osrm_gui(debug = TRUE)
#' }
#' }
osrm_gui <- function(
  input_osrm = NULL,
  port = "auto",
  style = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
  center = NULL,
  zoom = NULL,
  autozoom = TRUE,
  update_while_drag = FALSE,
  debug = FALSE
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
    locations <- shiny::reactiveValues(
      start = NULL,
      end = NULL,
      iso_start = NULL,
      trip = list()
    )
    init <- shiny::reactiveValues(
      route = FALSE,
      iso = FALSE,
      highlight = FALSE,
      trip = FALSE
    )
    autozoom_enabled <- shiny::reactiveVal(autozoom)
    live_update_enabled <- shiny::reactiveVal(update_while_drag)
    is_dragging <- shiny::reactiveVal(FALSE)

    # --- Debug Helper ---
    debug_msg <- function(...) {
      if (debug) {
        message("DEBUG [", format(Sys.time(), "%H:%M:%S"), "]: ", ...)
      }
    }

    if (debug) {
      debug_msg("Starting GUI in DEBUG mode.")
      debug_msg("OSRM Server: ", getOption("osrm.server"))
      debug_msg("OSRM Profile: ", getOption("osrm.profile"))
    }

    # Store latest route summary for the sidebar
    route_summary <- shiny::reactiveVal(NULL)
    # Store current route steps for highlighting
    current_steps <- shiny::reactiveVal(NULL)
    # Store intermediate coords for tracking
    tracking_coords <- shiny::reactiveValues(
      start = NULL,
      end = NULL,
      iso_start = NULL
    )
    # Store trip result for table
    trip_result <- shiny::reactiveVal(NULL)

    # Time of last mode switch (for robust autozoom suppression)
    last_mode_switch <- shiny::reactiveVal(Sys.time())

    # --- History Manager ---
    history <- shiny::reactiveValues(past = list(), future = list())

    # Snapshot current state
    get_state_snapshot <- function() {
      shiny::reactiveValuesToList(locations)
    }

    # Commit current state to history BEFORE making changes
    commit <- function() {
      history$past <- c(history$past, list(get_state_snapshot()))
      history$future <- list() # Clear future on new branch
    }

    # Restore state from snapshot
    restore <- function(snapshot) {
      # 1. Update Internal State
      locations$start <- snapshot$start
      locations$end <- snapshot$end
      locations$iso_start <- snapshot$iso_start
      locations$trip <- snapshot$trip

      # 2. Clear Tracking Overrides (Fix for Undo/Redo glitch with live updates)
      tracking_coords$start <- NULL
      tracking_coords$end <- NULL
      tracking_coords$iso_start <- NULL
      for (n in names(tracking_coords)) {
        if (startsWith(n, "trip_")) tracking_coords[[n]] <- NULL
      }

      # 3. Sync Map Visuals
      session$sendCustomMessage("clearAllMarkers", "clear")

      if (!is.null(snapshot$start)) {
        session$sendCustomMessage(
          'updateMarker',
          list(id = 'start', lng = snapshot$start$lng, lat = snapshot$start$lat)
        )
      }
      if (!is.null(snapshot$end)) {
        session$sendCustomMessage(
          'updateMarker',
          list(id = 'end', lng = snapshot$end$lng, lat = snapshot$end$lat)
        )
      }
      if (!is.null(snapshot$iso_start)) {
        session$sendCustomMessage(
          'updateMarker',
          list(
            id = 'iso_start',
            lng = snapshot$iso_start$lng,
            lat = snapshot$iso_start$lat
          )
        )
      }

      # Update inputs based on active mode for clarity (optional, but good for UX)
      if (input$mode == "route") {
        if (!is.null(snapshot$start)) {
          shiny::updateTextInput(
            session,
            "start_coords_input",
            value = paste(snapshot$start$lat, snapshot$start$lng, sep = ", ")
          )
        }
        if (!is.null(snapshot$end)) {
          shiny::updateTextInput(
            session,
            "end_coords_input",
            value = paste(snapshot$end$lat, snapshot$end$lng, sep = ", ")
          )
        }
      } else if (input$mode == "iso") {
        if (!is.null(snapshot$iso_start)) {
          shiny::updateTextInput(
            session,
            "start_coords_input",
            value = paste(
              snapshot$iso_start$lat,
              snapshot$iso_start$lng,
              sep = ", "
            )
          )
        }
      }

      if (!is.null(snapshot$trip)) {
        for (pt in snapshot$trip) {
          session$sendCustomMessage(
            'updateTripMarker',
            list(action = 'add', id = pt$id, lng = pt$lng, lat = pt$lat)
          )
        }
      }

      # Trigger route recalculation by invalidating/clearing result cache
      route_summary(NULL)
      current_steps(NULL)
      trip_result(NULL)

      # Force map layer updates (Reset clears them, Observers re-add them)
      proxy <- mapgl::maplibre_proxy("map")

      # Ensure all initialized layers are visible
      if (init$route) {
        mapgl::set_layout_property(
          proxy,
          "route_layer",
          "visibility",
          "visible"
        )
      }
      if (init$iso) {
        mapgl::set_layout_property(proxy, "iso_layer", "visibility", "visible")
      }
      if (init$trip) {
        mapgl::set_layout_property(proxy, "trip_layer", "visibility", "visible")
      }
      if (init$highlight) {
        mapgl::set_layout_property(
          proxy,
          "highlight_layer",
          "visibility",
          "visible"
        )
      }

      has_content <- !is.null(snapshot$start) ||
        !is.null(snapshot$end) ||
        !is.null(snapshot$iso_start) ||
        length(snapshot$trip) > 0
      if (!has_content) {
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
      }
    }

    output$map_edit_controls <- shiny::renderUI({
      has_history <- length(history$past) > 0
      has_future <- length(history$future) > 0
      has_content <- !is.null(locations$start) ||
        !is.null(locations$end) ||
        !is.null(locations$iso_start) ||
        length(locations$trip) > 0

      if (!has_history && !has_future && !has_content) {
        return(NULL)
      }

      btns <- list()

      style_base <- "background: white; border: none; border-radius: 4px; box-shadow: 0 0 0 2px rgba(0,0,0,0.1); width: 30px; height: 30px; padding: 0; color: #555; margin-bottom: 5px;"

      if (has_history) {
        btns[[length(btns) + 1]] <- shiny::actionButton(
          "undo_btn",
          shiny::icon("rotate-left"),
          style = style_base,
          title = "Undo"
        )
      }

      if (has_future) {
        btns[[length(btns) + 1]] <- shiny::actionButton(
          "redo_btn",
          shiny::icon("rotate-right"),
          style = style_base,
          title = "Redo"
        )
      }

      if (has_content) {
        btns[[length(btns) + 1]] <- shiny::actionButton(
          "clear_map_icon",
          shiny::icon("trash"),
          style = style_base,
          title = "Clear Map"
        )
      }

      shiny::div(
        style = "position: absolute; top: 150px; right: 10px; z-index: 1000; display: flex; flex-direction: column;",
        btns
      )
    })

    shiny::observeEvent(input$undo_btn, {
      req(length(history$past) > 0)
      current <- get_state_snapshot()
      # Push current to future
      history$future <- c(list(current), history$future)
      # Pop from past
      prev <- history$past[[length(history$past)]]
      history$past <- history$past[-length(history$past)]
      restore(prev)
    })

    shiny::observeEvent(input$redo_btn, {
      req(length(history$future) > 0)
      current <- get_state_snapshot()
      # Push current to past
      history$past <- c(history$past, list(current))
      # Pop from future
      next_state <- history$future[[1]]
      history$future <- history$future[-1]
      restore(next_state)
    })

    # --- UI Helpers ---
    output$mode_button_ui <- shiny::renderUI({
      current_mode <- input$mode
      labels <- c("route" = "Route", "iso" = "Isochrone", "trip" = "Trip")
      label <- labels[current_mode]
      if (is.na(label)) {
        label <- "Route"
      }

      shiny::actionButton(
        "cycle_mode",
        paste("Mode:", label),
        style = "background-color: #337ab7; color: white; border-width: 0px;"
      )
    })

    shiny::observeEvent(input$cycle_mode, {
      modes <- c("route", "iso", "trip")
      current_idx <- match(input$mode, modes)
      if (is.na(current_idx)) {
        current_idx <- 1
      }
      next_idx <- if (current_idx >= length(modes)) 1 else current_idx + 1
      shiny::updateSelectInput(session, "mode", selected = modes[next_idx])
    })

    # Mode Switch: Update Inputs (UX)
    shiny::observeEvent(input$mode, {
      if (input$mode == "route") {
        val_start <- if (!is.null(locations$start)) {
          paste(locations$start$lat, locations$start$lng, sep = ", ")
        } else {
          ""
        }
        val_end <- if (!is.null(locations$end)) {
          paste(locations$end$lat, locations$end$lng, sep = ", ")
        } else {
          ""
        }
        shiny::updateTextInput(session, "start_coords_input", value = val_start)
        shiny::updateTextInput(session, "end_coords_input", value = val_end)
      } else if (input$mode == "iso") {
        val_start <- if (!is.null(locations$iso_start)) {
          paste(locations$iso_start$lat, locations$iso_start$lng, sep = ", ")
        } else {
          ""
        }
        shiny::updateTextInput(session, "start_coords_input", value = val_start)
        shiny::updateTextInput(session, "end_coords_input", value = "")
      }
    })

    # Mode Switch: Record timestamp to suppress immediate auto-zooms
    shiny::observeEvent(
      input$mode,
      {
        last_mode_switch(Sys.time())
      },
      priority = 100
    )

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
      state <- live_update_enabled()
      label <- if (state) "Update on Drag: ON" else "Update on Drag: OFF"
      color <- if (state) "#5cb85c" else "#777"
      shiny::actionButton(
        "toggle_update_on_drag",
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
    shiny::observeEvent(input$toggle_update_on_drag, {
      live_update_enabled(!live_update_enabled())
    })

    # --- Marker Helpers ---
    update_start <- function(lng, lat) {
      commit()
      is_dragging(FALSE)
      coords <- list(lat = round(lat, 5), lng = round(lng, 5))
      locations$start <- coords
      tracking_coords$start <- NULL
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
      commit()
      is_dragging(FALSE)
      coords <- list(lat = round(lat, 5), lng = round(lng, 5))
      locations$end <- coords
      tracking_coords$end <- NULL
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

    update_iso_start <- function(lng, lat) {
      commit()
      is_dragging(FALSE)
      coords <- list(lat = round(lat, 5), lng = round(lng, 5))
      locations$iso_start <- coords
      tracking_coords$iso_start <- NULL
      session$sendCustomMessage(
        'updateMarker',
        list(id = 'iso_start', lng = lng, lat = lat)
      )
      shiny::updateTextInput(
        session,
        "start_coords_input",
        value = paste(coords$lat, coords$lng, sep = ", ")
      )
    }

    add_trip_point <- function(lng, lat) {
      commit()
      id <- paste0("trip_", as.integer(Sys.time()), "_", sample(1000:9999, 1))
      locations$trip[[id]] <- list(id = id, lat = lat, lng = lng)
      session$sendCustomMessage(
        'updateTripMarker',
        list(action = 'add', id = id, lng = lng, lat = lat)
      )
    }

    remove_trip_point <- function(id) {
      commit()
      is_dragging(FALSE)
      locations$trip[[id]] <- NULL
      tracking_coords[[id]] <- NULL
      session$sendCustomMessage(
        'updateTripMarker',
        list(action = 'remove', id = id)
      )
    }

    move_trip_point <- function(id, lng, lat) {
      if (!is.null(locations$trip[[id]])) {
        commit()
        is_dragging(FALSE)
        locations$trip[[id]]$lat <- lat
        locations$trip[[id]]$lng <- lng
        tracking_coords[[id]] <- NULL
      }
    }

    # --- Interaction Handlers ---
    shiny::observeEvent(input$map_click, {
      shiny::req(input$map_click)
      if (input$mode == 'trip') {
        add_trip_point(input$map_click$lng, input$map_click$lat)
      } else if (input$mode == 'iso') {
        update_iso_start(input$map_click$lng, input$map_click$lat)
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

    # --- Live Events (Direct for responsiveness) ---

    # 1. Marker Dragged (Drop)
    shiny::observeEvent(input$marker_dragged, {
      drag <- input$marker_dragged
      if (is.null(drag)) {
        return()
      }

      if (drag$id == "start") {
        update_start(drag$lng, drag$lat)
      } else if (drag$id == "end") {
        update_end(drag$lng, drag$lat)
      } else if (drag$id == "iso_start") {
        update_iso_start(drag$lng, drag$lat)
      }
    })

    # 2. Marker Moving (Live Drag)
    shiny::observeEvent(input$marker_moving, {
      is_dragging(TRUE)
      moving <- input$marker_moving
      if (is.null(moving)) {
        return()
      }

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

    # Reset Logic
    reset_all <- function() {
      commit()
      locations$start <- NULL
      locations$end <- NULL
      locations$iso_start <- NULL
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
    }

    shiny::observeEvent(input$reset, {
      reset_all()
    })
    shiny::observeEvent(input$clear_map_icon, {
      reset_all()
    })

    # --- Calculation Logic ---

    active_coords <- shiny::throttle(
      shiny::reactive({
        res <- list(
          start = locations$start,
          end = locations$end,
          iso_start = locations$iso_start,
          trip = locations$trip
        )
        if (!is.null(tracking_coords$start)) {
          res$start <- tracking_coords$start
        }
        if (!is.null(tracking_coords$end)) {
          res$end <- tracking_coords$end
        }
        if (!is.null(tracking_coords$iso_start)) {
          res$iso_start <- tracking_coords$iso_start
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
      250
    )

    # Route Calculation: Live Tracking
    shiny::observe({
      shiny::req(live_update_enabled(), is_dragging())
      coords <- active_coords()
      shiny::req(coords$start, coords$end)
      tryCatch(
        {
          debug_msg(
            "Route (Live) request: ",
            coords$start$lng,
            ",",
            coords$start$lat,
            " -> ",
            coords$end$lng,
            ",",
            coords$end$lat
          )
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
              mapgl::set_layout_property(
                proxy,
                "route_layer",
                "visibility",
                "visible"
              )
            }
          }
        },
        error = function(e) NULL
      )
    })

    # Route Calculation: Stable Updates
    shiny::observe({
      shiny::req(locations$start, locations$end)
      calc_route <- function() {
        tryCatch(
          {
            debug_msg(
              "Route (Stable) request: ",
              locations$start$lng,
              ",",
              locations$start$lat,
              " -> ",
              locations$end$lng,
              ",",
              locations$end$lat
            )
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
              # Check if enough time has passed since mode switch (e.g., 1.0s)
              time_since_switch <- as.numeric(difftime(
                Sys.time(),
                last_mode_switch(),
                units = "secs"
              ))
              should_autozoom <- time_since_switch > 1.0

              if (shiny::isolate(autozoom_enabled()) && should_autozoom) {
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
      shiny::req(live_update_enabled(), is_dragging())
      coords_data <- active_coords()
      trip_pts <- coords_data$trip

      # Safer extraction using vapply
      lons <- tryCatch(
        unname(vapply(trip_pts, function(p) as.numeric(p$lng), numeric(1))),
        error = function(e) numeric(0)
      )
      lats <- tryCatch(
        unname(vapply(trip_pts, function(p) as.numeric(p$lat), numeric(1))),
        error = function(e) numeric(0)
      )

      shiny::req(length(lons) >= 2, length(lats) == length(lons))

      # Ensure data frame is as clean as possible
      pts_df <- data.frame(lon = lons, lat = lats)
      rownames(pts_df) <- NULL

      if (debug) {
        debug_msg("Trip (Live) request points:")
        print(pts_df)
      }

      # 1. Calculate trip using direct HTTP API
      trip_result_data <- api_fetch_trip(pts_df, debug = debug)

      if (!is.null(trip_result_data) && !is.null(trip_result_data$trip)) {
        trip_geom <- trip_result_data$trip

        summary <- trip_result_data$summary
        dur <- if (!is.null(summary$duration)) {
          as.numeric(summary$duration)
        } else {
          0
        }
        dis <- if (!is.null(summary$distance)) {
          as.numeric(summary$distance)
        } else {
          0
        }
        route_summary(list(duration = dur, distance = dis))

        # 2. Render on map (with fallback if set_source fails)
        tryCatch(
          {
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
          },
          error = function(e) {
            if (debug) {
              debug_msg(
                "Trip (Live) map update error: ",
                e$message,
                " -- resetting layer"
              )
            }
            tryCatch(
              {
                proxy <- mapgl::maplibre_proxy("map")
                init$trip <- FALSE
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
              },
              error = function(e2) NULL
            )
          }
        )
      }
    })

    # Trip Calculation: Stable Updates
    shiny::observe({
      shiny::req(!is.null(locations$trip))
      trip_pts <- locations$trip

      # Safer extraction using vapply
      lons <- tryCatch(
        unname(vapply(trip_pts, function(p) as.numeric(p$lng), numeric(1))),
        error = function(e) numeric(0)
      )
      lats <- tryCatch(
        unname(vapply(trip_pts, function(p) as.numeric(p$lat), numeric(1))),
        error = function(e) numeric(0)
      )

      shiny::req(length(lons) >= 2, length(lats) == length(lons))

      # Ensure data frame is as clean as possible
      pts_df <- data.frame(lon = lons, lat = lats)
      rownames(pts_df) <- NULL

      calc_trip <- function() {
        local_lons <- lons
        local_lats <- lats

        if (debug) {
          debug_msg("Trip (Stable) request points:")
          print(data.frame(lon = local_lons, lat = local_lats))
          debug_msg(
            "  class(local_lons)=",
            class(local_lons),
            " length=",
            length(local_lons)
          )
          debug_msg(
            "  class(local_lats)=",
            class(local_lats),
            " length=",
            length(local_lats)
          )
          debug_msg("  osrm.server=", getOption("osrm.server"))
          debug_msg("  osrm.profile=", getOption("osrm.profile"))
          debug_msg("  init$trip=", init$trip)
        }

        # Create the data.frame outside the tryCatch for cleaner debugging
        trip_df <- data.frame(
          lon = as.numeric(local_lons),
          lat = as.numeric(local_lats)
        )
        rownames(trip_df) <- NULL

        if (debug) {
          debug_msg("  trip_df created successfully, calling api_fetch_trip...")
        }

        # 1. Calculate trip using direct HTTP API (bypasses osrm::osrmTrip issues)
        trip_result_data <- api_fetch_trip(trip_df, debug = debug)

        if (is.null(trip_result_data) || is.null(trip_result_data$trip)) {
          shiny::showNotification(
            "Trip failed: No route returned (Is OSRM Trip service enabled? Are points within map coverage?)",
            type = "error"
          )
          return()
        }

        trip_geom <- trip_result_data$trip
        trip_result(trip_result_data)

        summary <- trip_result_data$summary
        dur <- if (!is.null(summary$duration)) {
          as.numeric(summary$duration)
        } else {
          0
        }
        dis <- if (!is.null(summary$distance)) {
          as.numeric(summary$distance)
        } else {
          0
        }
        route_summary(list(duration = dur, distance = dis))

        # 2. Render on map (with fallback if set_source fails)
        tryCatch(
          {
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
            # Check if enough time has passed since mode switch
            time_since_switch <- as.numeric(difftime(
              Sys.time(),
              last_mode_switch(),
              units = "secs"
            ))
            should_autozoom <- time_since_switch > 1.0

            if (shiny::isolate(autozoom_enabled()) && should_autozoom) {
              pts_sf <- sf::st_as_sf(
                data.frame(
                  lon = as.numeric(local_lons),
                  lat = as.numeric(local_lats)
                ),
                coords = c("lon", "lat"),
                crs = 4326
              )
              combined_sf <- rbind(
                sf::st_sf(geometry = sf::st_geometry(trip_geom)),
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
          },
          error = function(e) {
            if (debug) {
              debug_msg(
                "Trip map update error: ",
                e$message,
                " -- resetting layer"
              )
            }
            tryCatch(
              {
                proxy <- mapgl::maplibre_proxy("map")
                init$trip <- FALSE
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
              },
              error = function(e2) {
                if (debug) {
                  debug_msg("Trip layer reset also failed: ", e2$message)
                }
              }
            )
          }
        )
      }
      shiny::withProgress(message = "Calculating Trip...", calc_trip())
    })

    # Isochrone: Live Tracking (Custom Resolution)
    shiny::observe({
      shiny::req(live_update_enabled(), is_dragging())
      coords <- active_coords()
      shiny::req(coords$iso_start)

      tryCatch(
        {
          breaks <- tryCatch(
            sort(as.numeric(unlist(strsplit(input$iso_breaks, ",")))),
            error = function(e) c(5, 10, 15)
          )
          if (length(breaks) == 0) {
            breaks <- c(5, 10, 15)
          }

          # Live resolution
          debug_msg(
            "Isochrone (Live) request: ",
            coords$iso_start$lng,
            ",",
            coords$iso_start$lat,
            " breaks: ",
            paste(breaks, collapse = ",")
          )
          n_vals <- c(100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000)
          n_val <- tryCatch(
            n_vals[as.integer(input$iso_live_res)],
            error = function(e) 200
          )
          if (is.na(n_val)) {
            n_val <- 200
          }

          iso <- osrm::osrmIsochrone(
            loc = c(coords$iso_start$lng, coords$iso_start$lat),
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
            }
          }
        },
        error = function(e) NULL
      )
    })

    # Isochrone: Stable Updates
    shiny::observeEvent(
      list(locations$iso_start, input$iso_breaks, input$iso_res),
      {
        shiny::req(locations$iso_start)
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
              n_val <- tryCatch(
                n_vals[as.integer(input$iso_res)],
                error = function(e) 500
              )
              if (is.na(n_val)) {
                n_val <- 500
              }

              debug_msg(
                "Isochrone (Stable) request: ",
                locations$iso_start$lng,
                ",",
                locations$iso_start$lat,
                " breaks: ",
                paste(breaks, collapse = ","),
                " n: ",
                n_val
              )
              iso <- osrm::osrmIsochrone(
                loc = c(locations$iso_start$lng, locations$iso_start$lat),
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
                # Check if enough time has passed since mode switch
                time_since_switch <- as.numeric(difftime(
                  Sys.time(),
                  last_mode_switch(),
                  units = "secs"
                ))
                should_autozoom <- time_since_switch > 1.0

                if (shiny::isolate(autozoom_enabled()) && should_autozoom) {
                  map_width <- session$clientData$output_map_width %||% 1000
                  padding <- if (map_width < 768) 20 else 50
                  mapgl::fit_bounds(
                    proxy,
                    iso,
                    animate = TRUE,
                    padding = padding
                  )
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
      }
    )

    # --- Table Output ---
    output$itinerary_table <- DT::renderDataTable({
      shiny::req(input$mode == "route", locations$start, locations$end)
      res <- api_fetch_route_detailed(
        locations$start,
        locations$end,
        debug = debug
      )

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
