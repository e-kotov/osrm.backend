test_that("gui_parse_breaks parses input correctly", {
  expect_equal(gui_parse_breaks("5, 10, 15"), c(5, 10, 15))
  expect_equal(gui_parse_breaks(""), c(5, 10, 15))
  expect_equal(gui_parse_breaks(NULL), c(5, 10, 15))
  expect_equal(gui_parse_breaks("5"), 5)
  expect_equal(gui_parse_breaks("10, 5, 15, 15"), c(5, 10, 15))
  expect_equal(gui_parse_breaks("5, abc, 10"), c(5, 10))
  expect_equal(gui_parse_breaks("abc"), c(5, 10, 15))
  expect_equal(gui_parse_breaks(" 12 , 24 "), c(12, 24))
})

test_that("gui_resolve_map_view resolves center and zoom", {
  # Direct center and zoom
  res <- gui_resolve_map_view(center = c(10, 20), zoom = 5, input_osrm = NULL)
  expect_equal(res$center, c(10, 20))
  expect_equal(res$zoom, 5)

  # Named list for center
  res2 <- gui_resolve_map_view(center = list(lng = 10, lat = 20), zoom = 5, input_osrm = NULL)
  expect_equal(res2$center, c(10, 20))
  
  res3 <- gui_resolve_map_view(center = list(lon = 10, lat = 20), zoom = 5, input_osrm = NULL)
  expect_equal(res3$center, c(10, 20))
  
  res4 <- gui_resolve_map_view(center = list(x = 10, y = 20), zoom = 5, input_osrm = NULL)
  expect_equal(res4$center, c(10, 20))

  # Invalid center
  expect_error(gui_resolve_map_view(center = c(10), zoom = 5, input_osrm = NULL), "numeric vector of length 2")
  expect_warning(
    expect_error(gui_resolve_map_view(center = c("a", "b"), zoom = 5, input_osrm = NULL), "numeric vector of length 2"),
    "NAs introduced by coercion"
  )
})

test_that("gui_setup_server error handling works", {
  expect_error(gui_setup_server(NULL, port = "invalid"), "Invalid port specified")
  expect_error(gui_setup_server(NULL, port = -5), "Invalid port specified")
  
  # Mock dead process
  dummy_proc <- structure(list(is_alive = function() FALSE), class = "process")
  expect_error(gui_setup_server(dummy_proc, port = 5000), "The provided OSRM server process is not running")

  # Mock alive process with auto port
  dummy_proc2 <- structure(list(is_alive = function() TRUE), class = "process")
  expect_error(gui_setup_server(dummy_proc2, port = "auto"), "specify the port explicitly")
})

test_that("gui_check_dependencies checks required packages", {
  skip_if_not_installed("osrm")
  skip_if_not_installed("DT")
  skip_if_not_installed("shiny")
  skip_if_not_installed("mapgl")
  skip_if_not_installed("sf")
  skip_if_not_installed("viridisLite")
  
  expect_silent(gui_check_dependencies())
})

test_that("osrm_gui prepares arguments and handles missing servers", {
  skip_if_not_installed("osrm")
  skip_if_not_installed("DT")
  skip_if_not_installed("shiny")
  skip_if_not_installed("mapgl")
  skip_if_not_installed("sf")
  skip_if_not_installed("viridisLite")
  
  # When no servers are running, port='auto' throws error
  local_mocked_bindings(
    osrm_servers = function() {
      data.frame(
        name = character(),
        port = integer(),
        pid = integer(),
        alive = logical(),
        input_osm = character(),
        started_at = .POSIXct(double(0)),
        stringsAsFactors = FALSE
      )
    }
  )
  
  expect_error(osrm_gui(), "No running OSRM servers detected")
})