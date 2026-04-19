# ROBUST SKIP: Check if OSRM is actually available before running these tests.
osrm_exec_opt <- getOption("osrm.routed.exec", "")
has_osrm_option <- nzchar(osrm_exec_opt) && nzchar(Sys.which(osrm_exec_opt))
has_osrm_path <- nzchar(Sys.which("osrm-routed"))

if (!has_osrm_option && !has_osrm_path) {
  testthat::skip("OSRM binary not found (Skipping Server Additions Tests)")
}

test_that("osrm_start_server triggers hint on version incompatibility", {
  skip_if_not_installed("processx")
  skip_on_cran()
  skip_if(
    packageVersion("testthat") < "3.2.0",
    "Requires testthat >= 3.2.0 for object mocking"
  )

  tmp_dir <- tempdir()
  osrm_path <- file.path(tmp_dir, "test.osrm.mldgr")
  file.create(osrm_path)
  on.exit(unlink(osrm_path), add = TRUE)

  # Create a mock log file with the incompatibility error content
  mock_log_content <- c(
    "[info] Starting OSRM server...",
    "[error] File is incompatible with this version of OSRM: /path/to/test.osrm.cells prepared with OSRM 6.0.0 but this is v26.4.1 (at include/storage/tar.hpp:203)",
    "[info] Maximum header size: 25024"
  )
  mock_log_file <- file.path(tmp_dir, "mock_incompatible.log")
  writeLines(mock_log_content, mock_log_file)

  MockProcess <- list(
    new = function(command, args, ..., stdout, stderr) {
      structure(
        list(
          is_alive = function() FALSE, # Simulate immediate failure
          get_pid = function() 12345,
          kill = function() TRUE,
          wait = function(...) TRUE,
          get_exit_status = function() 3
        ),
        class = c("process", "list")
      )
    }
  )

  state_env <- asNamespace("osrm.backend")$.osrm_state
  orig_registry <- state_env$registry
  orig_session_id <- state_env$session_id
  on.exit({
    state_env$registry <- orig_registry
    state_env$session_id <- orig_session_id
  }, add = TRUE)

  state_env$registry <- list()
  state_env$session_id <- "test-session"

  with_mocked_bindings(
    {
      old_opt <- options(osrm.server.log_file = mock_log_file)
      on.exit(options(old_opt), add = TRUE)

      expect_error(
        osrm_start_server(osrm_path = osrm_path, quiet = TRUE),
        "Hint: The OSRM graph files are incompatible"
      )
    },
    process = MockProcess,
    .package = "processx"
  )
})

test_that("osrm_servers output='list' returns a named list of metadata", {
  skip_if(
    packageVersion("testthat") < "3.2.0",
    "Requires testthat >= 3.2.0 for object mocking"
  )

  mock_proc <- list(
    is_alive = function() TRUE,
    get_pid = function() 1001L
  )
  class(mock_proc) <- c("process", "list")

  mock_reg <- list(
    "osrm-5001-1001" = list(
      id = "osrm-5001-1001",
      pid = 1001L,
      port = 5001L,
      algorithm = "mld",
      started_at = "2025-01-01T12:00:00.000Z",
      input_osm = "data1.osm.pbf",
      center_lon = 10.5,
      center_lat = 50.2,
      proc = mock_proc
    )
  )

  # Store original registry and session id to restore later
  state_env <- asNamespace("osrm.backend")$.osrm_state
  orig_registry <- state_env$registry
  on.exit(state_env$registry <- orig_registry, add = TRUE)
  
  # Inject mock registry
  state_env$registry <- mock_reg

  with_mocked_bindings(
    {
      result_list <- osrm_servers(output = "list")

      expect_type(result_list, "list")
      expect_equal(length(result_list), 1)
      expect_named(result_list, "osrm-5001-1001")
      expect_equal(result_list[[1]]$pid, 1001L)
      expect_equal(result_list[[1]]$port, 5001L)
      expect_equal(result_list[[1]]$center_lon, 10.5)
      expect_equal(result_list[[1]]$center_lat, 50.2)
    },
    .osrm_pid_is_running = function(...) TRUE
  )
})

test_that("print.osrm_server_list produces expected console output", {
  # Create a dummy data frame with the right class and structure
  dummy_df <- data.frame(
    id = "osrm-5001-1001",
    pid = 1001L,
    port = 5001L,
    algorithm = "MLD",
    started_at = as.POSIXct("2025-01-01 12:00:00", tz = "UTC"),
    alive = TRUE,
    has_handle = TRUE,
    log = "/tmp/test.log",
    input_osm = "/tmp/data.osm.pbf",
    center_lon = 10.1234,
    center_lat = 50.5678,
    stringsAsFactors = FALSE
  )
  class(dummy_df) <- c("osrm_server_list", "data.frame")
  
  # Capture the print output
  out <- utils::capture.output(print(dummy_df))
  
  # Verify key components of Option 2 formatting are present
  out_str <- paste(out, collapse = "\n")
  expect_match(out_str, "OSRM Server \\(pid: 1001, port: 5001\\)")
  expect_match(out_str, "Status    : RUNNING")
  expect_match(out_str, "Algorithm : MLD")
  expect_match(out_str, "Center    : 10.1234, 50.5678")
  expect_match(out_str, "Input OSM : /tmp/data.osm.pbf")
  expect_match(out_str, "Log File  : /tmp/test.log")
})