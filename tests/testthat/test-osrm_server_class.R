test_that("osrm_server class and metadata are correctly assigned", {
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

  # Create a fake executable path to bypass osrm_start_server's Sys.which check.
  # We use a real file to avoid complex Sys.which mocking across platforms.
  fake_osrm_bin <- file.path(tmp_dir, if (.Platform$OS.type == "windows") "osrm-routed.exe" else "osrm-routed")
  file.create(fake_osrm_bin)
  if (.Platform$OS.type != "windows") Sys.chmod(fake_osrm_bin, mode = "0755")
  
  # Ensure the function uses our fake binary
  old_exec <- options(osrm.routed.exec = fake_osrm_bin)
  on.exit({
    options(old_exec)
    unlink(fake_osrm_bin)
  }, add = TRUE)

  # Mock processx::process
  MockProcess <- list(
    new = function(...) {
      structure(
        list(
          is_alive = function() TRUE,
          get_pid = function() 12345,
          kill = function() TRUE,
          wait = function(...) TRUE
        ),
        class = c("process", "list")
      )
    }
  )

  state_env <- asNamespace("osrm.backend")$.osrm_state
  orig_reg <- state_env$registry
  on.exit(state_env$registry <- orig_reg, add = TRUE)
  state_env$registry <- list()

  with_mocked_bindings(
    {
      server <- osrm_start_server(
        osrm_path = osrm_path,
        algorithm = "MLD",
        port = 5002L,
        quiet = TRUE
      )
    },
    process = MockProcess,
    .package = "processx"
  )

  expect_s3_class(server, "osrm_server")
  expect_s3_class(server, "process")
  
  meta <- attr(server, "osrm_metadata")
  expect_equal(meta$port, 5002L)
  expect_equal(meta$algorithm, "MLD")
  expect_equal(meta$path, osrm_path)
  
  # Check print method
  expect_output(print(server), "OSRM Server \\(Job Process\\)")
  expect_output(print(server), "Status:\\s+Running")
  expect_output(print(server), "Port:\\s+5002")
  
  # Verify inheritance (process methods still work)
  expect_true(server$is_alive())
  expect_equal(server$get_pid(), 12345)
})

test_that("osrm_server print method handles stopped state", {
  server <- structure(
    list(
      is_alive = function() FALSE,
      get_pid = function() 12345
    ),
    class = c("osrm_server", "process", "list"),
    osrm_metadata = list(
      port = 5001L,
      profile = "car",
      algorithm = "MLD",
      path = "test.osrm.mldgr"
    )
  )
  
  expect_output(print(server), "Status:\\s+Stopped")
  # PID should NOT be printed if stopped (based on our print logic)
  out <- capture.output(print(server))
  expect_false(any(grepl("PID", out)))
})

test_that("osrm_stop works with osrm_server object", {
  skip_if_not_installed("processx")
  
  # Create a mock that tracks if kill was called
  killed <- FALSE
  server <- structure(
    list(
      is_alive = function() TRUE,
      get_pid = function() 12345,
      kill = function() { killed <<- TRUE; TRUE },
      wait = function(...) TRUE
    ),
    class = c("osrm_server", "process", "list")
  )
  
  # Mock the registry state so osrm_stop can find it if needed
  state_env <- asNamespace("osrm.backend")$.osrm_state
  orig_reg <- state_env$registry
  on.exit(state_env$registry <- orig_reg, add = TRUE)
  
  state_env$registry <- list(
    "test" = list(id = "test", pid = 12345, port = 5001, proc = server)
  )
  
  osrm_stop(server = server, quiet = TRUE)
  
  expect_true(killed)
})
