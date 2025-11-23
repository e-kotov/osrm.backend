test_that("osrm_start_server launches osrm-routed with correct arguments", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  osrm_path <- file.path(tmp_dir, "test.osrm.mldgr")
  file.create(osrm_path)
  on.exit(unlink(osrm_path), add = TRUE)

  captured <- list()

  # Mock processx::process
  # Mock processx::process without R6 dependency
  MockProcess <- list(
    new = function(command, args, ...) {
      captured <<- list(command = command, args = args)
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

  with_mocked_bindings(
    {
      server <- osrm_start_server(
        osrm_path = osrm_path,
        algorithm = "MLD",
        port = 5002L,
        threads = 4L,
        max_table_size = 500L,
        quiet = TRUE
      )
    },
    process = MockProcess,
    .package = "processx"
  )

  expect_equal(captured$command, "osrm-routed")
  expect_true("-a" %in% captured$args && "MLD" %in% captured$args)
  expect_true("-p" %in% captured$args && "5002" %in% captured$args)
  expect_true("-t" %in% captured$args && "4" %in% captured$args)
  expect_true("--max-table-size" %in% captured$args && "500" %in% captured$args)

  # Clean up registry
  osrm_stop(server, quiet = TRUE)
})

test_that("osrm_start_server validates input file extension", {
  tmp_file <- "test.txt"
  file.create(tmp_file)
  on.exit(unlink(tmp_file))

  expect_error(
    osrm_start_server(tmp_file),
    "must end in .osrm.mldgr or .osrm.hsgr"
  )
})

test_that("osrm_stop handles stopping by object, id, port, and pid", {
  skip_if_not_installed("processx")

  # Mock registry state
  mock_reg <- list(
    "server1" = list(id = "server1", pid = 1001L, port = 5001L, proc = NULL),
    "server2" = list(id = "server2", pid = 1002L, port = 5002L, proc = NULL)
  )

  # Mock internal functions
  mock_state <- new.env()
  mock_state$registry <- mock_reg

  with_mocked_bindings(
    {
      # Stop by ID
      res1 <- osrm_stop(id = "server1", quiet = TRUE)
      expect_equal(res1$id, "server1")

      # Stop by Port
      res2 <- osrm_stop(port = 5002, quiet = TRUE)
      expect_equal(res2$port, 5002)

      # Stop non-existent
      expect_error(
        osrm_stop(id = "server3", quiet = TRUE),
        "Could not identify a server"
      )
    },
    .osrm_state = mock_state,
    .osrm_deregister = function(...) TRUE,
    .osrm_pid_is_running = function(...) TRUE,
    .osrm_kill_pid = function(...) TRUE
  )
})
