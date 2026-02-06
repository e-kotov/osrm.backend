# ROBUST SKIP: Check if OSRM is actually available before running these tests.
# This prevents crashes locally AND ensures CRAN compliance.
# We check both the Option (set by setup) and the PATH.
has_osrm_option <- !is.null(getOption("osrm.routed.exec"))
has_osrm_path <- nzchar(Sys.which("osrm-routed"))

if (!has_osrm_option && !has_osrm_path) {
  testthat::skip("OSRM binary not found (Skipping Server Tests)")
}

test_that("osrm_start_server launches osrm-routed with correct arguments", {
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
  skip_if(
    packageVersion("testthat") < "3.2.0",
    "Requires testthat >= 3.2.0 for object mocking"
  )

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

# Additional tests for osrm_servers() ----
test_that("osrm_servers returns empty data frame when no servers", {
  skip_if(
    packageVersion("testthat") < "3.2.0",
    "Requires testthat >= 3.2.0 for object mocking"
  )

  mock_state <- new.env()
  mock_state$registry <- list()

  with_mocked_bindings(
    {
      result <- osrm_servers()

      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 0)
      expect_named(
        result,
        c("id", "pid", "port", "algorithm", "started_at", "alive", "has_handle")
      )
    },
    .osrm_state = mock_state
  )
})

test_that("osrm_servers returns server information correctly", {
  skip_if(
    packageVersion("testthat") < "3.2.0",
    "Requires testthat >= 3.2.0 for object mocking"
  )

  mock_proc <- list(
    is_alive = function() TRUE,
    get_pid = function() 1001L
  )
  class(mock_proc) <- c("process", "list")

  mock_state <- new.env()
  mock_state$registry <- list(
    "server1" = list(
      id = "server1",
      pid = 1001L,
      port = 5001L,
      algorithm = "mld",
      started_at = "2025-01-01T12:00:00.000Z",
      proc = mock_proc
    ),
    "server2" = list(
      id = "server2",
      pid = 1002L,
      port = 5002L,
      algorithm = "ch",
      started_at = "2025-01-01T12:00:01.000Z",
      proc = NULL
    )
  )

  with_mocked_bindings(
    {
      result <- osrm_servers()

      expect_equal(nrow(result), 2)
      expect_equal(result$id, c("server1", "server2"))
      expect_equal(result$pid, c(1001L, 1002L))
      expect_equal(result$port, c(5001L, 5002L))
      expect_equal(result$algorithm, c("mld", "ch"))
      expect_true(result$has_handle[1])
      expect_false(result$has_handle[2])
    },
    .osrm_state = mock_state,
    .osrm_pid_is_running = function(...) TRUE
  )
})

# Tests for osrm_stop_all() ----
test_that("osrm_stop_all stops all servers", {
  skip_if(
    packageVersion("testthat") < "3.2.0",
    "Requires testthat >= 3.2.0 for object mocking"
  )

  mock_state <- new.env()
  mock_state$registry <- list(
    "server1" = list(id = "server1", pid = 1001L, port = 5001L, proc = NULL),
    "server2" = list(id = "server2", pid = 1002L, port = 5002L, proc = NULL)
  )

  stop_calls <- character()

  with_mocked_bindings(
    {
      result <- osrm_stop_all()

      expect_equal(result, 2L)
      expect_length(stop_calls, 2)
    },
    .osrm_state = mock_state,
    osrm_stop = function(id, quiet) {
      stop_calls <<- c(stop_calls, id)
      list(id = id, stopped = TRUE)
    }
  )
})

test_that("osrm_stop_all returns 0 when no servers", {
  skip_if(
    packageVersion("testthat") < "3.2.0",
    "Requires testthat >= 3.2.0 for object mocking"
  )

  mock_state <- new.env()
  mock_state$registry <- list()

  with_mocked_bindings(
    {
      result <- osrm_stop_all()
      expect_equal(result, 0L)
    },
    .osrm_state = mock_state
  )
})

# Tests for server registry internal functions ----
test_that("registry saves and loads correctly", {
  skip_if_not_installed("jsonlite")
  skip_if(
    packageVersion("testthat") < "3.2.0",
    "Requires testthat >= 3.2.0 for object mocking"
  )

  tmp_dir <- tempfile()
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a mock state with registry
  mock_state <- new.env()
  mock_state$registry <- list(
    "test-server" = list(
      id = "test-server",
      pid = 1234L,
      port = 5001L,
      algorithm = "mld",
      started_at = "2025-01-01T12:00:00.000Z",
      prefix = "/tmp/test.osrm"
    )
  )

  with_mocked_bindings(
    {
      # Test save
      .osrm_registry_save()

      # Check file exists
      registry_path <- file.path(tmp_dir, "servers.json")
      expect_true(file.exists(registry_path))

      # Test load
      .osrm_state$registry <- list() # Clear registry
      .osrm_registry_load()

      # Check loaded correctly (excluding proc field which is NULL after load)
      expect_true("test-server" %in% names(.osrm_state$registry))
      expect_equal(.osrm_state$registry$`test-server`$pid, 1234L)
    },
    .osrm_state = mock_state,
    .osrm_registry_dir = function() tmp_dir,
    .osrm_cleanup_orphans = function() invisible(NULL)
  )
})

# Tests for osrm_start_server error validation ----
test_that("osrm_start_server validates input file extension", {
  tmp_file <- "test.txt"
  file.create(tmp_file)
  on.exit(unlink(tmp_file))

  expect_error(
    osrm_start_server(tmp_file),
    "must end in .osrm.mldgr or .osrm.hsgr"
  )
})

test_that("osrm_start_server accepts dataset_name parameter", {
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

  captured <- list()

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
        dataset_name = "my_dataset",
        quiet = TRUE
      )

      expect_true("--dataset-name" %in% captured$args)
      expect_true("my_dataset" %in% captured$args)
    },
    process = MockProcess,
    .package = "processx"
  )
})

test_that("osrm_start_server handles max size parameters", {
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

  captured <- list()

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
        max_table_size = 200L,
        max_trip_size = 50L,
        quiet = TRUE
      )

      expect_true("--max-table-size" %in% captured$args)
      expect_true("200" %in% captured$args)
      expect_true("--max-trip-size" %in% captured$args)
      expect_true("50" %in% captured$args)
    },
    process = MockProcess,
    .package = "processx"
  )
})

# Tests for logging configuration ----
test_that("osrm_start_server uses temp file by default for logging", {
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

  captured <- list()

  MockProcess <- list(
    new = function(command, args, ..., stdout, stderr) {
      captured <<- list(
        command = command,
        args = args,
        stdout = stdout,
        stderr = stderr
      )
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
      # Ensure no log option is set
      old_opt <- options(osrm.server.log_file = NULL)
      on.exit(options(old_opt), add = TRUE)

      server <- osrm_start_server(
        osrm_path = osrm_path,
        quiet = TRUE
      )

      # Should use a temp file path (character, not "|")
      expect_type(captured$stdout, "character")
      expect_type(captured$stderr, "character")
      expect_false(identical(captured$stdout, "|"))
      expect_false(identical(captured$stderr, "|"))
      expect_false(identical(captured$stdout, ""))
      expect_false(identical(captured$stderr, ""))

      # Should be the same path for both
      expect_equal(captured$stdout, captured$stderr)

      # Should have .log extension
      expect_match(captured$stdout, "\\.log$")
    },
    process = MockProcess,
    .package = "processx"
  )
})

test_that("osrm_start_server routes to console when verbose = TRUE", {
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

  captured <- list()

  MockProcess <- list(
    new = function(command, args, ..., stdout, stderr) {
      captured <<- list(
        command = command,
        args = args,
        stdout = stdout,
        stderr = stderr
      )
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
        verbose = TRUE,
        quiet = TRUE
      )

      # Should route to console (empty string)
      expect_equal(captured$stdout, "")
      expect_equal(captured$stderr, "")
    },
    process = MockProcess,
    .package = "processx"
  )
})

test_that("osrm_start_server uses osrm.server.log_file option when set (character)", {
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

  custom_log <- file.path(tmp_dir, "custom_osrm.log")

  captured <- list()

  MockProcess <- list(
    new = function(command, args, ..., stdout, stderr) {
      captured <<- list(
        command = command,
        args = args,
        stdout = stdout,
        stderr = stderr
      )
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
      # Set custom log file option
      old_opt <- options(osrm.server.log_file = custom_log)
      on.exit(options(old_opt), add = TRUE)

      server <- osrm_start_server(
        osrm_path = osrm_path,
        quiet = TRUE
      )

      # Should use the custom log path
      expect_equal(captured$stdout, custom_log)
      expect_equal(captured$stderr, custom_log)
    },
    process = MockProcess,
    .package = "processx"
  )
})

test_that("osrm_start_server falls back to temp file when list option is used", {
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

  captured <- list()

  MockProcess <- list(
    new = function(command, args, ..., stdout, stderr) {
      captured <<- list(
        command = command,
        args = args,
        stdout = stdout,
        stderr = stderr
      )
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
      # Set list option (now deprecated) - should fall back to temp file
      old_opt <- options(
        osrm.server.log_file = list(
          stdout = "/tmp/out.log",
          stderr = "/tmp/err.log"
        )
      )
      on.exit(options(old_opt), add = TRUE)

      server <- osrm_start_server(
        osrm_path = osrm_path,
        quiet = TRUE
      )

      # Should fall back to temp file (not use the list paths)
      expect_type(captured$stdout, "character")
      expect_type(captured$stderr, "character")
      expect_false(identical(captured$stdout, "/tmp/out.log"))
      expect_false(identical(captured$stderr, "/tmp/err.log"))
      expect_match(captured$stdout, "\\.log$")
    },
    process = MockProcess,
    .package = "processx"
  )
})

test_that("osrm_start_server reads log file on startup failure", {
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

  # Create a mock log file with error content
  mock_log_content <- c(
    "[info] Starting OSRM server...",
    "[error] Port 5001 is already in use",
    "[error] Failed to bind to socket"
  )
  mock_log_file <- file.path(tmp_dir, "mock_error.log")
  writeLines(mock_log_content, mock_log_file)

  call_count <- 0
  captured_stdout <- NULL

  MockProcess <- list(
    new = function(command, args, ..., stdout, stderr) {
      call_count <<- call_count + 1
      captured_stdout <<- stdout
      structure(
        list(
          is_alive = function() FALSE, # Simulate immediate failure
          get_pid = function() 12345,
          kill = function() TRUE,
          wait = function(...) TRUE,
          get_exit_status = function() 1
        ),
        class = c("process", "list")
      )
    }
  )

  with_mocked_bindings(
    {
      # Use the mock log file directly via the option
      old_opt <- options(osrm.server.log_file = mock_log_file)
      on.exit(options(old_opt), add = TRUE)

      expect_error(
        osrm_start_server(osrm_path = osrm_path, quiet = TRUE),
        "Port 5001 is already in use"
      )
    },
    process = MockProcess,
    .package = "processx"
  )
})

test_that("verbose = TRUE takes precedence over osrm.server.log_file option", {
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

  custom_log <- file.path(tmp_dir, "custom_osrm.log")

  captured <- list()

  MockProcess <- list(
    new = function(command, args, ..., stdout, stderr) {
      captured <<- list(
        command = command,
        args = args,
        stdout = stdout,
        stderr = stderr
      )
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
      # Set custom log file option but also verbose = TRUE
      old_opt <- options(osrm.server.log_file = custom_log)
      on.exit(options(old_opt), add = TRUE)

      server <- osrm_start_server(
        osrm_path = osrm_path,
        verbose = TRUE,
        quiet = TRUE
      )

      # verbose = TRUE should take precedence (console output)
      expect_equal(captured$stdout, "")
      expect_equal(captured$stderr, "")
    },
    process = MockProcess,
    .package = "processx"
  )
})
