test_that("osrm_customize runs osrm-customize with expected arguments", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osrm <- file.path(
    tmp_dir,
    paste0("osrm-customize-test-", Sys.getpid(), ".osrm.partition")
  )
  on.exit(unlink(input_osrm), add = TRUE)
  file.create(input_osrm)

  captured <- list()
  mock_run <- function(command, args, echo, spinner, echo_cmd, ...) {
    captured <<- list(
      command = command,
      args = args,
      echo = echo,
      spinner = spinner,
      echo_cmd = echo_cmd
    )
    # Create the expected output file
    mldgr_file <- sub("\\.partition$", ".mldgr", input_osrm)
    file.create(mldgr_file)
    list(status = 0, stdout = "", stderr = "")
  }

  result <- with_mocked_bindings(
    osrm_customize(
      input_osrm = input_osrm,
      threads = 2L,
      verbosity = "WARNING",
      segment_speed_file = "speeds.csv",
      turn_penalty_file = "penalties.csv",
      edge_weight_updates_over_factor = 2.0,
      parse_conditionals_from_now = 987654321,
      time_zone_file = "zones.geojson",
      quiet = TRUE,
      verbose = FALSE,
      spinner = TRUE,
      echo_cmd = FALSE
    ),
    run = mock_run,
    .package = "processx"
  )

  expect_equal(captured$command, "osrm-customize")
  expect_true("-t" %in% captured$args && "2" %in% captured$args)
  expect_true("-l" %in% captured$args && "WARNING" %in% captured$args)
  expect_true(
    "--segment-speed-file" %in% captured$args && "speeds.csv" %in% captured$args
  )
  expect_true(
    "--turn-penalty-file" %in%
      captured$args &&
      "penalties.csv" %in% captured$args
  )
  expect_true(
    "--edge-weight-updates-over-factor" %in%
      captured$args &&
      "2" %in% captured$args
  )
  expect_true(
    "--parse-conditionals-from-now" %in%
      captured$args &&
      "987654321" %in% captured$args
  )
  expect_true(
    "--time-zone-file" %in% captured$args && "zones.geojson" %in% captured$args
  )

  # Check processx options (quiet=TRUE overrides verbose/spinner/echo_cmd)
  expect_false(captured$echo)
  expect_false(captured$spinner)
  expect_false(captured$echo_cmd)
})

test_that("osrm_customize handles missing input file", {
  expect_error(
    osrm_customize("nonexistent.osrm.partition"),
    "File does not exist"
  )
})

test_that("osrm_customize checks for output file", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osrm <- file.path(
    tmp_dir,
    paste0("osrm-fail-", Sys.getpid(), ".osrm.partition")
  )
  on.exit(unlink(input_osrm), add = TRUE)
  file.create(input_osrm)

  mock_run <- function(...) {
    list(status = 0, stdout = "", stderr = "")
  }

  expect_error(
    with_mocked_bindings(
      osrm_customize(input_osrm),
      run = mock_run,
      .package = "processx"
    ),
    "Customization did not produce MLD graph file"
  )
})

test_that("osrm_customize accepts a directory with one .osrm.partition file", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-customize-dir-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  input_osrm <- file.path(tmp_dir, "test.osrm.partition")
  file.create(input_osrm)

  normalized_input <- normalizePath(input_osrm)
  expected_mldgr <- sub("\\.partition$", ".mldgr", normalized_input)

  mock_run <- function(command, args, echo, spinner, echo_cmd, ...) {
    file.create(expected_mldgr)
    list(status = 0, stdout = "", stderr = "")
  }

  result <- with_mocked_bindings(
    osrm_customize(
      input_osrm = tmp_dir,
      quiet = TRUE
    ),
    run = mock_run,
    .package = "processx"
  )

  expect_equal(result$osrm_job_artifact, expected_mldgr)
})

test_that("osrm_customize errors when directory has no .osrm.partition files", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-customize-empty-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    osrm_customize(input_osrm = tmp_dir),
    "No .osrm.partition files found"
  )
})

test_that("osrm_customize errors when directory has multiple .osrm.partition files", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-customize-multi-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(tmp_dir, "test1.osrm.partition"))
  file.create(file.path(tmp_dir, "test2.osrm.partition"))

  expect_error(
    osrm_customize(input_osrm = tmp_dir),
    "Multiple .osrm.partition files found"
  )
})

test_that("osrm_customize gives helpful error when used after osrm_extract without partition", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  timestamp_file <- file.path(
    tmp_dir,
    paste0("osrm-extract-test-", Sys.getpid(), ".osrm.timestamp")
  )
  on.exit(unlink(timestamp_file), add = TRUE)
  file.create(timestamp_file)

  # Create a mock osrm_job object with a timestamp file (simulating osrm_extract output)
  mock_job <- structure(
    list(
      osrm_job_artifact = timestamp_file,
      osrm_working_dir = tmp_dir,
      logs = list()
    ),
    class = "osrm_job"
  )

  expect_error(
    osrm_customize(input_osrm = mock_job),
    "requires a partitioned graph"
  )
  expect_error(
    osrm_customize(input_osrm = mock_job),
    "MLD pipeline"
  )
  expect_error(
    osrm_customize(input_osrm = mock_job),
    "osrm_partition"
  )
})
