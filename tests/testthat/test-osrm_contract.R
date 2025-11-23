test_that("osrm_contract runs osrm-contract with expected arguments", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osrm <- file.path(
    tmp_dir,
    paste0("osrm-contract-test-", Sys.getpid(), ".osrm.timestamp")
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
    # Create the expected output file to satisfy the check
    hsgr_file <- sub("\\.timestamp$", ".hsgr", input_osrm)
    file.create(hsgr_file)
    list(status = 0, stdout = "", stderr = "")
  }

  result <- with_mocked_bindings(
    osrm_contract(
      input_osrm = input_osrm,
      threads = 4L,
      verbosity = "DEBUG",
      segment_speed_file = "speeds.csv",
      turn_penalty_file = "penalties.csv",
      edge_weight_updates_over_factor = 1.5,
      parse_conditionals_from_now = 1234567890,
      time_zone_file = "zones.geojson",
      quiet = FALSE,
      verbose = TRUE,
      spinner = FALSE,
      echo_cmd = TRUE
    ),
    run = mock_run,
    .package = "processx"
  )

  expect_equal(captured$command, "osrm-contract")
  expect_true("-t" %in% captured$args && "4" %in% captured$args)
  expect_true("-l" %in% captured$args && "DEBUG" %in% captured$args)
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
      "1.5" %in% captured$args
  )
  expect_true(
    "--parse-conditionals-from-now" %in%
      captured$args &&
      "1234567890" %in% captured$args
  )
  expect_true(
    "--time-zone-file" %in% captured$args && "zones.geojson" %in% captured$args
  )

  # Check processx options
  expect_true(captured$echo)
  expect_false(captured$spinner)
  expect_true(captured$echo_cmd)
})

test_that("osrm_contract handles missing input file", {
  expect_error(
    osrm_contract("nonexistent.osrm.timestamp"),
    "File does not exist"
  )
})

test_that("osrm_contract checks for output file", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osrm <- file.path(
    tmp_dir,
    paste0("osrm-fail-", Sys.getpid(), ".osrm.timestamp")
  )
  on.exit(unlink(input_osrm), add = TRUE)
  file.create(input_osrm)

  mock_run <- function(...) {
    list(status = 0, stdout = "", stderr = "")
  }

  expect_error(
    with_mocked_bindings(
      osrm_contract(input_osrm),
      run = mock_run,
      .package = "processx"
    ),
    "Contracting did not produce CH hierarchy file"
  )
})
