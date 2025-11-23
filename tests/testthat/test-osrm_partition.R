test_that("osrm_partition runs osrm-partition with expected arguments", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osrm <- file.path(
    tmp_dir,
    paste0("osrm-partition-test-", Sys.getpid(), ".osrm")
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
    partition_file <- paste0(input_osrm, ".partition")
    file.create(partition_file)
    list(status = 0, stdout = "", stderr = "")
  }

  result <- with_mocked_bindings(
    osrm_partition(
      input_osrm = input_osrm,
      threads = 6L,
      verbosity = "ERROR",
      balance = 1.5,
      boundary = 0.3,
      optimizing_cuts = 20L,
      small_component_size = 500L,
      max_cell_sizes = c(100, 1000),
      quiet = FALSE,
      verbose = TRUE,
      spinner = TRUE,
      echo_cmd = TRUE
    ),
    run = mock_run,
    .package = "processx"
  )

  expect_equal(captured$command, "osrm-partition")
  expect_true("-t" %in% captured$args && "6" %in% captured$args)
  expect_true("-l" %in% captured$args && "ERROR" %in% captured$args)
  expect_true("--balance" %in% captured$args && "1.5" %in% captured$args)
  expect_true("--boundary" %in% captured$args && "0.3" %in% captured$args)
  expect_true("--optimizing-cuts" %in% captured$args && "20" %in% captured$args)
  expect_true(
    "--small-component-size" %in% captured$args && "500" %in% captured$args
  )
  expect_true(
    "--max-cell-sizes" %in% captured$args && "100,1000" %in% captured$args
  )
})

test_that("osrm_partition handles missing input file", {
  expect_error(
    osrm_partition("nonexistent.osrm"),
    "File does not exist"
  )
})

test_that("osrm_partition checks for output file", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osrm <- file.path(
    tmp_dir,
    paste0("osrm-fail-", Sys.getpid(), ".osrm")
  )
  on.exit(unlink(input_osrm), add = TRUE)
  file.create(input_osrm)

  mock_run <- function(...) {
    list(status = 0, stdout = "", stderr = "")
  }

  expect_error(
    with_mocked_bindings(
      osrm_partition(input_osrm),
      run = mock_run,
      .package = "processx"
    ),
    "Partitioning did not produce partition file"
  )
})
