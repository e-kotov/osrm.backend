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

test_that("osrm_partition accepts a directory with one .osrm.timestamp file", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-partition-dir-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  input_osrm <- file.path(tmp_dir, "test.osrm.timestamp")
  file.create(input_osrm)

  normalized_input <- normalizePath(input_osrm)
  expected_partition <- sub("\\.timestamp$", ".partition", normalized_input)

  mock_run <- function(command, args, echo, spinner, echo_cmd, ...) {
    file.create(expected_partition)
    list(status = 0, stdout = "", stderr = "")
  }

  result <- with_mocked_bindings(
    osrm_partition(
      input_osrm = tmp_dir,
      quiet = TRUE
    ),
    run = mock_run,
    .package = "processx"
  )

  expect_equal(result$osrm_path, expected_partition)
})

test_that("osrm_partition errors when directory has no .osrm.timestamp files", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-partition-empty-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  expect_error(
    osrm_partition(input_osrm = tmp_dir),
    "No .osrm.timestamp files found"
  )
})

test_that("osrm_partition errors when directory has multiple .osrm.timestamp files", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-partition-multi-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file.create(file.path(tmp_dir, "test1.osrm.timestamp"))
  file.create(file.path(tmp_dir, "test2.osrm.timestamp"))

  expect_error(
    osrm_partition(input_osrm = tmp_dir),
    "Multiple .osrm.timestamp files found"
  )
})
