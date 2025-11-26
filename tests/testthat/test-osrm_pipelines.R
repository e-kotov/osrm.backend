test_that("CH pipeline: extract -> contract works correctly", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-ch-pipeline-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  input_osm_path <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm_path)

  # Mock processx::run to create the expected files
  mock_run <- function(command, args, ...) {
    # The first arg is the input path
    input_path <- args[1]

    if (command == "osrm-extract") {
      # Input is test.osm.pbf, create test.osrm.timestamp
      base <- sub("\\.osm\\.pbf$", "", input_osm_path)
      timestamp_file <- paste0(base, ".osrm.timestamp")
      file.create(timestamp_file)
    } else if (command == "osrm-contract") {
      # Input is test.osrm (without extension), create test.osrm.hsgr
      hsgr_file <- paste0(input_path, ".hsgr")
      file.create(hsgr_file)
    }
    list(status = 0, stdout = "", stderr = "")
  }

  # Test the pipeline
  result <- with_mocked_bindings(
    {
      extract_result <- osrm_extract(
        input_osm_path,
        profile = "car.lua",
        quiet = TRUE
      )
      osrm_contract(extract_result, quiet = TRUE)
    },
    run = mock_run,
    .package = "processx"
  )

  # Verify the result
  expect_s3_class(result, "osrm_job")
  expect_true(grepl("\\.hsgr$", result$osrm_job_artifact))
  expect_true(file.exists(result$osrm_job_artifact))
  expect_named(result$logs, c("extract", "contract"))
})

test_that("MLD pipeline: extract -> partition -> customize works correctly", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-mld-pipeline-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  input_osm_path <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm_path)

  mock_run <- function(command, args, ...) {
    # The first arg is the input path
    input_path <- args[1]

    if (command == "osrm-extract") {
      base <- sub("\\.osm\\.pbf$", "", input_osm_path)
      timestamp_file <- paste0(base, ".osrm.timestamp")
      file.create(timestamp_file)
    } else if (command == "osrm-partition") {
      partition_file <- paste0(input_path, ".partition")
      file.create(partition_file)
    } else if (command == "osrm-customize") {
      mldgr_file <- paste0(input_path, ".mldgr")
      file.create(mldgr_file)
    }
    list(status = 0, stdout = "", stderr = "")
  }

  result <- with_mocked_bindings(
    {
      extract_result <- osrm_extract(
        input_osm_path,
        profile = "car.lua",
        quiet = TRUE
      )
      partition_result <- osrm_partition(extract_result, quiet = TRUE)
      osrm_customize(partition_result, quiet = TRUE)
    },
    run = mock_run,
    .package = "processx"
  )

  # Verify the result
  expect_s3_class(result, "osrm_job")
  expect_true(grepl("\\.mldgr$", result$osrm_job_artifact))
  expect_true(file.exists(result$osrm_job_artifact))
  expect_named(result$logs, c("extract", "partition", "customize"))
})

test_that("Mixed pipelines fail with helpful errors: extract -> partition -> contract", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-mixed-pipeline-1-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  input_osm_path <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm_path)

  mock_run <- function(command, args, ...) {
    input_path <- args[1]

    if (command == "osrm-extract") {
      base <- sub("\\.osm\\.pbf$", "", input_osm_path)
      timestamp_file <- paste0(base, ".osrm.timestamp")
      file.create(timestamp_file)
    } else if (command == "osrm-partition") {
      partition_file <- paste0(input_path, ".partition")
      file.create(partition_file)
    }
    list(status = 0, stdout = "", stderr = "")
  }

  expect_error(
    with_mocked_bindings(
      {
        extract_result <- osrm_extract(
          input_osm_path,
          profile = "car.lua",
          quiet = TRUE
        )
        partition_result <- osrm_partition(extract_result, quiet = TRUE)
        osrm_contract(partition_result, quiet = TRUE)
      },
      run = mock_run,
      .package = "processx"
    ),
    "cannot be used after.*partition"
  )
})

test_that("Mixed pipelines fail with helpful errors: extract -> contract -> partition", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-mixed-pipeline-2-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  input_osm_path <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm_path)

  # Create a mock osrm_job object with .hsgr file (simulating result after contract)
  hsgr_file <- file.path(tmp_dir, "test.osrm.hsgr")
  file.create(hsgr_file)

  contract_result <- structure(
    list(
      osrm_job_artifact = hsgr_file,
      osrm_working_dir = tmp_dir,
      logs = list(extract = list(), contract = list())
    ),
    class = "osrm_job"
  )

  # Trying to partition after contract should fail
  # (partition expects a .timestamp file, not .hsgr)
  # This will fail either with a "timestamp" error or a command execution error
  expect_error(
    osrm_partition(contract_result, quiet = TRUE)
  )
})

test_that("Mixed pipelines fail with helpful errors: extract -> customize (without partition)", {
  skip_if_not_installed("processx")

  tmp_dir <- file.path(tempdir(), paste0("osrm-mixed-pipeline-3-", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  input_osm_path <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm_path)

  mock_run <- function(command, args, ...) {
    if (command == "osrm-extract") {
      base <- sub("\\.osm\\.pbf$", "", input_osm_path)
      timestamp_file <- paste0(base, ".osrm.timestamp")
      file.create(timestamp_file)
    }
    list(status = 0, stdout = "", stderr = "")
  }

  expect_error(
    with_mocked_bindings(
      {
        extract_result <- osrm_extract(
          input_osm_path,
          profile = "car.lua",
          quiet = TRUE
        )
        osrm_customize(extract_result, quiet = TRUE)
      },
      run = mock_run,
      .package = "processx"
    ),
    "requires a partitioned graph"
  )
})
