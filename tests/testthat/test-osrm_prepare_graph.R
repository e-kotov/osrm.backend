test_that("osrm_prepare_graph runs MLD pipeline correctly", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osm <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm)
  on.exit(unlink(input_osm), add = TRUE)

  # Mock osrm_extract, osrm_partition, osrm_customize
  mock_extract <- function(input_osm, ...) {
    osrm.backend:::as_osrm_job(
      osrm_job_artifact = sub("\\.osm\\.pbf$", ".osrm.timestamp", input_osm),
      osrm_working_dir = dirname(input_osm),
      logs = list(extract = list(status = 0))
    )
  }
  mock_partition <- function(input_osrm, ...) {
    # Extract path and previous logs if input is an osrm_job
    if (inherits(input_osrm, "osrm_job")) {
      prev_logs <- input_osrm$logs
      input_path <- input_osrm$osrm_job_artifact
    } else {
      prev_logs <- list()
      input_path <- input_osrm
    }
    osrm.backend:::as_osrm_job(
      osrm_job_artifact = paste0(sub("\\.timestamp$", "", input_path), ".osrm.partition"),
      osrm_working_dir = dirname(input_path),
      logs = c(prev_logs, list(partition = list(status = 0)))
    )
  }
  mock_customize <- function(input_osrm, ...) {
    # Extract path and previous logs if input is an osrm_job
    if (inherits(input_osrm, "osrm_job")) {
      prev_logs <- input_osrm$logs
      input_path <- input_osrm$osrm_job_artifact
    } else {
      prev_logs <- list()
      input_path <- input_osrm
    }
    osrm.backend:::as_osrm_job(
      osrm_job_artifact = sub("\\.partition$", ".mldgr", input_path),
      osrm_working_dir = dirname(input_path),
      logs = c(prev_logs, list(customize = list(status = 0)))
    )
  }

  result <- with_mocked_bindings(
    osrm_prepare_graph(
      input_osm = input_osm,
      algorithm = "mld",
      quiet = TRUE
    ),
    osrm_extract = mock_extract,
    osrm_partition = mock_partition,
    osrm_customize = mock_customize
  )

  expect_true(grepl("\\.mldgr$", result$osrm_job_artifact))
  expect_named(result$logs, c("extract", "partition", "customize"))
})

test_that("osrm_prepare_graph runs CH pipeline correctly", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osm <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm)
  on.exit(unlink(input_osm), add = TRUE)

  mock_extract <- function(input_osm, ...) {
    osrm.backend:::as_osrm_job(
      osrm_job_artifact = sub("\\.osm\\.pbf$", ".osrm.timestamp", input_osm),
      osrm_working_dir = dirname(input_osm),
      logs = list(extract = list(status = 0))
    )
  }
  mock_contract <- function(input_osrm, ...) {
    # Extract path and previous logs if input is an osrm_job
    if (inherits(input_osrm, "osrm_job")) {
      prev_logs <- input_osrm$logs
      input_path <- input_osrm$osrm_job_artifact
    } else {
      prev_logs <- list()
      input_path <- input_osrm
    }
    osrm.backend:::as_osrm_job(
      osrm_job_artifact = sub("\\.timestamp$", ".hsgr", input_path),
      osrm_working_dir = dirname(input_path),
      logs = c(prev_logs, list(contract = list(status = 0)))
    )
  }

  result <- with_mocked_bindings(
    osrm_prepare_graph(
      input_osm = input_osm,
      algorithm = "ch",
      quiet = TRUE
    ),
    osrm_extract = mock_extract,
    osrm_contract = mock_contract
  )

  expect_true(grepl("\\.hsgr$", result$osrm_job_artifact))
  expect_named(result$logs, c("extract", "contract"))
})
