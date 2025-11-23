test_that("osrm_prepare_graph runs MLD pipeline correctly", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osm <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm)
  on.exit(unlink(input_osm), add = TRUE)

  # Mock osrm_extract, osrm_partition, osrm_customize
  mock_extract <- function(input_osm, ...) {
    list(
      osrm_path = sub("\\.osm\\.pbf$", ".osrm", input_osm),
      logs = list(status = 0)
    )
  }
  mock_partition <- function(input_osrm, ...) {
    list(
      osrm_path = paste0(input_osrm, ".partition"),
      logs = list(status = 0)
    )
  }
  mock_customize <- function(input_osrm, ...) {
    list(
      osrm_path = sub("\\.partition$", ".mldgr", input_osrm),
      logs = list(status = 0)
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

  expect_true(grepl("\\.mldgr$", result$osrm_path))
  expect_named(result$logs, c("extract", "partition", "customize"))
})

test_that("osrm_prepare_graph runs CH pipeline correctly", {
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osm <- file.path(tmp_dir, "test.osm.pbf")
  file.create(input_osm)
  on.exit(unlink(input_osm), add = TRUE)

  mock_extract <- function(input_osm, ...) {
    list(
      osrm_path = sub("\\.osm\\.pbf$", ".osrm.timestamp", input_osm),
      logs = list(status = 0)
    )
  }
  mock_contract <- function(input_osrm, ...) {
    list(
      osrm_path = sub("\\.timestamp$", ".hsgr", input_osrm),
      logs = list(status = 0)
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

  expect_true(grepl("\\.hsgr$", result$osrm_path))
  expect_named(result$logs, c("extract", "contract"))
})
