test_that("osrm_extract runs osrm-extract with expected arguments", {
  skip_if_no_osrm()
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osm <- file.path(
    tmp_dir,
    paste0("osrm-test-", Sys.getpid(), ".osm.pbf")
  )
  generated_prefix <- sub("\\.osm\\.pbf$", "", input_osm)
  on.exit(
    {
      artifacts <- list.files(
        tmp_dir,
        pattern = paste0("^", basename(generated_prefix), ""),
        full.names = TRUE
      )
      unlink(artifacts, recursive = TRUE)
    },
    add = TRUE
  )

  file.create(input_osm)
  normalized_input <- normalizePath(input_osm)
  expected_timestamp <- paste0(
    sub("\\.osm\\.pbf$", "", normalized_input, ignore.case = TRUE),
    ".osrm.timestamp"
  )

  captured <- list()
  mock_run <- function(command, args, echo, spinner, echo_cmd, ...) {
    captured <<- list(
      command = command,
      args = args,
      quiet = !isTRUE(echo),
      spinner = spinner,
      echo_cmd = echo_cmd
    )
    file.create(expected_timestamp)
    list(status = 0, stdout = "", stderr = "")
  }

  result <- with_mocked_bindings(
    osrm_extract(
      input_osm = input_osm,
      profile = "car.lua",
      threads = 2L,
      data_version = "2024-01",
      dump_nbg_graph = TRUE,
      quiet = FALSE,
      spinner = FALSE,
      echo_cmd = FALSE
    ),
    run = mock_run,
    .package = "processx"
  )

  expect_equal(captured$command, "osrm-extract")
  expect_true(all(c("-p", "car.lua") %in% captured$args))
  expect_true("--dump-nbg-graph" %in% captured$args)
  expect_true("-d" %in% captured$args && "2024-01" %in% captured$args)
  expect_equal(result$osrm_path, expected_timestamp)
  expect_true(file.exists(expected_timestamp))
})

test_that("osrm_extract enforces overwrite flag for existing outputs", {
  skip_if_no_osrm()
  skip_if_not_installed("processx")

  tmp_dir <- tempdir()
  input_osm <- file.path(
    tmp_dir,
    paste0("osrm-overwrite-", Sys.getpid(), ".osm.pbf")
  )
  generated_prefix <- sub("\\.osm\\.pbf$", "", input_osm)
  on.exit(
    {
      artifacts <- list.files(
        tmp_dir,
        pattern = paste0("^", basename(generated_prefix), ""),
        full.names = TRUE
      )
      unlink(artifacts, recursive = TRUE)
    },
    add = TRUE
  )

  file.create(input_osm)
  existing_artifact <- paste0(generated_prefix, ".osrm.hsgr")
  file.create(existing_artifact)

  expect_error(
    with_mocked_bindings(
      osrm_extract(
        input_osm = input_osm,
        profile = "car.lua"
      ),
      run = function(...) stop("processx::run should not be called"),
      .package = "processx"
    ),
    "Found existing OSRM files"
  )

  normalized_input <- normalizePath(input_osm)
  expected_timestamp <- paste0(
    sub("\\.osm\\.pbf$", "", normalized_input, ignore.case = TRUE),
    ".osrm.timestamp"
  )

  result <- with_mocked_bindings(
    osrm_extract(
      input_osm = input_osm,
      profile = "car.lua",
      overwrite = TRUE,
      quiet = FALSE,
      spinner = FALSE
    ),
    run = function(...) {
      file.create(expected_timestamp)
      list(status = 0, stdout = "", stderr = "")
    },
    .package = "processx"
  )

  expect_equal(result$osrm_path, expected_timestamp)
  expect_true(file.exists(expected_timestamp))
})
