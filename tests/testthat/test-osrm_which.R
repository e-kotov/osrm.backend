# Tests for osrm_which.R

test_that("osrm_which returns correct structure", {
  skip_if_not_installed("processx")
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")

  result <- osrm_which(quiet = TRUE)

  expect_type(result, "list")
  expect_named(result, c("executable", "directory", "osrm_version", "logs"))
  expect_true(file.exists(result$executable))
  expect_true(dir.exists(result$directory))
})

test_that("osrm_which quiet parameter suppresses output", {
  skip_if_not_installed("processx")
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")

  expect_silent(osrm_which(quiet = TRUE))
})

test_that("osrm_which non-quiet shows messages", {
  skip_if_not_installed("processx")
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")

  expect_message(
    osrm_which(quiet = FALSE),
    "OSRM installation directory"
  )
})

test_that("osrm_which errors when osrm-routed not found", {
  # This test works by temporarily setting an option to a non-existent path
  orig_opt <- getOption("osrm.routed.exec")
  on.exit(options(osrm.routed.exec = orig_opt))

  options(osrm.routed.exec = "definitely-not-a-real-osrm-executable-xyz123")

  expect_error(
    osrm_which(),
    "Cannot find"
  )
})

test_that("osrm_which respects osrm.routed.exec option", {
  skip_if_not_installed("processx")

  orig_opt <- getOption("osrm.routed.exec")
  on.exit(options(osrm.routed.exec = orig_opt))

  # Set to a non-existent executable - should error
  options(osrm.routed.exec = "/custom/path/osrm-routed-fake")

  expect_error(
    osrm_which(),
    "/custom/path/osrm-routed-fake"
  )
})
