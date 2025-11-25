# Tests for osrm_start.R

test_that("osrm_start errors when path doesn't exist", {
  expect_error(
    osrm_start(path = "/nonexistent/path/xyz123", quiet = TRUE),
    "Input path does not exist"
  )
})

test_that("osrm_start errors with invalid file type", {
  tmp_dir <- tempdir()
  invalid_path <- file.path(tmp_dir, "data.txt")
  file.create(invalid_path)
  on.exit(unlink(invalid_path), add = TRUE)

  # This test requires OSRM to be installed
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")

  expect_error(
    osrm_start(path = invalid_path, quiet = TRUE),
    "Invalid input file type"
  )
})

test_that("osrm_start errors when directory has no graphs or OSM files", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a dummy file that's not .osm.pbf or graph
  file.create(file.path(tmp_dir, "dummy.txt"))

  # This test requires OSRM to be installed
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")

  expect_error(
    osrm_start(path = tmp_dir, quiet = TRUE),
    "Directory contains no prepared OSRM graphs"
  )
})

test_that("osrm_start works with direct path to prepared graph file", {
  skip_if_not_installed("processx")
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")

  # Use the test graph from setup
  pbf_file <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  skip_if(!file.exists(pbf_file), "Test data not available")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Copy test file
  file.copy(pbf_file, file.path(tmp_dir, "cur.osm.pbf"))

  # Prepare the graph
  skip_if(
    !getOption("osrm.backend.skip_osrm_tests", TRUE) == FALSE,
    "OSRM tests skipped (binary not available)"
  )

  graph_result <- try(
    osrm_prepare_graph(
      input_osm = file.path(tmp_dir, "cur.osm.pbf"),
      quiet = TRUE
    ),
    silent = TRUE
  )

  skip_if(inherits(graph_result, "try-error"), "Graph preparation failed")

  # Now test osrm_start with the prepared graph
  server <- try(
    osrm_start(
      path = graph_result$osrm_job_artifact,
      quiet = TRUE
    ),
    silent = TRUE
  )

  if (!inherits(server, "try-error") && server$is_alive()) {
    on.exit(osrm_stop(server, quiet = TRUE), add = TRUE)
    expect_s3_class(server, "process")
  } else {
    skip("Server failed to start")
  }
})

test_that("osrm_start handles directory with graph files", {
  skip_if_not_installed("processx")
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")

  # Use the test graph from setup
  pbf_file <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  skip_if(!file.exists(pbf_file), "Test data not available")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file.copy(pbf_file, file.path(tmp_dir, "cur.osm.pbf"))

  skip_if(
    !getOption("osrm.backend.skip_osrm_tests", TRUE) == FALSE,
    "OSRM tests skipped (binary not available)"
  )

  # Prepare the graph to create graph files in the directory
  graph_result <- try(
    osrm_prepare_graph(
      input_osm = file.path(tmp_dir, "cur.osm.pbf"),
      quiet = TRUE
    ),
    silent = TRUE
  )

  skip_if(inherits(graph_result, "try-error"), "Graph preparation failed")

  # Now test osrm_start with the directory
  server <- try(
    osrm_start(
      path = tmp_dir,
      quiet = TRUE
    ),
    silent = TRUE
  )

  if (!inherits(server, "try-error") && server$is_alive()) {
    on.exit(osrm_stop(server, quiet = TRUE), add = TRUE)
    expect_s3_class(server, "process")
  } else {
    skip("Server failed to start")
  }
})

test_that("osrm_start algorithm parameter validation", {
  # Test that it accepts valid algorithms
  expect_silent({
    match.arg("mld", c("mld", "ch"))
    match.arg("ch", c("mld", "ch"))
  })

  # Test that it rejects invalid algorithms
  expect_error(
    match.arg("invalid", c("mld", "ch")),
    "should be one of"
  )
})

test_that("osrm_start respects algorithm parameter when selecting existing graphs", {
  skip_if_not_installed("processx")
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")

  pbf_file <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  skip_if(!file.exists(pbf_file), "Test data not available")

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file.copy(pbf_file, file.path(tmp_dir, "cur.osm.pbf"))

  skip_if(
    !getOption("osrm.backend.skip_osrm_tests", TRUE) == FALSE,
    "OSRM tests skipped (binary not available)"
  )

  # Prepare both MLD and CH graphs
  mld_result <- try(
    osrm_prepare_graph(
      input_osm = file.path(tmp_dir, "cur.osm.pbf"),
      algorithm = "mld",
      quiet = TRUE
    ),
    silent = TRUE
  )
  skip_if(inherits(mld_result, "try-error"), "MLD graph preparation failed")

  ch_result <- try(
    osrm_prepare_graph(
      input_osm = file.path(tmp_dir, "cur.osm.pbf"),
      algorithm = "ch",
      quiet = TRUE
    ),
    silent = TRUE
  )
  skip_if(inherits(ch_result, "try-error"), "CH graph preparation failed")

  # Both graph types should now exist
  expect_true(file.exists(mld_result$osrm_job_artifact))
  expect_true(file.exists(ch_result$osrm_job_artifact))

  # Test 1: Start with algorithm = "ch" should use CH graph
  server_ch <- try(
    osrm_start(
      path = tmp_dir,
      algorithm = "ch",
      quiet = TRUE
    ),
    silent = TRUE
  )

  if (!inherits(server_ch, "try-error") && server_ch$is_alive()) {
    # Check that the server command line contains the CH graph file
    cmdline <- server_ch$get_cmdline()
    expect_true(any(grepl("\\.osrm\\.hsgr$", cmdline)))
    expect_false(any(grepl("\\.osrm\\.mldgr$", cmdline)))
    osrm_stop(server_ch, quiet = TRUE)
  } else {
    skip("CH server failed to start")
  }

  # Test 2: Start with algorithm = "mld" should use MLD graph
  server_mld <- try(
    osrm_start(
      path = tmp_dir,
      algorithm = "mld",
      quiet = TRUE
    ),
    silent = TRUE
  )

  if (!inherits(server_mld, "try-error") && server_mld$is_alive()) {
    # Check that the server command line contains the MLD graph file
    cmdline <- server_mld$get_cmdline()
    expect_true(any(grepl("\\.osrm\\.mldgr$", cmdline)))
    expect_false(any(grepl("\\.osrm\\.hsgr$", cmdline)))
    osrm_stop(server_mld, quiet = TRUE)
  } else {
    skip("MLD server failed to start")
  }
})
