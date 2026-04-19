test_that("read_pbf_header_bbox works with cur.osm.pbf", {
  pbf_file <- system.file("extdata", "cur.osm.pbf", package = "osrm.backend")
  skip_if(pbf_file == "", message = "cur.osm.pbf not found")

  res <- read_pbf_header_bbox(pbf_file)

  expect_type(res, "list")
  expect_named(res, c("center", "bbox"))
  expect_length(res$center, 2)
  expect_length(res$bbox, 4)
  expect_true(all(is.numeric(res$center)))
  expect_true(all(is.numeric(res$bbox)))

  # Semi-hardcoded expectations based on Curitiba (cur.osm.pbf location)
  # lng: -49.27, lat: -25.43 approx
  expect_equal(res$center[1], -49.2699, tolerance = 0.001)
  expect_equal(res$center[2], -25.4305, tolerance = 0.001)
  expect_equal(res$bbox[1], -49.2889, tolerance = 0.001) # lng_min
  expect_equal(res$bbox[2], -25.4413, tolerance = 0.001) # lat_min
  expect_equal(res$bbox[3], -49.2509, tolerance = 0.001) # lng_max
  expect_equal(res$bbox[4], -25.4197, tolerance = 0.001) # lat_max
})

test_that(".get_pbf_extent works with cur.osm.pbf", {
  pbf_file <- system.file("extdata", "cur.osm.pbf", package = "osrm.backend")
  skip_if(pbf_file == "", message = "cur.osm.pbf not found")

  res <- .get_pbf_extent(pbf_file)

  expect_type(res, "list")
  expect_named(res, c("center", "bbox"))
  expect_equal(res$center[1], -49.2699, tolerance = 0.001)
})

test_that("read_pbf_header_bbox handles missing files", {
  expect_null(read_pbf_header_bbox("nonexistent.pbf"))
})

test_that(".get_pbf_extent handles missing files", {
  expect_null(.get_pbf_extent("nonexistent.pbf"))
})

test_that("read_pbf_header_bbox handles invalid files", {
  tmp <- tempfile(fileext = ".pbf")
  writeBin(as.raw(c(0, 0, 0, 1, 0)), tmp) # Small invalid header
  on.exit(unlink(tmp))

  expect_null(read_pbf_header_bbox(tmp))
})
