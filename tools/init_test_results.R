# This script initializes the test results RDS file before the live tests run.
# It ensures that we have a record of what we INTENDED to test, 
# defaulting all to FALSE (failed) until the tests explicitly mark them TRUE.

pkgload::load_all(".")

# 1. Discover all versions that SHOULD be tested
all_tags <- osrm_check_available_versions(prereleases = TRUE)

# Filter logic matching the test suite:
# 1. Latest v5.x
# 2. All versions >= v6.0.0
v5_tags <- all_tags[grepl("^v5\\.", all_tags)]
latest_v5 <- if (length(v5_tags) > 0) v5_tags[1] else NULL
v6_plus_tags <- all_tags[!grepl("^v5\\.", all_tags)]

versions_to_test <- unique(c(latest_v5, v6_plus_tags))

if (length(versions_to_test) == 0) {
  cat("No OSRM versions found to test. Skipping initialization.\n")
} else {
  # Initialize with FALSE
  test_results <- structure(rep(FALSE, length(versions_to_test)), names = versions_to_test)
  saveRDS(test_results, "test_results.rds")
  cat("Initialized test_results.rds with", length(versions_to_test), "versions.\n")
}
