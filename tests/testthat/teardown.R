# Clean up the temporary binary folder after tests complete
test_osrm_path <- file.path(tempdir(), "osrm_bin_test")
if (dir.exists(test_osrm_path)) {
  unlink(test_osrm_path, recursive = TRUE, force = TRUE)
}
