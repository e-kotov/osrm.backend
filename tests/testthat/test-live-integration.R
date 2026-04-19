test_that("Live installation and basic routing works for all supported OSRM versions", {
  skip_if_not(identical(Sys.getenv("RUN_OSRM_LIVE_TESTS"), "true"), "Live OSRM tests skipped")
  
  # 0. DYNAMIC VERSION DISCOVERY
  # Get all versions with binaries for this platform
  all_tags <- osrm_check_available_versions(prereleases = TRUE)
  
  if (length(all_tags) == 0) {
    skip("No OSRM releases with binaries found for this platform.")
  }
  
  # Filter logic:
  # 1. Latest v5.x (usually v5.27.1)
  # 2. All versions >= v6.0.0
  v5_tags <- all_tags[grepl("^v5\\.", all_tags)]
  latest_v5 <- if (length(v5_tags) > 0) v5_tags[1] else NULL
  
  v6_plus_tags <- all_tags[!grepl("^v5\\.", all_tags)]
  # Note: osrm_check_available_versions returns tags sorted newest first.
  
  versions_to_test <- unique(c(latest_v5, v6_plus_tags))
  
  # For Mac, we can only test v6+ if we are on Sequoia (Darwin 24)
  is_macos <- identical(tolower(Sys.info()[["sysname"]]), "darwin")
  darwin_version <- NA_integer_
  if (is_macos) {
    darwin_version <- as.integer(gsub("\\..*", "", Sys.info()[["release"]]))
  }
  
  # Example OSM PBF (minimal)
  pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  
  for (ver in versions_to_test) {
    # Skip v6+ on old macOS to avoid intended failures
    if (is_macos && darwin_version < 24 && ver != latest_v5) {
      message(sprintf("Skipping OSRM %s on macOS %d (requires Darwin 24+)", ver, darwin_version))
      next
    }
    
    message(sprintf("\n>>> LIVE TEST: OSRM %s <<<", ver))
    
    # 1. INSTALL
    # Use a clean subdirectory for every version
    test_dir <- file.path(tempdir(), paste0("osrm_live_", gsub("\\.", "_", ver)))
    if (dir.exists(test_dir)) unlink(test_dir, recursive = TRUE)
    dir.create(test_dir, recursive = TRUE)
    
    # We use path_action="none" to avoid messing with .Rprofile in CI
    # This MUST succeed
    install_path <- expect_silent(
      osrm_install(version = ver, dest_dir = test_dir, path_action = "none", quiet = FALSE)
    )
    
    # 2. VERIFY BINARIES
    routed_bin <- list.files(install_path, pattern = "^osrm-routed(\\.exe)?$", full.names = TRUE)
    expect_length(routed_bin, 1)
    
    # Check version output
    res_ver <- system2(routed_bin, "--version", stdout = TRUE, stderr = TRUE)
    # The --version output usually starts with the version number (e.g. "5.27.1")
    expect_match(res_ver, gsub("^v", "", ver), fixed = TRUE)
    
    # 3. PREPARE GRAPH
    # Copy PBF to unique name in version dir
    tmp_pbf <- file.path(test_dir, paste0("test_", ver, ".osm.pbf"))
    file.copy(pbf_path, tmp_pbf)
    
    # Ensure the binaries we just installed are the ones used
    old_opt <- options(osrm.routed.exec = routed_bin)
    
    # This MUST succeed
    graph <- expect_silent(
      osrm_prepare_graph(tmp_pbf, threads = 1L, quiet = FALSE)
    )
    
    # 4. START SERVER
    # Use a random high port to avoid conflicts
    port <- 10000 + sample(1:1000, 1)
    
    # This MUST succeed
    srv <- expect_silent(
      osrm_start_server(graph$osrm_job_artifact, port = port, threads = 1L)
    )
    
    # 5. LIVENESS CHECK
    expect_true(srv$is_alive())
    
    # 6. STOP SERVER
    expect_message(
      stopped <- osrm_stop(srv),
      "Terminated OSRM server"
    )
    expect_false(srv$is_alive())
    
    # Cleanup this version
    options(old_opt)
    unlink(test_dir, recursive = TRUE)
  }
})
