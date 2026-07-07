test_that("Cross-provider compatibility works for latest versions", {
  skip_if_not(identical(Sys.getenv("RUN_OSRM_LIVE_TESTS"), "true"), "Live OSRM tests skipped")
  
  test_map <- tempfile(fileext = ".osm.pbf")
  download.file("https://download.geofabrik.de/europe/monaco-latest.osm.pbf", test_map, quiet = TRUE)
  
  test_versions <- osrm_check_available_versions(prereleases = FALSE)
  
  for (ver in test_versions) {
    dir_official <- tempfile()
    dir.create(dir_official)
    
    has_official <- tryCatch({
      osrm_install(version = ver, osrm_binaries_provider = "official", dest_dir = dir_official, quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    
    dir_ours <- tempfile()
    dir.create(dir_ours)
    osrm_install(version = ver, osrm_binaries_provider = "default", dest_dir = dir_ours, quiet = TRUE)
    
    if (has_official) {
      map_official <- tempfile(fileext = ".osm.pbf")
      file.copy(test_map, map_official)
      
      opts <- options(osrm.backend.path = dir_official)
      job_official <- osrm_prepare_graph(map_official, profile = osrm_find_profile("car.lua"), verbosity = "NONE")
      options(opts)
      
      opts <- options(osrm.backend.path = dir_ours)
      server_ours <- osrm_start(job_official$osrm_job_artifact, port = 5001, verbosity = "NONE")
      Sys.sleep(2) 
      res_ours <- jsonlite::fromJSON("http://127.0.0.1:5001/route/v1/driving/7.419,43.731;7.418,43.733")
      server_ours$kill()
      options(opts)
      Sys.sleep(1)
      
      map_ours <- tempfile(fileext = ".osm.pbf")
      file.copy(test_map, map_ours)
      
      opts <- options(osrm.backend.path = dir_ours)
      job_ours <- osrm_prepare_graph(map_ours, profile = osrm_find_profile("car.lua"), verbosity = "NONE")
      options(opts)
      
      opts <- options(osrm.backend.path = dir_official)
      server_official <- osrm_start(job_ours$osrm_job_artifact, port = 5001, verbosity = "NONE")
      Sys.sleep(2)
      res_official <- jsonlite::fromJSON("http://127.0.0.1:5001/route/v1/driving/7.419,43.731;7.418,43.733")
      server_official$kill()
      options(opts)
      
      expect_equal(res_ours$routes$distance, res_official$routes$distance)
      expect_equal(res_ours$routes$duration, res_official$routes$duration)
      expect_equal(res_ours$routes$geometry, res_official$routes$geometry)
    }
  }
})
