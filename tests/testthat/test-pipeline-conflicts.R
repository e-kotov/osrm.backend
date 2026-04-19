# Analysis of CH and MLD pipeline file conflicts

# Helper function to get all osrm-related files with their modification times
get_osrm_files <- function(dir) {
  all_files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  osrm_files <- all_files[grepl("\\.osrm\\.", basename(all_files))]

  if (length(osrm_files) == 0) {
    return(data.frame(
      file = character(0),
      basename = character(0),
      size = numeric(0),
      mtime = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    file = osrm_files,
    basename = basename(osrm_files),
    size = file.size(osrm_files),
    mtime = format(file.mtime(osrm_files), "%Y-%m-%d %H:%M:%OS3"),
    stringsAsFactors = FALSE
  )
}

# Helper to compare file lists and identify changes
compare_file_snapshots <- function(before, after, label = "") {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(label, "\n")
  cat(strrep("=", 70), "\n")

  if (nrow(before) == 0 && nrow(after) == 0) {
    cat("No OSRM files in either snapshot.\n")
    return(invisible(NULL))
  }

  if (nrow(before) == 0) {
    cat("\nNew files created:\n")
    for (i in seq_len(nrow(after))) {
      cat(sprintf("  + %s (size: %d bytes)\n",
                  after$basename[i], after$size[i]))
    }
    return(invisible(NULL))
  }

  # Find new files
  new_files <- after[!after$basename %in% before$basename, ]
  if (nrow(new_files) > 0) {
    cat("\nNew files created:\n")
    for (i in seq_len(nrow(new_files))) {
      cat(sprintf("  + %s (size: %d bytes)\n",
                  new_files$basename[i], new_files$size[i]))
    }
  }

  # Find modified files (same name but different mtime or size)
  common_files <- intersect(before$basename, after$basename)
  if (length(common_files) > 0) {
    modified <- character()
    for (fname in common_files) {
      before_file <- before[before$basename == fname, ]
      after_file <- after[after$basename == fname, ]

      if (before_file$mtime != after_file$mtime ||
          before_file$size != after_file$size) {
        modified <- c(modified, fname)
        cat(sprintf("\nModified file: %s\n", fname))
        cat(sprintf("  Before: size=%d, mtime=%s\n",
                    before_file$size, before_file$mtime))
        cat(sprintf("  After:  size=%d, mtime=%s\n",
                    after_file$size, after_file$mtime))
      }
    }

    if (length(modified) == 0 && nrow(new_files) == 0) {
      cat("\nNo files modified or created.\n")
    }
  }

  # Find removed files
  removed_files <- before[!before$basename %in% after$basename, ]
  if (nrow(removed_files) > 0) {
    cat("\nFiles removed:\n")
    for (i in seq_len(nrow(removed_files))) {
      cat(sprintf("  - %s\n", removed_files$basename[i]))
    }
  }

  invisible(NULL)
}

test_that("Analyze CH and MLD pipeline file conflicts", {
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")
  skip_if(
    !getOption("osrm.backend.skip_osrm_tests", TRUE) == FALSE,
    "OSRM tests skipped (binary not available)"
  )

  pbf_file <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  skip_if(!file.exists(pbf_file), "Test data not available")

  # Create base temp directory for all tests
  base_tmp <- tempfile()
  dir.create(base_tmp)
  on.exit(unlink(base_tmp, recursive = TRUE), add = TRUE)

  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("# OSRM Pipeline File Conflict Analysis\n")
  cat(strrep("#", 70), "\n")

  # Test 1: CH pipeline only
  cat("\n\nTEST 1: CH Pipeline Only\n")
  cat(strrep("-", 70), "\n")

  ch_dir <- file.path(base_tmp, "ch_only")
  dir.create(ch_dir)
  file.copy(pbf_file, file.path(ch_dir, "cur.osm.pbf"))

  before_ch <- get_osrm_files(ch_dir)
  cat("\nRunning osrm_prepare_graph with algorithm='ch'...\n")

  ch_result <- try(
    osrm_prepare_graph(
      input_osm = file.path(ch_dir, "cur.osm.pbf"),
      algorithm = "ch",
      quiet = TRUE,
      verbose = FALSE
    ),
    silent = TRUE
  )

  skip_if(inherits(ch_result, "try-error"), "CH preparation failed")

  after_ch <- get_osrm_files(ch_dir)
  compare_file_snapshots(before_ch, after_ch, "CH Pipeline - Files Created")

  # Test 2: MLD pipeline only
  cat("\n\nTEST 2: MLD Pipeline Only\n")
  cat(strrep("-", 70), "\n")

  mld_dir <- file.path(base_tmp, "mld_only")
  dir.create(mld_dir)
  file.copy(pbf_file, file.path(mld_dir, "cur.osm.pbf"))

  before_mld <- get_osrm_files(mld_dir)
  cat("\nRunning osrm_prepare_graph with algorithm='mld'...\n")

  mld_result <- try(
    osrm_prepare_graph(
      input_osm = file.path(mld_dir, "cur.osm.pbf"),
      algorithm = "mld",
      quiet = TRUE,
      verbose = FALSE
    ),
    silent = TRUE
  )

  skip_if(inherits(mld_result, "try-error"), "MLD preparation failed")

  after_mld <- get_osrm_files(mld_dir)
  compare_file_snapshots(before_mld, after_mld, "MLD Pipeline - Files Created")

  # Test 3: CH then MLD in same directory
  cat("\n\nTEST 3: CH Pipeline, Then MLD Pipeline (Same Directory)\n")
  cat(strrep("-", 70), "\n")

  ch_then_mld_dir <- file.path(base_tmp, "ch_then_mld")
  dir.create(ch_then_mld_dir)
  file.copy(pbf_file, file.path(ch_then_mld_dir, "cur.osm.pbf"))

  # First run CH
  cat("\nStep 1: Running CH pipeline...\n")
  ch_first <- try(
    osrm_prepare_graph(
      input_osm = file.path(ch_then_mld_dir, "cur.osm.pbf"),
      algorithm = "ch",
      quiet = TRUE,
      verbose = FALSE
    ),
    silent = TRUE
  )
  skip_if(inherits(ch_first, "try-error"), "CH preparation failed")

  Sys.sleep(0.1)  # Small delay to ensure different timestamps

  after_ch_first <- get_osrm_files(ch_then_mld_dir)
  cat("\nFiles after CH pipeline:\n")
  print(after_ch_first$basename)

  # Then run MLD
  cat("\nStep 2: Running MLD pipeline on same directory...\n")
  mld_second <- try(
    osrm_prepare_graph(
      input_osm = file.path(ch_then_mld_dir, "cur.osm.pbf"),
      algorithm = "mld",
      quiet = TRUE,
      verbose = FALSE,
      overwrite = TRUE
    ),
    silent = TRUE
  )
  skip_if(inherits(mld_second, "try-error"), "MLD preparation failed")

  after_mld_second <- get_osrm_files(ch_then_mld_dir)
  compare_file_snapshots(
    after_ch_first,
    after_mld_second,
    "CH → MLD: Changes When MLD Runs After CH"
  )

  # Test 4: MLD then CH in same directory
  cat("\n\nTEST 4: MLD Pipeline, Then CH Pipeline (Same Directory)\n")
  cat(strrep("-", 70), "\n")

  mld_then_ch_dir <- file.path(base_tmp, "mld_then_ch")
  dir.create(mld_then_ch_dir)
  file.copy(pbf_file, file.path(mld_then_ch_dir, "cur.osm.pbf"))

  # First run MLD
  cat("\nStep 1: Running MLD pipeline...\n")
  mld_first <- try(
    osrm_prepare_graph(
      input_osm = file.path(mld_then_ch_dir, "cur.osm.pbf"),
      algorithm = "mld",
      quiet = TRUE,
      verbose = FALSE
    ),
    silent = TRUE
  )
  skip_if(inherits(mld_first, "try-error"), "MLD preparation failed")

  Sys.sleep(0.1)  # Small delay to ensure different timestamps

  after_mld_first <- get_osrm_files(mld_then_ch_dir)
  cat("\nFiles after MLD pipeline:\n")
  print(after_mld_first$basename)

  # Then run CH
  cat("\nStep 2: Running CH pipeline on same directory...\n")
  ch_second <- try(
    osrm_prepare_graph(
      input_osm = file.path(mld_then_ch_dir, "cur.osm.pbf"),
      algorithm = "ch",
      quiet = TRUE,
      verbose = FALSE,
      overwrite = TRUE
    ),
    silent = TRUE
  )
  skip_if(inherits(ch_second, "try-error"), "CH preparation failed")

  after_ch_second <- get_osrm_files(mld_then_ch_dir)
  compare_file_snapshots(
    after_mld_first,
    after_ch_second,
    "MLD → CH: Changes When CH Runs After MLD"
  )

  # Summary comparison
  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("# SUMMARY: File Types by Pipeline\n")
  cat(strrep("#", 70), "\n")

  ch_only_files <- after_ch$basename
  mld_only_files <- after_mld$basename

  cat("\nCH-specific files (not in MLD):\n")
  ch_specific <- setdiff(ch_only_files, mld_only_files)
  if (length(ch_specific) > 0) {
    for (f in ch_specific) cat("  -", f, "\n")
  } else {
    cat("  (none)\n")
  }

  cat("\nMLD-specific files (not in CH):\n")
  mld_specific <- setdiff(mld_only_files, ch_only_files)
  if (length(mld_specific) > 0) {
    for (f in mld_specific) cat("  -", f, "\n")
  } else {
    cat("  (none)\n")
  }

  cat("\nShared files (in both pipelines):\n")
  shared <- intersect(ch_only_files, mld_only_files)
  if (length(shared) > 0) {
    for (f in shared) cat("  -", f, "\n")
  } else {
    cat("  (none)\n")
  }

  cat("\n")
  cat(strrep("#", 70), "\n")
  cat("# Analysis Complete\n")
  cat(strrep("#", 70), "\n\n")

  # Always succeed - this test is for analysis only
  expect_true(TRUE)
})

test_that("Algorithm conflict detection prevents mixing CH and MLD", {
  skip_on_cran()
  skip_if(Sys.which("osrm-routed") == "", "osrm-routed not available")
  skip_if(
    !getOption("osrm.backend.skip_osrm_tests", TRUE) == FALSE,
    "OSRM tests skipped (binary not available)"
  )

  pbf_file <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  skip_if(!file.exists(pbf_file), "Test data not available")

  # Test 1: Trying to run CH contract after MLD partition should error
  mld_then_ch_dir <- tempfile()
  dir.create(mld_then_ch_dir)
  on.exit(unlink(mld_then_ch_dir, recursive = TRUE), add = TRUE)

  file.copy(pbf_file, file.path(mld_then_ch_dir, "cur.osm.pbf"))

  # Run MLD pipeline stages
  extract_result <- osrm_extract(
    input_osm = file.path(mld_then_ch_dir, "cur.osm.pbf"),
    quiet = TRUE,
    overwrite = TRUE
  )

  partition_result <- osrm_partition(
    input_osrm = extract_result,
    quiet = TRUE
  )

  # Now try to run CH contract - should error
  expect_error(
    osrm_contract(
      input_osrm = extract_result,
      quiet = TRUE
    ),
    "Cannot run CH pipeline.*directory contains MLD algorithm files"
  )

  # Test 2: Trying to run MLD partition after CH contract should error
  ch_then_mld_dir <- tempfile()
  dir.create(ch_then_mld_dir)
  on.exit(unlink(ch_then_mld_dir, recursive = TRUE), add = TRUE)

  file.copy(pbf_file, file.path(ch_then_mld_dir, "cur.osm.pbf"))

  # Run CH pipeline stages
  extract_result2 <- osrm_extract(
    input_osm = file.path(ch_then_mld_dir, "cur.osm.pbf"),
    quiet = TRUE,
    overwrite = TRUE
  )

  contract_result <- osrm_contract(
    input_osrm = extract_result2,
    quiet = TRUE
  )

  # Now try to run MLD partition - should error
  expect_error(
    osrm_partition(
      input_osrm = extract_result2,
      quiet = TRUE
    ),
    "Cannot run MLD pipeline.*directory contains CH algorithm files"
  )

  # Test 3: osrm_prepare_graph should error when trying to switch without overwrite
  mixed_dir <- tempfile()
  dir.create(mixed_dir)
  on.exit(unlink(mixed_dir, recursive = TRUE), add = TRUE)

  file.copy(pbf_file, file.path(mixed_dir, "cur.osm.pbf"))

  # First prepare with CH
  osrm_prepare_graph(
    input_osm = file.path(mixed_dir, "cur.osm.pbf"),
    algorithm = "ch",
    quiet = TRUE,
    overwrite = TRUE
  )

  # Try to prepare with MLD without overwrite - should error
  expect_error(
    osrm_prepare_graph(
      input_osm = file.path(mixed_dir, "cur.osm.pbf"),
      algorithm = "mld",
      quiet = TRUE,
      overwrite = FALSE
    ),
    "Found existing OSRM files|Cannot run MLD pipeline"
  )
})

test_that("osrm_cleanup removes OSRM files correctly", {
  # Create a temporary directory with some OSRM files
  test_dir <- tempfile()
  dir.create(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE), add = TRUE)

  # Create mock OSRM files
  file.create(file.path(test_dir, "data.osm.pbf"))
  file.create(file.path(test_dir, "data.osrm.timestamp"))
  file.create(file.path(test_dir, "data.osrm.hsgr"))
  file.create(file.path(test_dir, "data.osrm.geometry"))

  # Test dry run
  removed_files <- osrm_cleanup(test_dir, dry_run = TRUE, quiet = TRUE)
  expect_true(length(removed_files) >= 3) # Should find at least 3 .osrm.* files
  expect_true(all(file.exists(removed_files))) # Files should still exist after dry run

  # Test actual cleanup (keeping OSM file)
  removed_files <- osrm_cleanup(test_dir, keep_osm = TRUE, quiet = TRUE)
  expect_false(file.exists(file.path(test_dir, "data.osrm.timestamp")))
  expect_false(file.exists(file.path(test_dir, "data.osrm.hsgr")))
  expect_true(file.exists(file.path(test_dir, "data.osm.pbf"))) # OSM file should remain

  # Test cleanup including OSM file
  file.create(file.path(test_dir, "data.osrm.timestamp")) # Recreate one file
  removed_files <- osrm_cleanup(
    file.path(test_dir, "data.osm.pbf"),
    keep_osm = FALSE,
    quiet = TRUE
  )
  expect_false(file.exists(file.path(test_dir, "data.osm.pbf")))
})
