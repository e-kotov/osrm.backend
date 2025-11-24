#!/usr/bin/env Rscript

# Stage-by-stage analysis script for OSRM pipeline file changes
# This script identifies which files are produced at each stage and whether
# later stages modify files from earlier stages

devtools::load_all()
osrm_install()

# Helper function to get all osrm-related files with their MD5 checksums
get_osrm_files_with_checksums <- function(dir) {
  all_files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  osrm_files <- all_files[grepl("\\.osrm\\.", basename(all_files))]

  if (length(osrm_files) == 0) {
    return(data.frame(
      file = character(0),
      basename = character(0),
      size = numeric(0),
      md5 = character(0),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    file = osrm_files,
    basename = basename(osrm_files),
    size = file.size(osrm_files),
    md5 = unname(tools::md5sum(osrm_files)),
    stringsAsFactors = FALSE
  )
}

# Helper to compare file snapshots
compare_snapshots <- function(before, after, label = "") {
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat(label, "\n")
  cat(strrep("=", 70), "\n")

  # New files
  new_files <- after[!after$basename %in% before$basename, ]
  if (nrow(new_files) > 0) {
    cat("\nNew files created:\n")
    for (i in seq_len(nrow(new_files))) {
      cat(sprintf("  + %s (size: %d bytes)\n",
                  new_files$basename[i], new_files$size[i]))
    }
  } else {
    cat("\nNo new files created.\n")
  }

  # Modified files (same name but different checksum)
  common_files <- intersect(before$basename, after$basename)
  if (length(common_files) > 0) {
    modified <- character()
    for (fname in common_files) {
      before_file <- before[before$basename == fname, ]
      after_file <- after[after$basename == fname, ]

      if (before_file$md5 != after_file$md5) {
        modified <- c(modified, fname)
      }
    }

    if (length(modified) > 0) {
      cat("\nModified files (changed MD5):\n")
      for (fname in modified) {
        before_file <- before[before$basename == fname, ]
        after_file <- after[after$basename == fname, ]
        cat(sprintf("  ! %s\n", fname))
        cat(sprintf("    Before: size=%d, md5=%s\n",
                    before_file$size, before_file$md5))
        cat(sprintf("    After:  size=%d, md5=%s\n",
                    after_file$size, after_file$md5))
      }
    } else if (nrow(new_files) == 0) {
      cat("\nNo files modified.\n")
    }
  }

  list(
    new_files = if (nrow(new_files) > 0) new_files$basename else character(0),
    modified_files = if (exists("modified")) modified else character(0)
  )
}

# Main analysis
main <- function() {
  # When using devtools::load_all(), system.file() doesn't work
  # So we look for the file in the package source directory
  pbf_file <- "inst/extdata/cur.osm.pbf"

  if (!file.exists(pbf_file)) {
    # Try system.file() as fallback (for installed package)
    pbf_file <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
  }

  if (!file.exists(pbf_file)) {
    stop(
      "Test data not available. Looked in inst/extdata/cur.osm.pbf and via system.file()"
    )
  }

  if (Sys.which("osrm-routed") == "") {
    stop("osrm-routed not found on PATH. Please install OSRM first.")
  }

  # Create base temp directory for all tests
  base_tmp <- tempfile()
  dir.create(base_tmp)
  on.exit(unlink(base_tmp, recursive = TRUE), add = TRUE)

  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("# OSRM Stage-by-Stage Pipeline Analysis\n")
  cat(strrep("#", 70), "\n")

  # ===========================================================================
  # TEST 1: Extract-only (baseline)
  # ===========================================================================
  cat("\n\nTEST 1: Extract Stage Only (Baseline)\n")
  cat(strrep("-", 70), "\n")

  extract_only_dir <- file.path(base_tmp, "extract_only")
  dir.create(extract_only_dir)
  file.copy(pbf_file, file.path(extract_only_dir, "cur.osm.pbf"))

  before_extract <- get_osrm_files_with_checksums(extract_only_dir)
  cat("\nRunning osrm_extract only...\n")

  extract_result <- osrm_extract(
    input_osm = file.path(extract_only_dir, "cur.osm.pbf"),
    quiet = TRUE,
    verbose = FALSE,
    overwrite = TRUE
  )

  after_extract <- get_osrm_files_with_checksums(extract_only_dir)
  extract_comparison <- compare_snapshots(
    before_extract, after_extract,
    "Extract Stage: Files Created"
  )

  extract_files <- after_extract$basename
  cat(sprintf("\nExtract stage created %d files\n", length(extract_files)))

  # ===========================================================================
  # TEST 2: Extract + Contract (CH pipeline)
  # ===========================================================================
  cat("\n\nTEST 2: Extract + Contract (CH Pipeline)\n")
  cat(strrep("-", 70), "\n")

  ch_dir <- file.path(base_tmp, "ch_pipeline")
  dir.create(ch_dir)
  file.copy(pbf_file, file.path(ch_dir, "cur.osm.pbf"))

  cat("\nStep 1: Running osrm_extract...\n")
  ch_extract_result <- osrm_extract(
    input_osm = file.path(ch_dir, "cur.osm.pbf"),
    quiet = TRUE,
    verbose = FALSE,
    overwrite = TRUE
  )

  after_ch_extract <- get_osrm_files_with_checksums(ch_dir)

  cat("Step 2: Running osrm_contract...\n")
  Sys.sleep(0.1)  # Ensure different timestamps

  ch_contract_result <- osrm_contract(
    input_osrm = ch_extract_result,
    quiet = TRUE,
    verbose = FALSE
  )

  after_ch_contract <- get_osrm_files_with_checksums(ch_dir)
  ch_comparison <- compare_snapshots(
    after_ch_extract, after_ch_contract,
    "Contract Stage: Changes After Extract"
  )

  # ===========================================================================
  # TEST 3: Extract + Partition (MLD pipeline stage 1)
  # ===========================================================================
  cat("\n\nTEST 3: Extract + Partition (MLD Pipeline Stage 1)\n")
  cat(strrep("-", 70), "\n")

  mld_partition_dir <- file.path(base_tmp, "mld_partition")
  dir.create(mld_partition_dir)
  file.copy(pbf_file, file.path(mld_partition_dir, "cur.osm.pbf"))

  cat("\nStep 1: Running osrm_extract...\n")
  mld_extract_result <- osrm_extract(
    input_osm = file.path(mld_partition_dir, "cur.osm.pbf"),
    quiet = TRUE,
    verbose = FALSE,
    overwrite = TRUE
  )

  after_mld_extract <- get_osrm_files_with_checksums(mld_partition_dir)

  cat("Step 2: Running osrm_partition...\n")
  Sys.sleep(0.1)  # Ensure different timestamps

  mld_partition_result <- osrm_partition(
    input_osrm = mld_extract_result,
    quiet = TRUE,
    verbose = FALSE
  )

  after_mld_partition <- get_osrm_files_with_checksums(mld_partition_dir)
  partition_comparison <- compare_snapshots(
    after_mld_extract, after_mld_partition,
    "Partition Stage: Changes After Extract"
  )

  # ===========================================================================
  # TEST 4: Extract + Partition + Customize (Full MLD pipeline)
  # ===========================================================================
  cat("\n\nTEST 4: Extract + Partition + Customize (Full MLD Pipeline)\n")
  cat(strrep("-", 70), "\n")

  mld_full_dir <- file.path(base_tmp, "mld_full")
  dir.create(mld_full_dir)
  file.copy(pbf_file, file.path(mld_full_dir, "cur.osm.pbf"))

  cat("\nStep 1: Running osrm_extract...\n")
  mld_full_extract_result <- osrm_extract(
    input_osm = file.path(mld_full_dir, "cur.osm.pbf"),
    quiet = TRUE,
    verbose = FALSE,
    overwrite = TRUE
  )

  cat("Step 2: Running osrm_partition...\n")
  Sys.sleep(0.1)

  mld_full_partition_result <- osrm_partition(
    input_osrm = mld_full_extract_result,
    quiet = TRUE,
    verbose = FALSE
  )

  after_mld_full_partition <- get_osrm_files_with_checksums(mld_full_dir)

  cat("Step 3: Running osrm_customize...\n")
  Sys.sleep(0.1)

  mld_full_customize_result <- osrm_customize(
    input_osrm = mld_full_partition_result,
    quiet = TRUE,
    verbose = FALSE
  )

  after_mld_full_customize <- get_osrm_files_with_checksums(mld_full_dir)
  customize_comparison <- compare_snapshots(
    after_mld_full_partition, after_mld_full_customize,
    "Customize Stage: Changes After Partition"
  )

  # ===========================================================================
  # SUMMARY ANALYSIS
  # ===========================================================================
  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("# SUMMARY: File Production and Modification by Stage\n")
  cat(strrep("#", 70), "\n")

  cat("\n1. EXTRACT STAGE CREATES:\n")
  cat(sprintf("   %d files total\n", length(extract_files)))
  for (f in extract_files) {
    cat("   -", f, "\n")
  }

  cat("\n2. CONTRACT STAGE (after extract):\n")
  if (length(ch_comparison$new_files) > 0) {
    cat(sprintf("   Creates %d new files:\n", length(ch_comparison$new_files)))
    for (f in ch_comparison$new_files) {
      cat("   +", f, "\n")
    }
  }
  if (length(ch_comparison$modified_files) > 0) {
    cat(sprintf("   MODIFIES %d files from extract stage:\n",
                length(ch_comparison$modified_files)))
    for (f in ch_comparison$modified_files) {
      cat("   !", f, "\n")
    }
  } else {
    cat("   Does NOT modify any extract-stage files\n")
  }

  cat("\n3. PARTITION STAGE (after extract):\n")
  if (length(partition_comparison$new_files) > 0) {
    cat(sprintf("   Creates %d new files:\n", length(partition_comparison$new_files)))
    for (f in partition_comparison$new_files) {
      cat("   +", f, "\n")
    }
  }
  if (length(partition_comparison$modified_files) > 0) {
    cat(sprintf("   MODIFIES %d files from extract stage:\n",
                length(partition_comparison$modified_files)))
    for (f in partition_comparison$modified_files) {
      cat("   !", f, "\n")
    }
  } else {
    cat("   Does NOT modify any extract-stage files\n")
  }

  cat("\n4. CUSTOMIZE STAGE (after partition):\n")
  if (length(customize_comparison$new_files) > 0) {
    cat(sprintf("   Creates %d new files:\n", length(customize_comparison$new_files)))
    for (f in customize_comparison$new_files) {
      cat("   +", f, "\n")
    }
  }
  if (length(customize_comparison$modified_files) > 0) {
    cat(sprintf("   MODIFIES %d files from partition stage:\n",
                length(customize_comparison$modified_files)))
    for (f in customize_comparison$modified_files) {
      cat("   !", f, "\n")
    }
  } else {
    cat("   Does NOT modify any partition-stage files\n")
  }

  # ===========================================================================
  # KEY FINDINGS
  # ===========================================================================
  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("# KEY FINDINGS\n")
  cat(strrep("#", 70), "\n")

  # Determine if pipelines can safely coexist
  ch_modifies_extract <- length(ch_comparison$modified_files) > 0
  partition_modifies_extract <- length(partition_comparison$modified_files) > 0

  cat("\n")
  if (ch_modifies_extract && partition_modifies_extract) {
    # Both modify extract files - check if they modify the SAME files
    common_modified <- intersect(
      ch_comparison$modified_files,
      partition_comparison$modified_files
    )

    if (length(common_modified) > 0) {
      cat(strrep("!", 70), "\n")
      cat("*** CRITICAL: CH and MLD CANNOT SAFELY COEXIST ***\n")
      cat(strrep("!", 70), "\n")
      cat("\nBoth contract and partition stages modify the same extract-stage files:\n")
      for (f in common_modified) {
        cat("  -", f, "\n")
      }
      cat("\nRunning one algorithm will break the other algorithm's graph.\n")
      cat("Users must choose ONE algorithm per directory.\n")
    } else {
      cat("Both algorithms modify extract files, but different ones.\n")
      cat("Further investigation needed.\n")
    }
  } else if (!ch_modifies_extract && !partition_modifies_extract) {
    cat(strrep("*", 70), "\n")
    cat("*** GOOD NEWS: Extract files are NOT modified by later stages ***\n")
    cat(strrep("*", 70), "\n")
    cat("\nThis means:\n")
    cat("- Extract-stage files can be safely preserved\n")
    cat("- Algorithm-specific files can be safely removed without re-extracting\n")
    cat("- A cleanup function can remove algorithm-specific files and keep extract files\n")
    cat("- Users can switch algorithms by cleaning up and re-running contract/partition+customize\n")
  } else {
    cat("Mixed results - further investigation needed.\n")
  }

  # Identify algorithm-specific files for cleanup function
  cat("\n\n")
  cat(strrep("#", 70), "\n")
  cat("# ALGORITHM-SPECIFIC FILES (safe to remove for cleanup)\n")
  cat(strrep("#", 70), "\n")

  cat("\nCH-specific files (contract stage creates):\n")
  for (f in ch_comparison$new_files) {
    cat("  -", f, "\n")
  }

  cat("\nMLD-specific files (partition + customize stages create):\n")
  mld_specific <- c(
    partition_comparison$new_files,
    customize_comparison$new_files
  )
  for (f in unique(mld_specific)) {
    cat("  -", f, "\n")
  }

  cat("\n")
  cat(strrep("#", 70), "\n")
  cat("# Analysis Complete\n")
  cat(strrep("#", 70), "\n\n")
}

# Run the analysis
main()
