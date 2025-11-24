#!/usr/bin/env Rscript

# Analysis script for CH and MLD pipeline file conflicts
# This script can be run directly without testthat infrastructure

devtools::load_all()
osrm_install()

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
      cat(sprintf(
        "  + %s (size: %d bytes)\n",
        after$basename[i],
        after$size[i]
      ))
    }
    return(invisible(NULL))
  }

  # Find new files
  new_files <- after[!after$basename %in% before$basename, ]
  if (nrow(new_files) > 0) {
    cat("\nNew files created:\n")
    for (i in seq_len(nrow(new_files))) {
      cat(sprintf(
        "  + %s (size: %d bytes)\n",
        new_files$basename[i],
        new_files$size[i]
      ))
    }
  }

  invisible(NULL)
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
  cat("# OSRM Pipeline File Conflict Analysis\n")
  cat(strrep("#", 70), "\n")

  # Test 1: CH pipeline only
  cat("\n\nTEST 1: CH Pipeline Only\n")
  cat(strrep("-", 70), "\n")

  ch_dir <- file.path(base_tmp, "ch_only")
  dir.create(ch_dir)
  file.copy(pbf_file, file.path(ch_dir, "cur.osm.pbf"))

  cat("\nRunning osrm_prepare_graph with algorithm='ch'...\n")
  ch_result <- osrm_prepare_graph(
    input_osm = file.path(ch_dir, "cur.osm.pbf"),
    algorithm = "ch",
    quiet = TRUE,
    verbose = FALSE
  )

  after_ch <- get_osrm_files(ch_dir)
  cat(sprintf("\nCH created %d files\n", nrow(after_ch)))

  # Test 2: MLD pipeline only
  cat("\n\nTEST 2: MLD Pipeline Only\n")
  cat(strrep("-", 70), "\n")

  mld_dir <- file.path(base_tmp, "mld_only")
  dir.create(mld_dir)
  file.copy(pbf_file, file.path(mld_dir, "cur.osm.pbf"))

  cat("\nRunning osrm_prepare_graph with algorithm='mld'...\n")
  mld_result <- osrm_prepare_graph(
    input_osm = file.path(mld_dir, "cur.osm.pbf"),
    algorithm = "mld",
    quiet = TRUE,
    verbose = FALSE
  )

  after_mld <- get_osrm_files(mld_dir)
  cat(sprintf("\nMLD created %d files\n", nrow(after_mld)))

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
    for (f in ch_specific) {
      cat("  -", f, "\n")
    }
  } else {
    cat("  (none)\n")
  }

  cat("\nMLD-specific files (not in CH):\n")
  mld_specific <- setdiff(mld_only_files, ch_only_files)
  if (length(mld_specific) > 0) {
    for (f in mld_specific) {
      cat("  -", f, "\n")
    }
  } else {
    cat("  (none)\n")
  }

  cat("\nShared files (in both pipelines):\n")
  shared <- intersect(ch_only_files, mld_only_files)
  if (length(shared) > 0) {
    for (f in shared) {
      cat("  -", f, "\n")
    }
  } else {
    cat("  (none)\n")
  }

  # Compare checksums of shared files
  if (length(shared) > 0) {
    cat("\n")
    cat(strrep("#", 70), "\n")
    cat("# MD5 Checksum Comparison of Shared Files\n")
    cat(strrep("#", 70), "\n\n")

    identical_count <- 0
    different_count <- 0

    for (fname in shared) {
      ch_file <- file.path(ch_dir, fname)
      mld_file <- file.path(mld_dir, fname)

      ch_hash <- tools::md5sum(ch_file)
      mld_hash <- tools::md5sum(mld_file)

      if (ch_hash == mld_hash) {
        identical_count <- identical_count + 1
      } else {
        different_count <- different_count + 1
        cat(sprintf("DIFFERENT: %s\n", fname))
        cat(sprintf("  CH:  %s (size: %d)\n", ch_hash, file.size(ch_file)))
        cat(sprintf("  MLD: %s (size: %d)\n\n", mld_hash, file.size(mld_file)))
      }
    }

    cat(sprintf("\nSummary:\n"))
    cat(sprintf("  Identical files: %d/%d\n", identical_count, length(shared)))
    cat(sprintf("  Different files: %d/%d\n", different_count, length(shared)))

    if (different_count == 0) {
      cat("\n")
      cat(strrep("*", 70), "\n")
      cat("*** ALL SHARED FILES ARE IDENTICAL! ***\n")
      cat(strrep("*", 70), "\n")
      cat(
        "This means CH and MLD graphs CAN safely coexist in the same directory.\n"
      )
      cat(
        "The algorithm-specific files (.hsgr for CH, .mldgr/.cells/.partition\n"
      )
      cat("for MLD) are distinct, and the shared base files are identical.\n")
    } else {
      cat("\n")
      cat(strrep("!", 70), "\n")
      cat("*** SHARED FILES DIFFER! ***\n")
      cat(strrep("!", 70), "\n")
      cat("This means CH and MLD graphs CANNOT safely coexist - the shared\n")
      cat(
        "files have different contents depending on which algorithm was used.\n"
      )
    }
  }

  cat("\n")
  cat(strrep("#", 70), "\n")
  cat("# Analysis Complete\n")
  cat(strrep("#", 70), "\n\n")
}

# Run the analysis
main()
