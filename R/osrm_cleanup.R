#' Clean Up OSRM Files in a Directory
#'
#' Remove OSRM-generated files from a directory. This is useful when switching
#' between algorithms (CH and MLD) or when you want to start fresh.
#'
#' @details
#' OSRM creates many `.osrm.*` files during the extract, contract, partition,
#' and customize stages. This function helps clean up these files.
#'
#' **Important:** The CH and MLD algorithms cannot safely coexist in the same
#' directory because the MLD partition stage modifies some extract-stage files.
#' Use this function to clean up before switching algorithms.
#'
#' @param path A string. Path to an OSRM file or directory containing OSRM files.
#'   If a file path is provided (e.g., `data.osm.pbf` or `data.osrm.hsgr`),
#'   the base name will be extracted and all related `.osrm.*` files will be removed.
#' @param keep_osm Logical. If `TRUE` (default), keeps the original `.osm.pbf`
#'   (or `.osm`, `.osm.bz2`) file. If `FALSE`, removes it as well.
#' @param dry_run Logical. If `TRUE`, shows what would be deleted without
#'   actually deleting. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses messages. Default is `FALSE`.
#'
#' @return Invisibly returns a character vector of removed file paths.
#'
#' @export
#' @examples
#' \donttest{
#' if (identical(Sys.getenv("OSRM_EXAMPLES"), "true")) {
#'   install_dir <- osrm_install(
#'     version = "latest",
#'     path_action = "session",
#'     quiet = TRUE
#'   )
#'
#'   # Stage a temporary workspace with placeholder OSRM files
#'   pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#'   osrm_dir <- file.path(tempdir(), paste0("osrm-", Sys.getpid()))
#'   dir.create(osrm_dir, recursive = TRUE)
#'   tmp_pbf <- file.path(osrm_dir, "cur.osm.pbf")
#'   file.copy(from = pbf_path, to = tmp_pbf, overwrite = TRUE)
#'   file.create(
#'     file.path(osrm_dir, "cur.osrm.timestamp"),
#'     file.path(osrm_dir, "cur.osrm.hsgr"),
#'     file.path(osrm_dir, "cur.osrm.mldgr"),
#'     file.path(osrm_dir, "cur.osrm.partition")
#'   )
#'
#'   # Preview what would be deleted
#'   osrm_cleanup(osrm_dir, dry_run = TRUE)
#'
#'   # Clean up OSRM artifacts (keep the OSM file)
#'   osrm_cleanup(osrm_dir)
#'
#'   # Remove everything including the OSM source
#'   osrm_cleanup(osrm_dir, keep_osm = FALSE)
#'
#'   osrm_uninstall(
#'     dest_dir = install_dir,
#'     clear_path = TRUE,
#'     force = TRUE,
#'     quiet = TRUE
#'   )
#'   unlink(osrm_dir, recursive = TRUE)
#' }
#' }
osrm_cleanup <- function(
  path,
  keep_osm = TRUE,
  dry_run = FALSE,
  quiet = FALSE
) {
  if (!is.character(path) || length(path) != 1) {
    stop("'path' must be a single string", call. = FALSE)
  }

  if (!file.exists(path) && !dir.exists(path)) {
    stop("Path does not exist: ", path, call. = FALSE)
  }

  # Determine directory and base name
  if (dir.exists(path)) {
    dir_path <- path
    # Find all unique base names in the directory
    all_files <- list.files(dir_path, full.names = FALSE)
    osrm_files <- all_files[grepl("\\.osrm\\.", all_files)]

    if (length(osrm_files) == 0) {
      if (!quiet) {
        message("No OSRM files found in directory: ", dir_path)
      }
      return(invisible(character(0)))
    }

    # Extract unique base names
    base_names <- unique(sub("\\.osrm\\..*$", "", osrm_files))

    if (length(base_names) > 1) {
      stop(
        "Multiple OSRM file sets found in directory:\n  ",
        paste(base_names, collapse = ", "),
        "\n\nPlease specify a single file path to clean up a specific set.",
        call. = FALSE
      )
    }

    base_name <- base_names[1]
  } else {
    # Path is a file
    dir_path <- dirname(path)
    file_name <- basename(path)

    # Extract base name from file
    if (grepl("\\.osm(\\.pbf|\\.bz2)?$", file_name, ignore.case = TRUE)) {
      base_name <- sub(
        "\\.osm(\\.pbf|\\.bz2)?$",
        "",
        file_name,
        ignore.case = TRUE
      )
    } else if (grepl("\\.osrm\\.", file_name)) {
      base_name <- sub("\\.osrm\\..*$", "", file_name)
    } else {
      stop(
        "Cannot determine base name from file: ",
        file_name,
        "\nExpected .osm.pbf, .osm, .osm.bz2, or .osrm.* file",
        call. = FALSE
      )
    }
  }

  # Get all files matching the base name
  all_files <- list.files(dir_path, full.names = TRUE)

  # Find all OSRM files
  osrm_pattern <- paste0("^", gsub("\\.", "\\\\.", base_name), "\\.osrm\\.")
  osrm_files_to_remove <- all_files[grepl(osrm_pattern, basename(all_files))]

  # Optionally include OSM file
  if (!keep_osm) {
    osm_pattern <- paste0(
      "^",
      gsub("\\.", "\\\\.", base_name),
      "\\.osm(\\.pbf|\\.bz2)?$"
    )
    osm_files <- all_files[grepl(
      osm_pattern,
      basename(all_files),
      ignore.case = TRUE
    )]
    osrm_files_to_remove <- c(osrm_files_to_remove, osm_files)
  }

  if (length(osrm_files_to_remove) == 0) {
    if (!quiet) {
      message("No OSRM files found to remove for base name: ", base_name)
    }
    return(invisible(character(0)))
  }

  # Sort for consistent display
  osrm_files_to_remove <- sort(osrm_files_to_remove)

  # Display what will be removed
  if (!quiet || dry_run) {
    if (dry_run) {
      message("DRY RUN: Would remove the following files:")
    } else {
      message("Removing the following OSRM files:")
    }
    for (f in osrm_files_to_remove) {
      message("  - ", basename(f))
    }
    message("\nDirectory: ", dir_path)
    message("Total files: ", length(osrm_files_to_remove))
  }

  # Actually remove files (unless dry run)
  if (!dry_run) {
    removed <- file.remove(osrm_files_to_remove)
    failed <- osrm_files_to_remove[!removed]

    if (length(failed) > 0) {
      warning(
        "Failed to remove ",
        length(failed),
        " file(s):\n  ",
        paste(basename(failed), collapse = ", "),
        call. = FALSE
      )
    }

    if (!quiet) {
      message("\nSuccessfully removed ", sum(removed), " file(s).")
    }

    return(invisible(osrm_files_to_remove[removed]))
  } else {
    return(invisible(osrm_files_to_remove))
  }
}
