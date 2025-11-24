#' Resolve OSRM File Path from Directory or File
#'
#' Internal helper that resolves a file path, accepting either a direct file
#' path or a directory containing exactly one file matching the specified pattern.
#'
#' @param input_path A string. Path to a file or directory.
#' @param pattern A string. Regex pattern to match files (e.g., "\\.osm\\.pbf$").
#' @param file_description A string. Human-readable description of expected files
#'   for error messages (e.g., "OSM files (.osm, .osm.bz2, or .osm.pbf)").
#' @param error_context A string or NULL. Additional context for error messages
#'   (e.g., "Please check that you have run `osrm_extract` first.").
#'
#' @return A string. The resolved normalized file path.
#' @keywords internal
#' @noRd
resolve_osrm_path <- function(input_path,
                               pattern,
                               file_description,
                               error_context = NULL) {
  # Check if path exists (let calling function handle missing files)
  if (!file.exists(input_path) && !dir.exists(input_path)) {
    # Return as-is if doesn't exist - calling function will handle the error
    return(input_path)
  }

  # Normalize path
  input_path <- normalizePath(input_path, mustWork = TRUE)

  # If input is a directory, search for matching files
  if (dir.exists(input_path)) {
    matching_files <- list.files(
      input_path,
      pattern = pattern,
      ignore.case = TRUE,
      full.names = TRUE
    )

    if (length(matching_files) == 0) {
      error_msg <- paste0(
        "No ", file_description, " found in directory: ", input_path
      )
      if (!is.null(error_context)) {
        error_msg <- paste0(error_msg, "\n", error_context)
      }
      stop(error_msg, call. = FALSE)
    } else if (length(matching_files) > 1) {
      stop(
        "Multiple ", file_description, " found in directory: ", input_path,
        "\n  Files: ", paste(basename(matching_files), collapse = ", "),
        "\n  Please specify a single file path instead of a directory.",
        call. = FALSE
      )
    }

    input_path <- matching_files[1]
  }

  input_path
}
