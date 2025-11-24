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

#' @export
print.osrm_job <- function(x, ...) {
  cat("------------------------------------------------------\n")
  cat("OSRM Job Completed\n")
  cat("------------------------------------------------------\n")
  cat("Working Directory: ", x$osrm_working_dir, "\n")
  cat("Job Artifact:      ", x$osrm_job_artifact, "\n")

  # Check if logs is a processx object or a list of them
  if (inherits(x$logs, "process_result")) {
    status <- x$logs$status
    cat("Status: ", ifelse(status == 0, "Success", paste("Error code", status)), "\n")
  } else if (is.list(x$logs)) {
    completed_stages <- names(x$logs)
    cat("Stages completed:  ", paste(completed_stages, collapse = ", "), "\n")

    # Analyze pipeline state
    pipeline_info <- .analyze_pipeline_state(completed_stages)

    # Show pipeline path and remaining stages
    if (!is.null(pipeline_info$pipeline)) {
      cat("Pipeline:          ", pipeline_info$pipeline, "\n")

      if (length(pipeline_info$remaining) > 0) {
        cat("Remaining stages:  ", paste(pipeline_info$remaining, collapse = ", "), "\n")
      }
    }

    # Show next step
    if (!is.null(pipeline_info$next_step)) {
      cat("\n")
      cat("Next step:         ", pipeline_info$next_step, "\n")
    } else if (length(pipeline_info$alternative_steps) > 0) {
      cat("\n")
      cat("Next step options:\n")
      for (step in pipeline_info$alternative_steps) {
        cat("  - ", step, "\n", sep = "")
      }
    }

    # Show routing readiness
    if (pipeline_info$ready_for_routing) {
      cat("\n")
      cat("Status:            Ready for routing!\n")
      cat("Usage:             Use osrm_routed() to start routing server,\n")
      cat("                   then use the 'osrm' R package for routing queries.\n")
    }
  }

  cat("\nLogs available in: <object>$logs\n")
  cat("------------------------------------------------------\n")
  invisible(x)
}

#' Analyze OSRM pipeline state
#' @noRd
.analyze_pipeline_state <- function(completed_stages) {
  result <- list(
    pipeline = NULL,
    remaining = character(0),
    next_step = NULL,
    alternative_steps = character(0),
    ready_for_routing = FALSE
  )

  has_extract <- "extract" %in% completed_stages
  has_partition <- "partition" %in% completed_stages
  has_contract <- "contract" %in% completed_stages
  has_customize <- "customize" %in% completed_stages

  # Determine pipeline and state
  if (has_extract && !has_partition && !has_contract) {
    # Just extract - two paths available
    result$alternative_steps <- c(
      "osrm_contract() for CH pipeline (extract -> contract)",
      "osrm_partition() for MLD pipeline (extract -> partition -> customize)"
    )
  } else if (has_extract && has_partition && !has_customize) {
    # MLD pipeline in progress
    result$pipeline <- "MLD (Multi-Level Dijkstra)"
    result$remaining <- "customize"
    result$next_step <- "osrm_customize()"
  } else if (has_extract && has_contract) {
    # CH pipeline complete
    result$pipeline <- "CH (Contraction Hierarchies)"
    result$ready_for_routing <- TRUE
  } else if (has_extract && has_partition && has_customize) {
    # MLD pipeline complete
    result$pipeline <- "MLD (Multi-Level Dijkstra)"
    result$ready_for_routing <- TRUE
  }

  result
}

#' Internal helper to extract path from string or osrm_job object
#' @noRd
get_osrm_path_from_input <- function(input) {
  if (inherits(input, "osrm_job")) {
    # Try artifact first if it exists
    if (!is.null(input$osrm_job_artifact) && file.exists(input$osrm_job_artifact)) {
      return(input$osrm_job_artifact)
    }
    # Fall back to working dir
    if (!is.null(input$osrm_working_dir)) {
      return(input$osrm_working_dir)
    }
    stop("osrm_job object has no valid paths", call. = FALSE)
  }
  return(input)
}

#' Internal helper to class return objects
#' @noRd
as_osrm_job <- function(osrm_job_artifact, osrm_working_dir, logs) {
  structure(
    list(
      osrm_job_artifact = osrm_job_artifact,
      osrm_working_dir = osrm_working_dir,
      logs = logs
    ),
    class = "osrm_job"
  )
}
