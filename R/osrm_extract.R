#' Extract OSM into OSRM Graph Files
#'
#' Run the `osrm-extract` tool to preprocess an OSM file
#' (`.osm`, `.osm.bz2`, or `.osm.pbf`) into the base `.osrm` graph files
#' using a specified Lua profile.  After running, a companion
#' `<base>.osrm.timestamp` file must exist to confirm success.
#'
#' @param input_osm A string. Path to the input OSM file:
#'   `.osm`, `.osm.bz2`, or `.osm.pbf`.
#' @param profile A string. Path to the OSRM Lua profile
#'   (e.g. returned by \code{osrm_find_profile("car.lua")}).
#' @param threads An integer. Number of threads for
#'   \code{-t/--threads}; default \code{8} (OSRM's default).
#' @param overwrite A logical. If \code{FALSE} (default), stops when any
#'   existing `.osrm*` files matching the base name are found alongside
#'   \code{input_osm}. Set to \code{TRUE} to proceed regardless.
#' @param verbosity A string. Log verbosity level passed to
#'   \code{-l/--verbosity} (one of \code{"NONE","ERROR","WARNING","INFO","DEBUG"}),
#'   default \code{"INFO"}.
#' @param data_version A string or \code{NULL}. Passed to
#'   \code{-d/--data_version}; default \code{NULL}, in which case the option
#'   is omitted.
#' @param small_component_size An integer. For
#'   \code{--small-component-size}; default \code{1000} (OSRM's default).
#' @param with_osm_metadata A logical. If \code{TRUE}, adds
#'   \code{--with-osm-metadata}; default \code{FALSE}.
#' @param parse_conditional_restrictions A logical. If \code{TRUE}, adds
#'   \code{--parse-conditional-restrictions}; default \code{FALSE}.
#' @param location_dependent_data A string or \code{NULL}. Path to GeoJSON,
#'   passed to \code{--location-dependent-data}; default \code{NULL}, in which
#'   case the option is omitted.
#' @param disable_location_cache A logical. If \code{TRUE}, adds
#'   \code{--disable-location-cache}; default \code{FALSE}.
#' @param dump_nbg_graph A logical. If \code{TRUE}, adds
#'   \code{--dump-nbg-graph}; default \code{FALSE}.
#' @inheritParams osrm_prepare_graph
#'
#' @return A list with two elements:
#' \describe{
#'   \item{osrm_path}{The expected path to the generated `.osrm` base,
#'     i.e. the timestamp file path with `.timestamp` dropped.}
#'   \item{logs}{The \code{processx::run} result object.}
#' }
#'
#' @examples
#' \donttest{
#' # install osrm and set up PATH for the session
#' osrm_executable <- osrm_install(
#'  version = "v5.27.1",
#'  path_action = "session"
#' )
#' # copy example OSM PBF into a temporary workspace to avoid polluting pkg data
#' workspace <- tempdir()
#' tmp_prefix <- file.path(workspace, paste0("osrm-example-", Sys.getpid()))
#' pbf_path <- system.file("extdata/cur.osm.pbf", package = "osrm.backend")
#' tmp_pbf <- paste0(tmp_prefix, ".osm.pbf")
#' file.copy(pbf_path, tmp_pbf, overwrite = TRUE)
#' # extract OSRM graph files
#' result <- osrm_extract(
#'   input_osm                  = tmp_pbf,
#'   profile                    = osrm_find_profile("car.lua"),
#'   overwrite                  = TRUE,
#'   threads                    = 1L
#' )
#' # path to generated .osrm files (specifically, the .osrm.timestamp file)
#' result$osrm_path
#' # clean up the temporary workspace
#' unlink(generated, recursive = TRUE)
#' }
#' @export
osrm_extract <- function(
  input_osm,
  profile = osrm_find_profile("car.lua"),
  threads = 8L,
  overwrite = FALSE,
  verbosity = c("INFO", "NONE", "ERROR", "WARNING", "DEBUG"),
  data_version = NULL,
  small_component_size = 1000L,
  with_osm_metadata = FALSE,
  parse_conditional_restrictions = FALSE,
  location_dependent_data = NULL,
  disable_location_cache = FALSE,
  dump_nbg_graph = FALSE,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required for osrm_extract", call. = FALSE)
  }

  # normalize and verify input
  input_osm <- normalizePath(input_osm, mustWork = TRUE)

  # strip recognized extensions to derive base path
  if (grepl("\\.osm\\.pbf$", input_osm, ignore.case = TRUE)) {
    base <- sub("\\.osm\\.pbf$", "", input_osm, ignore.case = TRUE)
  } else if (grepl("\\.osm\\.bz2$", input_osm, ignore.case = TRUE)) {
    base <- sub("\\.osm\\.bz2$", "", input_osm, ignore.case = TRUE)
  } else if (grepl("\\.osm$", input_osm, ignore.case = TRUE)) {
    base <- sub("\\.osm$", "", input_osm, ignore.case = TRUE)
  } else {
    stop(
      "'input_osm' must have extension .osm, .osm.bz2, or .osm.pbf",
      call. = FALSE
    )
  }

  # check for existing .osrm* files
  existing <- list.files(
    dirname(base),
    pattern = paste0("^", basename(base), ".*\\.osrm"),
    ignore.case = TRUE
  )
  if (length(existing) > 0 && !overwrite) {
    stop(
      "Found existing OSRM files: ",
      paste(existing, collapse = ", "),
      ".\nSet overwrite = TRUE to proceed.",
      call. = FALSE
    )
  }

  # build command arguments
  verbosity <- match.arg(verbosity)
  arguments <- c(
    input_osm,
    "-p",
    profile,
    "-l",
    verbosity,
    "-t",
    as.character(threads),
    "--small-component-size",
    as.character(small_component_size)
  )
  if (!is.null(data_version)) {
    arguments <- c(arguments, "-d", data_version)
  }
  if (with_osm_metadata) {
    arguments <- c(arguments, "--with-osm-metadata")
  }
  if (parse_conditional_restrictions) {
    arguments <- c(arguments, "--parse-conditional-restrictions")
  }
  if (!is.null(location_dependent_data)) {
    arguments <- c(
      arguments,
      "--location-dependent-data",
      location_dependent_data
    )
  }
  if (disable_location_cache) {
    arguments <- c(arguments, "--disable-location-cache")
  }
  if (dump_nbg_graph) {
    arguments <- c(arguments, "--dump-nbg-graph")
  }

  # run extraction
  logs <- processx::run(
    "osrm-extract",
    args = arguments,
    echo = echo,
    spinner = spinner,
    echo_cmd = echo_cmd
  )

  # verify timestamp file
  timestamp_file <- paste0(base, ".osrm.timestamp")
  if (!file.exists(timestamp_file)) {
    stop(
      "Extraction did not produce timestamp file: ",
      basename(timestamp_file),
      call. = FALSE
    )
  }

  # expected .osrm base is timestamp file minus '.timestamp'
  # osrm_path <- sub("\\.timestamp$", "", timestamp_file)

  return(
    invisible(
      list(
        osrm_path = timestamp_file,
        logs = logs
      )
    )
  )
}
