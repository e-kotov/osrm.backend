#' Fetch validated OSRM versions from per-OS badge JSON
#'
#' The package's live integration workflow publishes per-OS JSON "badges"
#' (for example `osrm_versions_ubuntu.json`) to the `badges` branch. These
#' files contain a short message describing the validated range or latest
#' validated versions. This helper fetches that JSON and (optionally) expands
#' a range to the list of release tags available in the `e-kotov/osrm-binaries`
#' repository.
#'
#' @param os Character, one of `"linux"`, `"macos"`, `"windows"`, or `"all"`.
#'   `"linux"` reads the `osrm_versions_ubuntu.json` badge. `"all"` returns a
#'   named list with entries for each OS.
#' @param repo Character, repository hosting the badges. Defaults to
#'   `"e-kotov/osrm.backend"`.
#' @param branch Character, the branch where badges are stored. Defaults to
#'   `"badges"`.
#' @param expand Logical, if `TRUE` and the badge message encodes a simple
#'   range (e.g. `v5.27.1 - v26.7.3`), attempt to expand the range to the list
#'   of available release tags from `e-kotov/osrm-binaries`. If expansion
#'   fails, the function falls back to returning the raw badge message.
#' @param gh_token Optional GitHub token for API requests. By default the
#'   function does not use authentication (public raw.githubusercontent URLs).
#' @return If `os != "all"`: a list with elements `label`, `message`, `raw`
#'   (the parsed JSON). If `expand = TRUE` and expansion succeeded, returns a
#'   character vector of tag names (newest first). If `os = "all"`, returns a
#'   named list with one entry per OS.
#' @export
#' @examples
#' \dontrun{
#' osrm_validated_versions("linux")
#' osrm_validated_versions("all", expand = FALSE)
#' }
osrm_validated_versions <- function(os = c("linux", "macos", "windows", "all"),
                                    repo = "e-kotov/osrm.backend",
                                    branch = "badges",
                                    expand = TRUE,
                                    gh_token = NULL) {
  os <- match.arg(os)

  name_for_os <- function(x) {
    switch(x,
           linux = "osrm_versions_ubuntu.json",
           macos = "osrm_versions_macos.json",
           windows = "osrm_versions_windows.json",
           stop("unknown os"))
  }

  fetch_badge <- function(filename) {
    url <- sprintf("https://raw.githubusercontent.com/%s/%s/%s", repo, branch, filename)
    res <- tryCatch({
      jsonlite::fromJSON(url)
    }, error = function(e) {
      stop("Failed to fetch badge JSON from ", url, ": ", conditionMessage(e))
    })
    res
  }

  expand_range_to_tags <- function(message) {
    # Expect "vX.Y.Z - vA.B.C" or comma-separated list; be permissive.
    if (is.null(message) || !nzchar(message)) return(NULL)
    # comma-separated explicit list?
    if (grepl(",", message)) {
      toks <- trimws(strsplit(message, ",")[[1]])
      return(toks[grepl("^v\\d+\\.\\d+\\.\\d+$", toks)])
    }
    if (grepl(" - ", message)) {
      parts <- trimws(strsplit(message, " - ", fixed = TRUE)[[1]])
      if (length(parts) == 2 &&
          grepl("^v\\d+\\.\\d+\\.\\d+$", parts[1]) &&
          grepl("^v\\d+\\.\\d+\\.\\d+$", parts[2])) {
        from <- sub("^v", "", parts[1])
        to <- sub("^v", "", parts[2])
        # fetch releases from e-kotov/osrm-binaries
        releases_url <- "https://api.github.com/repos/e-kotov/osrm-binaries/releases?per_page=200"
        releases <- tryCatch({
          jsonlite::fromJSON(releases_url)
        }, error = function(e) NULL)
        if (is.null(releases) || length(releases) == 0) return(NULL)
        tags <- vapply(releases, function(x) x$tag_name %||% NA_character_, FUN.VALUE = "")
        tags <- tags[grepl("^v\\d+\\.\\d+\\.\\d+$", tags)]
        if (length(tags) == 0) return(NULL)
        # numeric compare by package_version
        tag_versions <- vapply(sub("^v", "", tags), function(x) tryCatch(utils::packageVersion(x), error = function(e) NA), FUN.VALUE = numeric_version(1))
        from_v <- utils::packageVersion(from)
        to_v <- utils::packageVersion(to)
        keep <- vapply(tag_versions, function(tv) {
          !is.na(tv) && tv >= from_v && tv <= to_v
        }, logical(1))
        out <- tags[keep]
        # sort newest first by version
        if (length(out) > 0) {
          # sort newest first by numeric version parts (major, minor, patch)
          parts_mat <- t(sapply(sub("^v", "", out), function(x) {
            p <- as.integer(strsplit(x, "\\.")[[1]])
            length(p) <- 3
            p[is.na(p)] <- 0L
            p
          }))
          ord <- order(parts_mat[,1], parts_mat[,2], parts_mat[,3], decreasing = TRUE)
          out <- out[ord]
        }
        return(out)
      }
    }
    return(NULL)
  }

  if (os == "all") {
    res <- list()
    for (o in c("linux", "macos", "windows")) {
      filename <- name_for_os(o)
      json <- tryCatch(fetch_badge(filename), error = function(e) NULL)
      if (is.null(json)) {
        res[[o]] <- NULL
      } else {
        msg <- json$message %||% ""
        if (isTRUE(expand)) {
          tags <- tryCatch(expand_range_to_tags(msg), error = function(e) NULL)
          if (!is.null(tags)) {
            res[[o]] <- tags
          } else {
            res[[o]] <- list(label = json$label %||% NA_character_, message = msg, raw = json)
          }
        } else {
          res[[o]] <- list(label = json$label %||% NA_character_, message = msg, raw = json)
        }
      }
    }
    return(res)
  }

  filename <- name_for_os(os)
  json <- fetch_badge(filename)
  label <- json$label %||% NA_character_
  message <- json$message %||% ""
  if (isTRUE(expand)) {
    tags <- expand_range_to_tags(message)
    if (!is.null(tags)) return(tags)
  }
  list(label = label, message = message, raw = json)
}
