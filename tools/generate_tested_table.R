#!/usr/bin/env Rscript
# Generate a small markdown fragment with tested OSRM releases for the vignette.
# This script expects to be run from the package root.

suppressPackageStartupMessages({
  have_pkgload <- requireNamespace("pkgload", quietly = TRUE)
  have_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)
})

out_md <- "vignettes/generated-tested-versions.md"

write_lines <- function(x, file) {
  cat(paste0(x, collapse = "\n"), file = file)
}

if (!have_jsonlite) {
  message("jsonlite required but not installed; exiting")
  quit(status = 1)
}

# Try to load package code to reuse helper; fall back to direct fetch
use_pkg_helpers <- FALSE
if (have_pkgload) {
  tryCatch({
    pkgload::load_all(.)
    use_pkg_helpers <- exists("osrm_validated_versions")
  }, error = function(e) {
    use_pkg_helpers <<- FALSE
  })
}

get_all <- function() {
  os_list <- c(linux = "osrm_versions_ubuntu.json",
               macos = "osrm_versions_macos.json",
               windows = "osrm_versions_windows.json")
  badges_repo_raw <- function(filename) sprintf("https://raw.githubusercontent.com/e-kotov/osrm.backend/badges/%s", filename)

  res <- list()
  for (os in names(os_list)) {
    fname <- os_list[[os]]
    url <- badges_repo_raw(fname)
    json <- tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    if (is.null(json)) {
      res[[os]] <- list(ok = FALSE, message = "badge not available", raw = NULL)
    } else {
      msg <- json$message %||% ""
      # Prefer expanding using package helper if available
      if (use_pkg_helpers) {
        v <- tryCatch(osrm_validated_versions(os, expand = TRUE), error = function(e) NULL)
        if (!is.null(v) && (is.character(v) || is.list(v))) {
          res[[os]] <- list(ok = TRUE, tags = v, message = msg, raw = json)
        } else {
          res[[os]] <- list(ok = TRUE, tags = NULL, message = msg, raw = json)
        }
      } else {
        res[[os]] <- list(ok = TRUE, tags = NULL, message = msg, raw = json)
      }
    }
  }
  res
}

# Helper to format tags into a short inline list
format_tags <- function(tags, max_items = 8) {
  if (is.null(tags) || length(tags) == 0) return("")
  tags <- unname(unlist(tags))
  tags <- as.character(tags)
  if (length(tags) <= max_items) return(paste(tags, collapse = ", "))
  paste0(paste(tags[1:max_items], collapse = ", "), sprintf(" (+%d more)", length(tags) - max_items))
}

res_all <- get_all()

lines <- c("### Auto-tested OSRM releases (generated)", "", "| OS | Validated releases |", "|---|---|")
for (os in names(res_all)) {
  it <- res_all[[os]]
  if (!isTRUE(it$ok)) {
    row <- sprintf("| %s | %s |", toupper(os), it$message)
  } else {
    if (!is.null(it$tags) && length(it$tags) > 0 && is.character(it$tags)) {
      row <- sprintf("| %s | %s |", toupper(os), format_tags(it$tags))
    } else {
      # fallback to badge message
      msg <- if (!is.null(it$message) && nzchar(it$message)) it$message else "n/a"
      row <- sprintf("| %s | %s |", toupper(os), msg)
    }
  }
  lines <- c(lines, row)
}

# footnote with link to badges branch
lines <- c(lines, "", "Badge source: `badges` branch — raw JSON files (e.g. osrm_versions_ubuntu.json).")

# Ensure vignettes exists
dir.create("vignettes", showWarnings = FALSE)
write_lines(lines, out_md)
cat("Wrote", out_md, "\n")
