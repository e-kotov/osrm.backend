#!/usr/bin/env Rscript
# Generate a small HTML/Markdown fragment with tested OSRM releases for the vignette,
# and update README.md with the same content so it appears on GitHub and pkgdown.
# This script expects to be run from the package root.

suppressPackageStartupMessages({
  have_pkgload <- requireNamespace("pkgload", quietly = TRUE)
  have_jsonlite <- requireNamespace("jsonlite", quietly = TRUE)
})

# null-coalesce helper (avoid requiring rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x

if (!have_jsonlite) {
  message("jsonlite required but not installed; exiting")
  quit(status = 1)
}

out_vignette <- "vignettes/generated-tested-versions.md"
out_readme_fragment <- "README-generated-tested-versions.md"

color_map <- function(key) {
  key <- tolower(as.character(key))
  switch(key,
         success = "#4c1",
         critical = "#e05d44",
         important = "#fe7f2d",
         "#eeeeee")
}

badges_repo_raw <- function(filename) {
  sprintf("https://raw.githubusercontent.com/e-kotov/osrm.backend/badges/%s", filename)
}

name_for_os <- function(x) {
  switch(x,
         linux = "osrm_versions_ubuntu.json",
         macos = "osrm_versions_macos.json",
         windows = "osrm_versions_windows.json",
         stop("unknown os"))
}

expand_range_to_tags <- function(message) {
  if (is.null(message) || !nzchar(message)) return(NULL)
  # comma-separated explicit list?
  if (grepl(",", message)) {
    toks <- trimws(strsplit(message, ",")[[1]])
    toks <- toks[grepl("^v\\d+\\.\\d+\\.\\d+$", toks)]
    return(if (length(toks)) toks else NULL)
  }
  if (grepl(" - ", message)) {
    parts <- trimws(strsplit(message, " - ", fixed = TRUE)[[1]])
    if (length(parts) == 2 &&
        grepl("^v\\d+\\.\\d+\\.\\d+$", parts[1]) &&
        grepl("^v\\d+\\.\\d+\\.\\d+$", parts[2])) {
      # Query releases from e-kotov/osrm-binaries (public)
      releases_url <- "https://api.github.com/repos/e-kotov/osrm-binaries/releases?per_page=200"
      releases <- tryCatch(jsonlite::fromJSON(releases_url), error = function(e) NULL)
      if (is.null(releases) || length(releases) == 0) return(NULL)
      tags <- vapply(releases, function(x) x$tag_name %||% NA_character_, FUN.VALUE = "")
      tags <- tags[grepl("^v\\d+\\.\\d+\\.\\d+$", tags)]
      if (length(tags) == 0) return(NULL)
      # numeric compare by package_version
      from_v <- utils::packageVersion(sub("^v", "", parts[1]))
      to_v <- utils::packageVersion(sub("^v", "", parts[2]))
      good <- vapply(tags, function(t) {
        tv <- tryCatch(utils::packageVersion(sub("^v", "", t)), error = function(e) NA)
        !is.na(tv) && (tv >= from_v) && (tv <= to_v)
      }, logical(1))
      out <- tags[good]
      if (length(out) == 0) return(NULL)
      # sort newest first
      parts_mat <- t(sapply(sub("^v", "", out), function(x) {
        p <- as.integer(strsplit(x, "\\.")[[1]])
        length(p) <- 3
        p[is.na(p)] <- 0L
        p
      }))
      ord <- order(parts_mat[,1], parts_mat[,2], parts_mat[,3], decreasing = TRUE)
      return(out[ord])
    }
  }
  NULL
}

fetch_badge_json <- function(filename) {
  url <- badges_repo_raw(filename)
  tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
}

use_pkg_helpers <- FALSE
if (have_pkgload) {
  tryCatch({
    pkgload::load_all(".")
    use_pkg_helpers <- exists("osrm_validated_versions")
  }, error = function(e) {
    use_pkg_helpers <<- FALSE
  })
}

os_list <- c(linux = "osrm_versions_ubuntu.json",
             macos = "osrm_versions_macos.json",
             windows = "osrm_versions_windows.json")

rows <- list()
for (os in names(os_list)) {
  fname <- os_list[[os]]
  json <- fetch_badge_json(fname)
  if (is.null(json)) {
    rows[[os]] <- list(ok = FALSE, message = "badge not available", color = "#eeeeee", tags = NULL, badge_url = badges_repo_raw(fname))
    next
  }
  label <- json$label %||% toupper(os)
  message <- json$message %||% ""
  color <- color_map(json$color %||% "")
  badge_img <- sprintf("https://img.shields.io/endpoint?url=%s", utils::URLencode(badges_repo_raw(fname), reserved = TRUE))

  tags <- NULL
  if (use_pkg_helpers) {
    tags_try <- tryCatch(osrm_validated_versions(os, expand = TRUE), error = function(e) NULL)
    if (!is.null(tags_try) && (is.character(tags_try) || is.list(tags_try))) {
      tags <- unname(unlist(tags_try))
    }
  }
  if (is.null(tags) || length(tags) == 0) {
    tags <- expand_range_to_tags(message)
  }
  rows[[os]] <- list(ok = TRUE, label = label, message = message, color = color, badge = badge_img, tags = tags, raw = json)
}

format_tags_links <- function(tags, max_items = 12) {
  if (is.null(tags) || length(tags) == 0) return("")
  tags <- unname(as.character(tags))
  link_for_tag <- function(t) {
    href <- sprintf("https://github.com/e-kotov/osrm-binaries/releases/tag/%s", t)
    sprintf("<a href=\"%s\"><code>%s</code></a>", href, t)
  }
  links <- vapply(tags, link_for_tag, FUN.VALUE = character(1))
  if (length(links) <= max_items) return(paste(links, collapse = ", "))
  paste0(paste(links[1:max_items], collapse = ", "), sprintf(" (+%d more)", length(links) - max_items))
}

# Build HTML table
html_lines <- c()
html_lines <- c(html_lines, "<div class='auto-tested-versions'>")
html_lines <- c(html_lines, "<h3>Auto-tested OSRM releases (generated)</h3>")
html_lines <- c(html_lines, "<table>")
html_lines <- c(html_lines, "<thead><tr><th>OS</th><th>Badge</th><th>Validated releases</th><th>Notes</th></tr></thead>")
html_lines <- c(html_lines, "<tbody>")
for (os in names(rows)) {
  it <- rows[[os]]
  bgcolor <- it$color %||% "#ffffff"
  os_label <- toupper(os)
  badge_html <- if (!is.null(it$badge)) sprintf("<img src=\"%s\" alt=\"%s badge\" />", it$badge, os_label) else ""
  tags_html <- if (!is.null(it$tags) && length(it$tags) > 0) format_tags_links(it$tags) else ""
  # minimal HTML-escape for notes
  notes <- if (!is.null(it$message) && nzchar(it$message)) {
    nm <- as.character(it$message)
    nm <- gsub("&", "&amp;", nm, fixed = TRUE)
    nm <- gsub("<", "&lt;", nm, fixed = TRUE)
    nm <- gsub(">", "&gt;", nm, fixed = TRUE)
    nm
  } else ""
  # create row; allow inline HTML in markdown
  row_html <- sprintf("<tr style=\"background-color:%s\"><td><strong>%s</strong></td><td>%s</td><td>%s</td><td>%s</td></tr>",
                      bgcolor, os_label, badge_html, tags_html, notes)
  html_lines <- c(html_lines, row_html)
}
html_lines <- c(html_lines, "</tbody></table>")
html_lines <- c(html_lines, "<p>Badge source: <code>badges</code> branch — raw JSON files (e.g. <code>osrm_versions_ubuntu.json</code>).</p>")
html_lines <- c(html_lines, "</div>")

# write vignette fragment
dir.create("vignettes", showWarnings = FALSE)
writeLines(html_lines, out_vignette)
cat("Wrote", out_vignette, "\n")

# update README.md (replace markers if present, otherwise append the section)
readme_file <- "README.md"
start_marker <- "<!-- AUTO_TESTED_VERSIONS_START -->"
end_marker <- "<!-- AUTO_TESTED_VERSIONS_END -->"

frag_text <- c("", start_marker, "", html_lines, "", end_marker, "")

if (file.exists(readme_file)) {
  rd <- readLines(readme_file)
  if (any(grepl(start_marker, rd, fixed = TRUE)) && any(grepl(end_marker, rd, fixed = TRUE))) {
    s <- which(grepl(start_marker, rd, fixed = TRUE))[1]
    e <- which(grepl(end_marker, rd, fixed = TRUE))[1]
    new_rd <- c(rd[1:(s-1)], frag_text, rd[(e+1):length(rd)])
  } else {
    # append
    new_rd <- c(rd, "", "", frag_text)
  }
  writeLines(new_rd, readme_file)
  cat("Updated", readme_file, "\n")
} else {
  # create simple README with fragment
  writeLines(c("# osrm.backend", "", frag_text), readme_file)
  cat("Created", readme_file, "\n")
}
