# This script generates a badge JSON for Shields.io based on OSRM version tests.
# It identifies passing versions and handles partial failures with color coding.

args <- commandArgs(trailingOnly = TRUE)
os_label <- if (length(args) > 0) args[1] else "OSRM tested"
output_file <- if (length(args) > 1) args[2] else "osrm_versions.json"

pkgload::load_all(".")

# 1. Discover all versions that SHOULD be tested
all_tags <- osrm_check_available_versions(prereleases = TRUE)
v5_tags <- all_tags[grepl("^v5\\.", all_tags)]
latest_v5 <- if (length(v5_tags) > 0) v5_tags[1] else NULL
v6_plus_tags <- all_tags[!grepl("^v5\\.", all_tags)]
target_versions <- unique(c(latest_v5, v6_plus_tags))

if (length(target_versions) == 0) {
  stop("No OSRM versions found to test.")
}

# 2. Determine which versions actually passed
results_file <- Sys.getenv("OSRM_TEST_RESULTS_FILE", "test_results.rds")

# Helper to parse versions for comparison
parse_v <- function(v) {
  # Remove 'v' and any non-numeric suffixes (like -rc1) for sorting
  v_clean <- gsub("^v", "", v)
  v_numeric <- gsub("[-].*$", "", v_clean)
  parts <- as.numeric(unlist(strsplit(v_numeric, "[.]")))
  parts <- c(parts, rep(0, 3 - length(parts)))
  # Handle potential NAs from malformed strings
  parts[is.na(parts)] <- 0
  sum(parts * c(1e6, 1e3, 1))
}

if (!file.exists(results_file)) {
  # Catastrophic failure: tests didn't even run or crashed before saving
  badge_data <- list(
    schemaVersion = 1,
    label = os_label,
    message = "test suite failed",
    color = "critical"
  )
} else {
  results <- readRDS(results_file)
  # results should be a named logical vector: c("v5.27.1" = TRUE, "v26.5.0" = FALSE)
  
  passing_versions <- names(results)[results]
  failing_versions <- names(results)[!results]
  
  if (length(passing_versions) == 0) {
    badge_data <- list(
      schemaVersion = 1,
      label = os_label,
      message = "all failing",
      color = "critical"
    )
  } else {
    v_values <- sapply(passing_versions, parse_v)
    v_min <- passing_versions[which.min(v_values)]
    v_max <- passing_versions[which.max(v_values)]
    
    range_msg <- if (v_min == v_max) v_min else paste0(v_min, " - ", v_max)
    
    # Logic for color and extra info
    if (length(failing_versions) == 0) {
      color <- "success"
      message <- range_msg
    } else {
      # Is the latest one failing?
      all_v_values <- sapply(target_versions, parse_v)
      latest_ver <- target_versions[which.max(all_v_values)]
      
      if (latest_ver %in% failing_versions) {
        color <- "critical"
        message <- paste0(range_msg, " (", latest_ver, " failing)")
      } else {
        color <- "important" # Yellow/Orange
        message <- paste0(range_msg, " (partial)")
      }
    }
    
    badge_data <- list(
      schemaVersion = 1,
      label = os_label,
      message = message,
      color = color
    )
  }
}

jsonlite::write_json(badge_data, output_file, auto_unbox = TRUE)
cat("Generated badge with message:", badge_data$message, "and color:", badge_data$color, "\n")
