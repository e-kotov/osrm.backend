This update addresses recent check failures on Fedora and Debian:

*   **Fixed Detritus/Cache Issue:** Tests were updated to use `tempdir()` for binary storage instead of the user cache directory.
*   **Fixed Vignette Build:** Switched vignette format from Quarto (`.qmd`) to R Markdown (`.Rmd`/`knitr`) to resolve missing system dependency errors on Debian.
*   **Removed examples from exported functions:** Examples were removed from exported functions as advised by CRAN maintainers.
