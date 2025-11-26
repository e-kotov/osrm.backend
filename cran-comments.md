## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Test environment notes

* Most examples are wrapped with `donttest{}`, as they require OSRM binaries.

* Some tests use `testthat::with_mocked_bindings()` for object mocking, which
  requires testthat >= 3.2.0. These tests are automatically skipped on systems
  with older testthat versions (e.g., R oldrel on Windows). This ensures
  compatibility across all R versions without affecting package functionality.

## Vignette notes

* The package includes a Quarto vignette that requires Quarto to be installed
  for building. On systems without Quarto, R CMD check may report a WARNING
  about files in 'vignettes' directory but no files in 'inst/doc'. This is
  expected behavior and does not affect package functionality.
