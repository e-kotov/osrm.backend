## CRAN Submission Comments for osrm.backend 0.4.1

This is a patch release addressing the CRAN check failure notification received on 2026-09-03 regarding graceful handling of network resources.

### Issue addressed

CRAN reported check failures on `r-devel-linux-x86_64-fedora-gcc`, `r-release-macos-x86_64`, `r-oldrel-macos-arm64`, and `r-oldrel-macos-x86_64` caused by unit tests in `tests/testthat/test-osrm_install.R` making unmocked live HTTP requests to GitHub during `R CMD check`. When rate-limited (HTTP 403) or run on older macOS versions, these tests produced check warnings or failures.

### Changes in this release

1. **Deterministic Unit Tests**:
   - Refactored `tests/testthat/test-osrm_install.R` so all unit tests run deterministically offline and on CRAN without requiring network access or GitHub API availability.
   - Used an existing-install fast path and mocked version availability to test validation messages without downloading binaries.
   - Placed archive downloading behind a package-owned internal transport helper (`download_archive_file()`) and mocked it to verify transport error handling without reaching out to public or dummy endpoints.

2. **Early Manual-Install Validation**:
   - Reordered argument processing in `osrm_install()` so manual source parameters (`file_path` and `download_url`) are validated immediately upon invocation.
   - Missing or unreadable local files and invalid URLs now fail cleanly prior to directory creation, version resolution, or GitHub API calls.
   - Bypassed online version lookups and the macOS version gate when manual install sources are provided, consistent with function documentation.

3. **Bounded GitHub API Handling**:
   - Added an explicit 15-second request timeout and bounded retries for API requests.
   - Primary GitHub rate-limit responses (HTTP 403 with `X-RateLimit-Remaining: 0`) fail immediately with actionable reset timestamps and instructions to set `GITHUB_PAT`, eliminating repeated exponential retry backoffs.
   - Extracted pure response classification helpers and added thorough offline unit tests.

4. **User-visible consequences of the reorder** (documented in `NEWS.md` and on the help page):
   - `file_path` and `download_url` supplied together are now rejected instead of silently preferring `file_path`.
   - A manually supplied archive has no resolvable version, so with the default destination it installs into a `manual` subdirectory and does not trigger the automatic v6 runtime library step. Supplying an explicit `version` tag alongside the archive restores both, without any network access.

### Test environments

* macOS Tahoe 26.6.2 (arm64), R 4.6.1
* GitHub Actions CI:
  - macOS (release)
  - Windows (release)
  - Ubuntu Linux (devel, release, oldrel-1 through oldrel-4)
  - Dedicated offline Linux check job mirroring CRAN network constraints

### R CMD check results

0 errors | 0 warnings | 0 notes

(Verified with `R CMD check --as-cran` under strict offline execution: every proxy variable pointed at a closed local port, `NO_PROXY`/`no_proxy` cleared, and `GITHUB_PAT` and `NOT_CRAN` unset. The full test suite runs in under three seconds with no network access.)
