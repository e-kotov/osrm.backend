# Tests for osrm_install.R - comprehensive coverage

# Test platform detection ----
test_that("get_platform_info detects Windows x86-64 machines", {
  fake_sys_info <- c(
    sysname = "Windows",
    machine = "x86-64"
  )

  platform <- get_platform_info(sys_info = fake_sys_info)

  expect_identical(platform, list(os = "win32", arch = "x64"))
})

test_that("get_platform_info detects Linux x86_64", {
  fake_sys_info <- c(
    sysname = "Linux",
    machine = "x86_64"
  )

  platform <- get_platform_info(sys_info = fake_sys_info)
  expect_identical(platform, list(os = "linux", arch = "x64"))
})

test_that("get_platform_info detects macOS arm64", {
  fake_sys_info <- c(
    sysname = "Darwin",
    machine = "arm64"
  )

  platform <- get_platform_info(sys_info = fake_sys_info)
  expect_identical(platform, list(os = "darwin", arch = "arm64"))
})

test_that("get_platform_info detects macOS x86_64", {
  fake_sys_info <- c(
    sysname = "Darwin",
    machine = "x86_64"
  )

  platform <- get_platform_info(sys_info = fake_sys_info)
  expect_identical(platform, list(os = "darwin", arch = "x64"))
})

test_that("get_platform_info handles alternative architecture names", {
  # Test amd64 -> x64
  platform <- get_platform_info(
    sys_info = c(sysname = "Linux", machine = "amd64")
  )
  expect_identical(platform$arch, "x64")

  # Test i386 -> x86
  platform <- get_platform_info(
    sys_info = c(sysname = "Linux", machine = "i386")
  )
  expect_identical(platform$arch, "x86")

  # Test i686 -> x86
  platform <- get_platform_info(
    sys_info = c(sysname = "Linux", machine = "i686")
  )
  expect_identical(platform$arch, "x86")

  # Test aarch64 -> arm64
  platform <- get_platform_info(
    sys_info = c(sysname = "Linux", machine = "aarch64")
  )
  expect_identical(platform$arch, "arm64")
})

test_that("get_platform_info respects override options", {
  # Store original options
  orig_os <- getOption("osrm.backend.override_os")
  orig_arch <- getOption("osrm.backend.override_arch")
  on.exit({
    options(osrm.backend.override_os = orig_os)
    options(osrm.backend.override_arch = orig_arch)
  })

  # Set overrides
  options(osrm.backend.override_os = "linux")
  options(osrm.backend.override_arch = "arm64")

  platform <- get_platform_info(
    sys_info = c(sysname = "Windows", machine = "x86_64")
  )

  expect_identical(platform, list(os = "linux", arch = "arm64"))
})

test_that("get_platform_info errors on unsupported platforms", {
  expect_error(
    get_platform_info(
      sys_info = c(sysname = "FreeBSD", machine = "x86_64")
    ),
    "not supported"
  )
})

test_that("get_platform_info errors on empty override options", {
  orig_os <- getOption("osrm.backend.override_os")
  on.exit(options(osrm.backend.override_os = orig_os))

  options(osrm.backend.override_os = "")
  expect_error(
    get_platform_info(),
    "must be a non-empty string"
  )
})

# Test macOS version detection ----
test_that("get_macos_release_info detects macOS Sequoia", {
  fake_sys_info <- c(
    sysname = "Darwin",
    release = "24.0.0"
  )

  info <- get_macos_release_info(fake_sys_info)

  expect_identical(info$darwin_major, 24L)
  expect_identical(info$marketing_version, "15")
  expect_identical(info$marketing_codename, "Sequoia")
  expect_true(info$meets_v6_requirement)
})

test_that("get_macos_release_info detects macOS Ventura", {
  fake_sys_info <- c(
    sysname = "Darwin",
    release = "22.6.0"
  )

  info <- get_macos_release_info(fake_sys_info)

  expect_identical(info$darwin_major, 22L)
  expect_identical(info$marketing_version, "13")
  expect_identical(info$marketing_codename, "Ventura")
  expect_false(info$meets_v6_requirement)
})

test_that("get_macos_release_info returns NA for non-Darwin systems", {
  fake_sys_info <- c(
    sysname = "Linux",
    release = "5.15.0"
  )

  info <- get_macos_release_info(fake_sys_info)

  expect_true(is.na(info$release))
  expect_true(is.na(info$darwin_major))
  expect_true(info$meets_v6_requirement) # Should be TRUE for non-macOS
})

# Test asset URL finding ----
test_that("find_asset_url prefers latest Node ABI for matching platform", {
  release_info <- list(
    tag_name = "v5.27.1",
    assets = list(
      list(
        name = "node_osrm-v5.27.1-node-v93-darwin-arm64-Release.tar.gz",
        browser_download_url = "https://example.com/node-v93"
      ),
      list(
        name = "node_osrm-v5.27.1-node-v108-darwin-arm64-Release.tar.gz",
        browser_download_url = "https://example.com/node-v108"
      )
    )
  )

  platform <- list(os = "darwin", arch = "arm64")

  selected <- find_asset_url(release_info, platform)
  expect_identical(selected, "https://example.com/node-v108")
})

test_that("find_asset_url selects asset without node version", {
  release_info <- list(
    tag_name = "v6.0.0",
    assets = list(
      list(
        name = "osrm-v6.0.0-linux-x64-Release.tar.gz",
        browser_download_url = "https://example.com/linux-x64"
      )
    )
  )

  platform <- list(os = "linux", arch = "x64")

  selected <- find_asset_url(release_info, platform)
  expect_identical(selected, "https://example.com/linux-x64")
})

test_that("find_asset_url errors when no matching asset", {
  release_info <- list(
    tag_name = "v5.27.1",
    assets = list(
      list(
        name = "node_osrm-v5.27.1-linux-x64-Release.tar.gz",
        browser_download_url = "https://example.com/linux"
      )
    )
  )

  platform <- list(os = "darwin", arch = "arm64")

  expect_error(
    find_asset_url(release_info, platform),
    "Could not find a compatible binary"
  )
})

# Test version comparison ----
test_that("version_at_least correctly compares versions", {
  expect_true(version_at_least("v6.0.0", "v5.27.1"))
  expect_true(version_at_least("v6.0.0", "v6.0.0"))
  expect_false(version_at_least("v5.27.1", "v6.0.0"))
  expect_true(version_at_least("v6.1.0", "v6.0.0"))
})

test_that("version_at_least handles version tags without 'v'", {
  expect_true(version_at_least("6.0.0", "5.27.1"))
  expect_true(version_at_least("v6.0.0", "5.27.1"))
  expect_true(version_at_least("6.0.0", "v5.27.1"))
})

test_that("version_at_least handles invalid versions", {
  expect_false(version_at_least(NULL, "v6.0.0"))
  expect_false(version_at_least("v6.0.0", NULL))
  expect_false(version_at_least("", "v6.0.0"))
  expect_false(version_at_least("invalid", "v6.0.0"))
})

# Test release filtering ----
test_that("release_has_binaries detects binaries correctly", {
  release <- list(
    assets = list(
      list(name = "node_osrm-v5.27.1-node-v108-darwin-arm64-Release.tar.gz"),
      list(name = "node_osrm-v5.27.1-node-v108-linux-x64-Release.tar.gz")
    )
  )

  platform_darwin <- list(os = "darwin", arch = "arm64")
  platform_linux <- list(os = "linux", arch = "x64")
  platform_win <- list(os = "win32", arch = "x64")

  expect_true(release_has_binaries(release, platform_darwin))
  expect_true(release_has_binaries(release, platform_linux))
  expect_false(release_has_binaries(release, platform_win))
})

# Test path helpers ----
test_that("osrm_default_install_root returns cache directory", {
  root <- osrm_default_install_root()
  expect_true(is.character(root))
  expect_true(nzchar(root))
  expect_true(grepl("osrm.backend", root, fixed = TRUE))
})

test_that("osrm_try_normalize_path handles valid paths", {
  path <- tempdir()
  normalized <- osrm_try_normalize_path(path)
  expect_true(is.character(normalized))
  expect_true(nzchar(normalized))
})

test_that("osrm_try_normalize_path handles errors gracefully", {
  path <- "/nonexistent/path/that/does/not/exist"
  normalized <- osrm_try_normalize_path(path)
  expect_identical(normalized, path)
})

test_that("osrm_is_install_dir detects valid installation", {
  # Create temporary directory with osrm-routed
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file.create(file.path(tmp_dir, "osrm-routed"))

  expect_true(osrm_is_install_dir(tmp_dir))
})

test_that("osrm_is_install_dir returns FALSE for non-installation", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  expect_false(osrm_is_install_dir(tmp_dir))
  expect_false(osrm_is_install_dir(NULL))
  expect_false(osrm_is_install_dir(character(0)))
})

test_that("osrm_detect_installations finds valid installations", {
  # Create a mock install root
  tmp_root <- tempfile()
  dir.create(tmp_root)
  on.exit(unlink(tmp_root, recursive = TRUE))

  # Create two version directories
  v1_dir <- file.path(tmp_root, "v5.27.1")
  v2_dir <- file.path(tmp_root, "v6.0.0")
  dir.create(v1_dir)
  dir.create(v2_dir)

  file.create(file.path(v1_dir, "osrm-routed"))
  file.create(file.path(v2_dir, "osrm-routed"))

  installations <- osrm_detect_installations(tmp_root)

  expect_length(installations, 2)
  expect_true(all(grepl(
    "osrm-routed",
    list.files(installations, recursive = FALSE)
  )))
})

# Test PATH setting (without actually modifying PATH) ----
test_that("set_path_session adds directory to PATH", {
  orig_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = orig_path))

  tmp_dir <- tempdir()
  set_path_session(tmp_dir)

  new_path <- Sys.getenv("PATH")
  expect_true(grepl(
    normalizePath(tmp_dir, mustWork = FALSE),
    new_path,
    fixed = TRUE
  ))
})

test_that("set_path_session doesn't duplicate paths", {
  orig_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = orig_path))

  tmp_dir <- tempdir()
  set_path_session(tmp_dir)
  path_after_first <- Sys.getenv("PATH")

  set_path_session(tmp_dir)
  path_after_second <- Sys.getenv("PATH")

  expect_identical(path_after_first, path_after_second)
})

# Test .Rprofile manipulation ----
test_that("osrm_clear_path removes added paths from .Rprofile", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  orig_wd <- getwd()
  on.exit({
    setwd(orig_wd)
    unlink(tmp_dir, recursive = TRUE)
  })

  setwd(tmp_dir)

  # Create .Rprofile with our tag
  rprofile <- file.path(tmp_dir, ".Rprofile")
  writeLines(
    c(
      "# Some existing code",
      'Sys.setenv(PATH = paste("/some/path", Sys.getenv("PATH"), sep = ":")) #added-by-r-pkg-osrm.backend',
      "# More code"
    ),
    rprofile
  )

  result <- osrm_clear_path(quiet = TRUE)

  expect_true(result)
  lines <- readLines(rprofile)
  expect_false(any(grepl("#added-by-r-pkg-osrm.backend", lines, fixed = TRUE)))
  expect_true(any(grepl("Some existing code", lines, fixed = TRUE)))
})

test_that("osrm_clear_path handles missing .Rprofile", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  orig_wd <- getwd()
  on.exit({
    setwd(orig_wd)
    unlink(tmp_dir, recursive = TRUE)
  })

  setwd(tmp_dir)

  result <- osrm_clear_path(quiet = TRUE)
  expect_false(result)
})

test_that("osrm_clear_path handles .Rprofile without our tags", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  orig_wd <- getwd()
  on.exit({
    setwd(orig_wd)
    unlink(tmp_dir, recursive = TRUE)
  })

  setwd(tmp_dir)

  rprofile <- file.path(tmp_dir, ".Rprofile")
  writeLines("# Some code", rprofile)

  result <- osrm_clear_path(quiet = TRUE)
  expect_false(result)
})

# Test set_path_project ----
# Test set_path_project ----
test_that("set_path_project creates .Rprofile if missing", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  orig_wd <- getwd()
  on.exit({
    setwd(orig_wd)
    unlink(tmp_dir, recursive = TRUE)
  })

  setwd(tmp_dir)

  set_path_project(tempdir(), quiet = TRUE)

  rprofile <- file.path(tmp_dir, ".Rprofile")
  expect_true(file.exists(rprofile))
  lines <- readLines(rprofile)
  expect_true(any(grepl("#added-by-r-pkg-osrm.backend", lines, fixed = TRUE)))
})

test_that("set_path_project doesn't duplicate entries", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  orig_wd <- getwd()
  on.exit({
    setwd(orig_wd)
    unlink(tmp_dir, recursive = TRUE)
  })

  setwd(tmp_dir)

  set_path_project(tempdir(), quiet = TRUE)
  set_path_project(tempdir(), quiet = TRUE)

  rprofile <- file.path(tmp_dir, ".Rprofile")
  lines <- readLines(rprofile)
  count <- sum(grepl("#added-by-r-pkg-osrm.backend", lines, fixed = TRUE))
  expect_identical(count, 1L)
})

# Test handle_path_setting dispatcher ----
test_that("handle_path_setting calls correct function", {
  orig_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = orig_path))

  tmp_dir <- tempdir()

  # Test "session"
  handle_path_setting("session", tmp_dir, quiet = TRUE)
  expect_true(grepl(
    normalizePath(tmp_dir, mustWork = FALSE),
    Sys.getenv("PATH"),
    fixed = TRUE
  ))

  # Test "none" - should not error
  expect_silent(handle_path_setting(
    "none",
    tmp_dir,
    quiet = TRUE
  ))
})

# Test runtime installation helpers ----
test_that("maybe_install_windows_v6_runtime only runs on Windows", {
  platform_linux <- list(os = "linux", arch = "x64")
  expect_silent(
    maybe_install_windows_v6_runtime(
      "v6.0.0",
      platform_linux,
      tempdir()
    )
  )
})

test_that("maybe_install_linux_v6_runtime only runs on Linux for v6+", {
  platform_darwin <- list(os = "darwin", arch = "arm64")
  expect_silent(
    maybe_install_linux_v6_runtime(
      "v6.0.0",
      platform_darwin,
      tempdir()
    )
  )

  platform_linux <- list(os = "linux", arch = "x64")
  expect_silent(
    maybe_install_linux_v6_runtime(
      "v5.27.1",
      platform_linux,
      tempdir()
    )
  )
})

test_that("maybe_install_macos_v6_runtime only runs on macOS for v6+", {
  platform_linux <- list(os = "linux", arch = "x64")
  expect_silent(
    maybe_install_macos_v6_runtime(
      "v6.0.0",
      platform_linux,
      tempdir()
    )
  )

  platform_darwin <- list(os = "darwin", arch = "arm64")
  expect_silent(
    maybe_install_macos_v6_runtime(
      "v5.27.1",
      platform_darwin,
      tempdir()
    )
  )
})

# More edge cases for macOS detection
test_that("get_macos_release_info handles edge cases", {
  # Invalid release format
  info <- get_macos_release_info(
    c(sysname = "Darwin", release = "invalid")
  )
  expect_true(is.na(info$darwin_major))

  # Empty release
  info <- get_macos_release_info(
    c(sysname = "Darwin", release = "")
  )
  expect_true(is.na(info$darwin_major))

  # NULL sys_info
  info <- get_macos_release_info(NULL)
  expect_true(is.na(info$release))
})

# Test edge cases for platform detection
test_that("get_platform_info handles NULL and empty inputs", {
  expect_error(
    get_platform_info(sys_info = NULL),
    "not supported"
  )

  expect_error(
    get_platform_info(
      sys_info = c(sysname = "Linux", machine = "")
    ),
    "not supported"
  )
})

# Test get_platform_info with empty arch override
test_that("get_platform_info errors on empty arch override", {
  orig_arch <- getOption("osrm.backend.override_arch")
  on.exit(options(osrm.backend.override_arch = orig_arch))

  options(osrm.backend.override_arch = "")
  expect_error(
    get_platform_info(),
    "must be a non-empty string"
  )
})

# Test osrm_detect_installations with empty root
test_that("osrm_detect_installations handles non-existent root", {
  fake_root <- "/path/that/definitely/does/not/exist/anywhere"
  installations <- osrm_detect_installations(fake_root)
  expect_length(installations, 0)
})

# Test osrm_detect_installations with files (not directories)
test_that("osrm_detect_installations ignores files", {
  tmp_root <- tempfile()
  dir.create(tmp_root)
  on.exit(unlink(tmp_root, recursive = TRUE))

  # Create a file, not a directory
  file.create(file.path(tmp_root, "not-a-dir"))

  installations <- osrm_detect_installations(tmp_root)
  expect_length(installations, 0)
})

# Test osrm_is_install_dir with Windows executable
test_that("osrm_is_install_dir detects Windows installation", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  file.create(file.path(tmp_dir, "osrm-routed.exe"))

  expect_true(osrm_is_install_dir(tmp_dir))
})

# Test PATH management when switching between versions ----
test_that("set_path_session removes old cached versions and prioritizes new version", {
  orig_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = orig_path))

  # Use the actual cache root to match production behavior
  cache_root <- osrm_default_install_root()

  # Create two version directories in the cache
  v6_dir <- file.path(cache_root, "test_v6.0.0")
  v5_dir <- file.path(cache_root, "test_v5.27.1")
  dir.create(v6_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(v5_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(v6_dir, recursive = TRUE)
    unlink(v5_dir, recursive = TRUE)
  }, add = TRUE)

  # Create a system installation directory (outside cache)
  system_dir <- tempfile()
  dir.create(system_dir)
  on.exit(unlink(system_dir, recursive = TRUE), add = TRUE)

  # Add system installation to PATH first
  Sys.setenv(PATH = paste(system_dir, orig_path, sep = .Platform$path.sep))

  # Step 1: Add v6.0.0 to PATH
  set_path_session(v6_dir, quiet = TRUE)
  path_with_v6 <- Sys.getenv("PATH")
  path_elements <- strsplit(path_with_v6, .Platform$path.sep)[[1]]
  normalized_paths <- normalizePath(path_elements, mustWork = FALSE)
  v6_normalized <- normalizePath(v6_dir, mustWork = FALSE)

  # v6.0.0 should be first in PATH
  expect_identical(normalized_paths[1], v6_normalized)
  # System installation should still be in PATH
  expect_true(normalizePath(system_dir, mustWork = FALSE) %in% normalized_paths)

  # Step 2: Add v5.27.1 to PATH (simulating a version switch)
  set_path_session(v5_dir, quiet = TRUE)
  path_with_v5 <- Sys.getenv("PATH")
  path_elements <- strsplit(path_with_v5, .Platform$path.sep)[[1]]
  normalized_paths <- normalizePath(path_elements, mustWork = FALSE)
  v5_normalized <- normalizePath(v5_dir, mustWork = FALSE)

  # v5.27.1 should now be first in PATH
  expect_identical(normalized_paths[1], v5_normalized)
  # v6.0.0 should have been REMOVED (it's a cached version)
  expect_false(v6_normalized %in% normalized_paths)
  # System installation should still be in PATH
  expect_true(normalizePath(system_dir, mustWork = FALSE) %in% normalized_paths)
})

test_that("set_path_session preserves system installations when adding cached version", {
  orig_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = orig_path))

  # Use the actual cache root
  cache_root <- osrm_default_install_root()

  # Create cached version directory
  v6_dir <- file.path(cache_root, "test_v6.0.0_preserve")
  dir.create(v6_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(v6_dir, recursive = TRUE), add = TRUE)

  # Create multiple system installation directories
  homebrew_dir <- "/usr/local/bin"  # Common homebrew location
  custom_dir <- tempfile()
  dir.create(custom_dir)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)

  # Set up PATH with system installations
  Sys.setenv(PATH = paste(
    c(homebrew_dir, custom_dir, orig_path),
    collapse = .Platform$path.sep
  ))

  # Add cached version to PATH
  set_path_session(v6_dir, quiet = TRUE)
  new_path <- Sys.getenv("PATH")
  path_elements <- strsplit(new_path, .Platform$path.sep)[[1]]
  normalized_paths <- normalizePath(path_elements, mustWork = FALSE)

  # Cached version should be first
  expect_identical(
    normalized_paths[1],
    normalizePath(v6_dir, mustWork = FALSE)
  )

  # System installations should still be present
  expect_true(homebrew_dir %in% path_elements)
  expect_true(normalizePath(custom_dir, mustWork = FALSE) %in% normalized_paths)
})

test_that("set_path_project replaces old cached version with new one", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  orig_wd <- getwd()
  on.exit({
    setwd(orig_wd)
    unlink(tmp_dir, recursive = TRUE)
  })

  setwd(tmp_dir)

  # Create mock cache directories
  tmp_cache <- tempfile()
  dir.create(tmp_cache, recursive = TRUE)
  on.exit(unlink(tmp_cache, recursive = TRUE), add = TRUE)

  v6_dir <- file.path(tmp_cache, "v6.0.0")
  v5_dir <- file.path(tmp_cache, "v5.27.1")
  dir.create(v6_dir)
  dir.create(v5_dir)

  # Add v6.0.0 to .Rprofile
  set_path_project(v6_dir, quiet = TRUE)

  rprofile <- file.path(tmp_dir, ".Rprofile")
  lines <- readLines(rprofile)
  expect_true(any(grepl(normalizePath(v6_dir, mustWork = FALSE), lines, fixed = TRUE)))

  # Now add v5.27.1 - it should replace v6.0.0
  set_path_project(v5_dir, quiet = TRUE)

  lines <- readLines(rprofile)
  # v5.27.1 should be in .Rprofile
  expect_true(any(grepl(normalizePath(v5_dir, mustWork = FALSE), lines, fixed = TRUE)))
  # v6.0.0 should NOT be in .Rprofile anymore
  expect_false(any(grepl(normalizePath(v6_dir, mustWork = FALSE), lines, fixed = TRUE)))
  # Should only have one osrm.backend line
  count <- sum(grepl("#added-by-r-pkg-osrm.backend", lines, fixed = TRUE))
  expect_identical(count, 1L)
})

# Integration tests with mocking would go here if we had a mocking framework
# For now, the actual osrm_install function is tested in the setup-osrm.R
