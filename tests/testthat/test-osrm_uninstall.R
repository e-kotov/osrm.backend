# Tests for osrm_uninstall.R

test_that("osrm_uninstall removes installation when dest_dir is provided", {
  # Create temporary directory structure
  tmp_root <- tempfile()
  dir.create(tmp_root, recursive = TRUE)
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)

  dest_dir <- file.path(tmp_root, "osrm-install")
  dir.create(dest_dir)
  file.create(file.path(dest_dir, "osrm-routed"))

  # Mock osrm_clear_path to avoid .Rprofile manipulation
  # Note: interactive() set via option to avoid base function mocking
  orig_inter <- getOption("testthat.force_interactive")
  options(testthat.force_interactive = FALSE)
  on.exit(options(testthat.force_interactive = orig_inter), add = TRUE)

  # Set non-interactive mode
  options(rlang_interactive = FALSE)

  result <- osrm_uninstall(
    dest_dir = dest_dir,
    clear_path = FALSE,
    quiet = TRUE
  )

  expect_true(result)
  expect_false(dir.exists(dest_dir))
})

test_that("osrm_uninstall auto-detects single installation", {
  tmp_root <- tempfile()
  dir.create(tmp_root, recursive = TRUE)
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)

  install_dir <- file.path(tmp_root, "v5.27.1")
  dir.create(install_dir)
  file.create(file.path(install_dir, "osrm-routed"))

  with_mocked_bindings(
    {
      options(rlang_interactive = FALSE)

      result <- osrm_uninstall(
        dest_dir = NULL,
        clear_path = FALSE,
        quiet = TRUE
      )

      expect_true(result)
      expect_false(dir.exists(install_dir))
    },
    osrm_default_install_root = function() tmp_root,
    osrm_clear_path = function(...) TRUE
  )
})

test_that("osrm_uninstall returns FALSE when no installation found", {
  tmp_root <- tempfile()
  dir.create(tmp_root, recursive = TRUE)
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)

  with_mocked_bindings(
    {
      result <- osrm_uninstall(
        dest_dir = NULL,
        clear_path = FALSE,
        quiet = TRUE
      )

      expect_false(result)
    },
    osrm_default_install_root = function() tmp_root,
    osrm_clear_path = function(...) TRUE
  )
})

test_that("osrm_uninstall returns FALSE when directory doesn't exist", {
  fake_dir <- "/this/path/does/not/exist"

  with_mocked_bindings(
    {
      result <- osrm_uninstall(
        dest_dir = fake_dir,
        clear_path = FALSE,
        quiet = TRUE
      )

      expect_false(result)
    },
    osrm_clear_path = function(...) TRUE
  )
})

test_that("osrm_uninstall calls osrm_clear_path when clear_path=TRUE", {
  tmp_root <- tempfile()
  dir.create(tmp_root, recursive = TRUE)
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)

  clear_path_called <- FALSE

  with_mocked_bindings(
    {
      osrm_uninstall(
        dest_dir = NULL,
        clear_path = TRUE,
        quiet = TRUE
      )

      expect_true(clear_path_called)
    },
    osrm_default_install_root = function() tmp_root,
    osrm_clear_path = function(...) {
      clear_path_called <<- TRUE
      TRUE
    }
  )
})

test_that("osrm_uninstall doesn't call osrm_clear_path when clear_path=FALSE", {
  tmp_root <- tempfile()
  dir.create(tmp_root, recursive = TRUE)
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)

  install_dir <- file.path(tmp_root, "v5.27.1")
  dir.create(install_dir)
  file.create(file.path(install_dir, "osrm-routed"))

  clear_path_called <- FALSE

  with_mocked_bindings(
    {
      options(rlang_interactive = FALSE)

      osrm_uninstall(
        dest_dir = install_dir,
        clear_path = FALSE,
        quiet = TRUE
      )

      expect_false(clear_path_called)
    },
    osrm_clear_path = function(...) {
      clear_path_called <<- TRUE
      TRUE
    }
  )
})

test_that("osrm_uninstall handles multiple installations in non-interactive mode", {
  tmp_root <- tempfile()
  dir.create(tmp_root, recursive = TRUE)
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)

  # Create two version directories
  v1_dir <- file.path(tmp_root, "v5.27.1")
  v2_dir <- file.path(tmp_root, "v6.0.0")
  dir.create(v1_dir)
  dir.create(v2_dir)
  file.create(file.path(v1_dir, "osrm-routed"))
  file.create(file.path(v2_dir, "osrm-routed"))

  with_mocked_bindings(
    {
      options(rlang_interactive = FALSE)

      # Should return FALSE because it can't decide which to uninstall
      result <- osrm_uninstall(
        dest_dir = NULL,
        clear_path = FALSE,
        quiet = TRUE
      )

      expect_false(result)
      expect_true(dir.exists(v1_dir))
      expect_true(dir.exists(v2_dir))
    },
    osrm_default_install_root = function() tmp_root,
    osrm_clear_path = function(...) TRUE
  )
})

test_that("osrm_prompt_install_selection returns NULL for empty paths", {
  result <- osrm_prompt_install_selection(character(0))
  expect_null(result)
})
