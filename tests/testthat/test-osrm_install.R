test_that("get_platform_info detects Windows x86-64 machines", {
  fake_sys_info <- c(
    sysname = "Windows",
    machine = "x86-64"
  )

  platform <- osrm.backend:::get_platform_info(sys_info = fake_sys_info)

  expect_identical(platform, list(os = "win32", arch = "x64"))
})

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

  selected <- osrm.backend:::find_asset_url(release_info, platform)
  expect_identical(selected, "https://example.com/node-v108")
})
