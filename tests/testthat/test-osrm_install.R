test_that("get_platform_info detects Windows x86-64 machines", {
  fake_sys_info <- c(
    sysname = "Windows",
    machine = "x86-64"
  )

  platform <- osrm.backend:::get_platform_info(sys_info = fake_sys_info)

  expect_identical(platform, list(os = "win32", arch = "x64"))
})
