test_that("the launcher is the only exported package API", {
  expect_equal(getNamespaceExports("ctsemGUI"), "ctgui_launch_app")
})

test_that("runtime dependencies satisfy the supported ctsem contract", {
  expect_gte(utils::packageVersion("ctsem"), numeric_version("3.11.1"))
  expect_true(requireNamespace("shiny", quietly = TRUE))
})

test_that("R sources stay ASCII so the package remains portable", {
  r_dir <- ctgui_test_source_path("R")
  sources <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  expect_gt(length(sources), 0L)

  offenders <- Filter(function(path) {
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    any(grepl("[^\x01-\x7f]", lines, useBytes = FALSE))
  }, sources)

  expect_equal(basename(offenders), character())
})
