test_that("the launcher is the only exported package API", {
  expect_equal(getNamespaceExports("ctsemgui"), "ctgui_launch_app")
})

test_that("runtime dependencies satisfy the supported ctsem contract", {
  expect_gte(utils::packageVersion("ctsem"), numeric_version("3.11.1"))
  expect_true(requireNamespace("shiny", quietly = TRUE))
})
