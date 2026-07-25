test_that("application composition constructs a Shiny application", {
  skip_if_not_installed("shiny")

  app <- ctgui_create_app()
  expect_s3_class(app, "shiny.appobj")
  expect_s3_class(
    ctgui_app_ui(
      ctgui_spec(), ctgui_help_catalog(),
      list(visual_asset_url = function(file) file)
    ),
    "shiny.tag.list"
  )
})

test_that("server construction has a testServer-compatible seam", {
  skip_if_not_installed("shiny")

  expect_no_error(suppressWarnings(shiny::testServer(
    ctgui_app_server(ctgui_spec(), ctgui_help_catalog()),
    { expect_true(is.function(session$sendCustomMessage)) }
  )))
})
