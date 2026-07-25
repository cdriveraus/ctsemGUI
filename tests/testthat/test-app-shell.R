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

test_that("server delegates workflow code and fit-shape handling to services", {
  server_file <- ctgui_test_source_path("R", "app_server.R")
  server_text <- paste(readLines(server_file, warn = FALSE), collapse = "\n")
  expect_match(server_text, "ctgui_output_workflow_code", fixed = TRUE)
  expect_match(server_text, "ctgui_output_snippet", fixed = TRUE)
  expect_match(server_text, "ctgui_ctsem_fit_model", fixed = TRUE)
  expect_match(server_text, "ctgui_ctsem_fit_generated", fixed = TRUE)
  expect_false(grepl("ctsemgui::ctgui_generate_data", server_text, fixed = TRUE))
  expect_false(grepl("fit\\$(model|modelbase|ctstanmodel|ctstanmodelbase|generated|stanfit)",
    server_text, perl = TRUE))

  suppressWarnings(shiny::testServer(
    ctgui_app_server(ctgui_spec(), ctgui_help_catalog()), {
      code <- output$output_code
      expect_error(parse(text = code), NA)
      expect_match(code, "# No data is currently active.", fixed = TRUE)
    }
  ))
})
