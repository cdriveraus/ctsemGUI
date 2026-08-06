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

test_that("close GUI control stops the Shiny app", {
  server_source <- paste(readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE), collapse = "\n")
  expect_match(server_source, 'observeEvent(input$close_gui', fixed = TRUE)
  expect_match(server_source, 'shiny::stopApp()', fixed = TRUE)
})

test_that("generation cores follow fit cores until manually adjusted", {
  ui_source <- paste(readLines(ctgui_test_source_path("R", "app_ui.R"), warn = FALSE), collapse = "\n")
  server_source <- paste(readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE), collapse = "\n")

  expect_false(grepl('fit_gen_follow_cores', ui_source, fixed = TRUE))
  expect_match(server_source, 'fit_gen_cores_follow_fit <- shiny::reactiveVal(TRUE)', fixed = TRUE)
  expect_match(server_source, 'fit_gen_cores_follow_fit(FALSE)', fixed = TRUE)
})

test_that("fit settings warn when the specification contains an expression", {
  skip_if_not_installed("shiny")
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta", "eta", label = "d1 + m1 * eta")

  suppressWarnings(shiny::testServer(ctgui_app_server(spec, ctgui_help_catalog()), {
    warning <- paste(as.character(output$fit_expression_warning), collapse = "\n")
    expect_match(warning, "Model compilation is likely required", fixed = TRUE)
    expect_match(warning, "can take a few minutes", fixed = TRUE)
  }))
})

test_that("page transitions commit the authored specification payload", {
  skip_if_not_installed("shiny")

  initial <- ctgui_spec(
    latent_names = "eta", manifest_names = "y", tipredDefault = TRUE
  )
  suppressWarnings(shiny::testServer(
    ctgui_app_server(initial, ctgui_help_catalog()), {
      # Reproduce the browser race: Shiny's ordinary binding still contains
      # the old value when Bootstrap begins the page transition.
      session$setInputs(
        latent_names = "eta", manifest_names = "y",
        tdpred_names = character(), tipred_names = character(),
        id = "id", time = "time", type = "ct", tipredDefault = TRUE
      )
      session$setInputs(tab_commit_nonce = list(
        nonce = 1,
        specification_authored = TRUE,
        specification = list(
          latent_names = "persisted_eta",
          manifest_names = "y",
          tdpred_names = character(),
          tipred_names = character(),
          id = "id", time = "time", type = "ct",
          tipredDefault = TRUE,
          manifest_type_1 = "0"
        )
      ))

      expect_match(
        output$code_output,
        'latentNames = "persisted_eta"',
        fixed = TRUE
      )
    }
  ))
})

test_that("unwritten Specification controls cannot replace a visual model edit", {
  skip_if_not_installed("shiny")
  initial <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  suppressWarnings(shiny::testServer(
    ctgui_app_server(initial, ctgui_help_catalog()), {
      session$setInputs(tab_commit_nonce = list(
        nonce = 1, specification_authored = FALSE,
        specification = list(latent_names = character(), manifest_names = character())
      ))
      expect_match(output$code_output, 'manifestNames = "y"', fixed = TRUE)
      expect_match(output$code_output, 'latentNames = "eta"', fixed = TRUE)
    }
  ))
})

test_that("server delegates workflow code and fit-shape handling to services", {
  server_file <- ctgui_test_source_path("R", "app_server.R")
  server_text <- paste(readLines(server_file, warn = FALSE), collapse = "\n")
  expect_match(server_text, "ctgui_output_workflow_code", fixed = TRUE)
  expect_match(server_text, "ctgui_output_snippet", fixed = TRUE)
  expect_match(server_text, "ctgui_ctsem_fit_model", fixed = TRUE)
  expect_match(server_text, "ctgui_ctsem_fit_generated", fixed = TRUE)
  expect_false(grepl("ctsemGUI::ctgui_generate_data", server_text, fixed = TRUE))
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
