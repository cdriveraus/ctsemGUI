ctgui_generate_worker <- getFromNamespace("ctgui_generate_worker", "ctsemGUI")
ctgui_background_start <- getFromNamespace("ctgui_background_start", "ctsemGUI")

test_that("the generation worker needs only ctsem, like the fit worker", {
  # Both children are started with package = FALSE, so anything the worker
  # reaches for must be named explicitly. A reference to a ctsemGUI helper
  # would fail only at run time, in a separate process.
  body_text <- paste(deparse(body(ctgui_generate_worker)), collapse = "\n")
  expect_match(body_text, "ctsem::ctGenerateFromFit", fixed = TRUE)
  expect_false(grepl("ctgui_", body_text, fixed = TRUE))
})

test_that("fitting and generating share one background mechanism", {
  # Generating from a fit is as slow as a fit and equally worth watching, so
  # it uses the same process, log and renderer rather than a parallel one.
  source <- paste(
    readLines(ctgui_test_source_path("R", "fit_process.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(source, "ctgui_background_start", fixed = TRUE)
  expect_match(source, "ctgui_generate_process_start", fixed = TRUE)
  expect_match(source, "ctgui_fit_process_start", fixed = TRUE)
})

test_that("generation is offered automatically and can be stopped", {
  skip_if_not_installed("shiny")

  ui <- ctgui_app_ui(
    ctgui_spec(), ctgui_help_catalog(),
    list(visual_asset_url = function(file) file, application_asset_version = "generate-contract")
  )
  html <- paste(as.character(ui), collapse = "\n")

  # Almost every diagnostic needs generated data; discovering that one panel
  # at a time is what this removes.
  expect_match(html, 'id="fit_generate_after"', fixed = TRUE)
  expect_match(html, 'id="cancel_generate"', fixed = TRUE)
  expect_match(html, 'id="generate_log"', fixed = TRUE)
})

test_that("the fit panel offers an engine and the fit carries the choice", {
  server_source <- paste(
    readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(server_source, "backend = fit_backend()", fixed = TRUE)
  # The availability check starts Julia and costs seconds, so it runs in a
  # background process as the session loads rather than blocking anything.
  expect_match(server_source, "ctgui_julia_check_worker", fixed = TRUE)
  expect_match(server_source, "output$fit_backend_status", fixed = TRUE)
})

test_that("a first Julia fit warns that it may precompile", {
  # Precompiling prints almost nothing and can take minutes. Without a word
  # here the fit looks stalled at "Activating project".
  server_source <- paste(
    readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(server_source, "precompiles the engine", fixed = TRUE)
})
