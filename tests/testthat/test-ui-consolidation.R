count_fixed <- function(text, pattern) {
  (length(strsplit(text, pattern, fixed = TRUE)[[1L]]) - 1L)
}

test_that("data-role add dialogs retain manual names", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = "eta", manifest_names = "typed_y",
    tdpred_names = "typed_event", tipred_names = "typed_group",
    id = "typed_id", time = "typed_time"
  )))
  roles_ui <- getFromNamespace("ctgui_data_roles_ui", "ctsemGUI")(
    spec, data.frame(observed = 1)
  )
  render_tags <- getFromNamespace("renderTags", "htmltools")
  html <- render_tags(roles_ui)$html

  for (id in c(
    "manifest_names", "tdpred_names", "tipred_names", "id", "time"
  )) {
    expect_equal(count_fixed(html, paste0('id="', id, '"')), 1L)
  }
  expect_equal(count_fixed(html, '"create":true'), 4L)
  expect_match(html, 'id="spec_add_manifest"', fixed = TRUE)
  expect_false(grepl('id="spec_add_tdpred"', html, fixed = TRUE))
  expect_false(grepl('id="spec_add_tipred"', html, fixed = TRUE))
  expect_match(html, 'value="observed"', fixed = TRUE)
  expect_match(html, 'value="typed_y"', fixed = TRUE)
  expect_match(html, 'readonly="readonly"', fixed = TRUE)
  expect_match(html, 'value="typed_id" selected', fixed = TRUE)

  no_data <- render_tags(
    getFromNamespace("ctgui_data_roles_ui", "ctsemGUI")(spec)
  )$html
  expect_match(no_data, 'value="typed_y"', fixed = TRUE)
  expect_match(no_data, 'value="typed_time" selected', fixed = TRUE)
})

test_that("application exposes each consolidated output once", {
  ui <- ctgui_app_ui(
    ctgui_spec(),
    ctgui_help_catalog(),
    list(
      visual_asset_url = function(file) file,
      application_asset_version = "ui-contract"
    )
  )
  render_tags <- getFromNamespace("renderTags", "htmltools")
  rendered <- render_tags(ui)
  html <- paste(rendered$head, rendered$html, collapse = "\n")

  for (id in c(
    "data_preview", "validation_table_spec", "fit_log_inline",
    "fit_warnings_inline", "code_output", "fit_summary",
    "fit_summary_matrices", "fit_comparison", "output_code"
  )) {
    expect_equal(count_fixed(html, paste0('id="', id, '"')), 1L)
  }
  for (removed in c(
    "data_preview_import", "data_preview_generate", "validation_table",
    "pars_table", "output_pars", "fit_log", "fit_warnings", "fit_summary_diagnostics",
    "fit_summary_matrices_diagnostics", "data_manifest_names",
    "data_tdpred_names", "data_tipred_names", "data_id", "data_time"
  )) {
    expect_equal(count_fixed(html, paste0('id="', removed, '"')), 0L)
  }
})

test_that("data import provides requirements and ctsem test data", {
  ui <- ctgui_app_ui(
    ctgui_spec(), ctgui_help_catalog(),
    list(visual_asset_url = function(file) file, application_asset_version = "ui-contract")
  )
  html <- paste(as.character(ui), collapse = "\n")

  expect_match(html, 'id="load_ctsem_test_data"', fixed = TRUE)

  # The panel must explain the required data layout and both predictor kinds.
  # Assert that against the explanation catalog rather than a copy of its
  # wording, so editing the prose does not fail a test about the panel.
  catalog <- getFromNamespace("ctgui_explanation_catalog", "ctsemGUI")()
  expect_match(html, catalog$spec_data$brief, fixed = TRUE)
  expect_match(html, substr(catalog$spec_data$detail, 1L, 60L), fixed = TRUE)
  expect_match(catalog$spec_data$detail, "row of the data is one subject")
  expect_match(catalog$spec_data$detail, "Time-dependent predictors")
  expect_match(catalog$spec_data$detail, "Time-independent predictors")
})
