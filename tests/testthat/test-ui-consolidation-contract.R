ui_id_count <- function(html, id) {
  pattern <- paste0('id="', id, '"')
  matches <- gregexpr(pattern, html, fixed = TRUE)[[1L]]
  if (matches[[1L]] == -1L) 0L else length(matches)
}

ui_data_value_count <- function(html, value) {
  pattern <- paste0('data-value="', value, '"')
  matches <- gregexpr(pattern, html, fixed = TRUE)[[1L]]
  if (matches[[1L]] == -1L) 0L else length(matches)
}

server_output_count <- function(source, id) {
  pattern <- paste0("output\\$", id, "\\s*<-")
  matches <- gregexpr(pattern, source, perl = TRUE)[[1L]]
  if (matches[[1L]] == -1L) 0L else length(matches)
}

test_that("approved outputs have one canonical UI and server destination", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = "eta", manifest_names = "y", tipred_names = "group"
  )))
  html <- as.character(ctgui_app_ui(
    spec, ctgui_help_catalog(),
    list(
      visual_asset_url = function(file) file,
      application_asset_version = "test"
    )
  ))
  server_source <- paste(readLines(
    ctgui_test_source_path("R", "app_server.R"),
    warn = FALSE
  ), collapse = "\n")

  canonical <- c(
    validation = "validation_table_spec",
    model_code = "code_output",
    generated_code = "output_code",
    pars = "output_pars",
    fit_summary = "fit_summary",
    summary_matrices = "fit_summary_matrices",
    fit_messages = "fit_log_inline",
    fit_warnings = "fit_warnings_inline",
    data_preview = "data_preview"
  )
  obsolete <- c(
    "validation_table", "pars_table",
    "fit_summary_diagnostics", "fit_summary_matrices_diagnostics",
    "fit_log", "fit_warnings", "data_preview_import", "data_preview_generate"
  )

  for (id in unname(canonical)) {
    expect_equal(ui_id_count(html, id), 1L, info = paste("UI destination", id))
    expect_equal(server_output_count(server_source, id), 1L,
      info = paste("server destination", id))
  }
  for (id in obsolete) {
    expect_equal(ui_id_count(html, id), 0L, info = paste("obsolete UI", id))
    expect_equal(server_output_count(server_source, id), 0L,
      info = paste("obsolete server output", id))
  }

  expect_equal(ui_data_value_count(html, "Validation"), 0L)
  expect_equal(ui_data_value_count(html, "Code"), 0L)
  expect_equal(ui_data_value_count(html, "Pars"), 0L)
  expect_equal(ui_data_value_count(html, "Messages"), 0L)
  expect_equal(ui_data_value_count(html, "Warnings"), 0L)
  expect_equal(ui_data_value_count(html, "Summary matrices"), 0L)
  # A retained tab renders one data-value on its link and one on its pane.
  # Data > Summary remains; only Diagnostics > Summary is removed.
  expect_equal(ui_data_value_count(html, "Summary"), 2L)
})

test_that("merged data-role controls are creatable and retain manual names", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = "eta",
    manifest_names = c("m_manual", "m_future"),
    tdpred_names = "td_manual",
    tipred_names = "ti_manual",
    id = "person_manual", time = "occasion_manual"
  )))

  no_data <- ctgui_data_role_selection(NULL, spec)
  expect_equal(no_data$manifest_names, spec$manifest_names)
  expect_equal(no_data$tdpred_names, spec$tdpred_names)
  expect_equal(no_data$tipred_names, spec$tipred_names)
  expect_equal(no_data$id, spec$id)
  expect_equal(no_data$time, spec$time)

  data <- data.frame(
    person = 1:2, occasion = 0:1, observed = c(1, 2),
    stringsAsFactors = FALSE
  )
  active <- ctgui_data_role_selection(data, spec)
  expect_setequal(active$choices, c(
    names(data), spec$manifest_names, spec$tdpred_names, spec$tipred_names,
    spec$id, spec$time
  ))
  expect_equal(active$manifest_names, spec$manifest_names)
  expect_equal(active$tdpred_names, spec$tdpred_names)
  expect_equal(active$tipred_names, spec$tipred_names)
  expect_equal(active$id, spec$id)
  expect_equal(active$time, spec$time)

  html <- getFromNamespace("renderTags", "htmltools")(
    getFromNamespace("ctgui_data_roles_ui", "ctsemgui")(spec, data = NULL)
  )$html
  role_aliases <- list(
    manifest = c("manifest_names", "data_manifest_names"),
    tdpred = c("tdpred_names", "data_tdpred_names"),
    tipred = c("tipred_names", "data_tipred_names"),
    id = c("id", "data_id"),
    time = c("time", "data_time")
  )
  for (role in names(role_aliases)) {
    aliases <- role_aliases[[role]]
    present <- aliases[vapply(aliases, function(id) ui_id_count(html, id) > 0L,
      logical(1L))]
    expect_length(present, 1L)
    if (length(present) == 1L) {
      create_pattern <- paste0(
        'data-for="', present, '"[^>]*>\\s*\\{[^<]*"create"\\s*:\\s*true'
      )
      expect_match(html, create_pattern, perl = TRUE,
        info = paste(role, "accepts manually created values"))
    }
  }
  expect_false(grepl(">Data mapping<", html, fixed = TRUE))
})

test_that("approved consolidation retains substantive workflows", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = "eta", manifest_names = "y", tipred_names = "group"
  )))
  html <- as.character(ctgui_app_ui(
    spec, ctgui_help_catalog(),
    list(
      visual_asset_url = function(file) file,
      application_asset_version = "test"
    )
  ))
  source <- paste(
    readLines(ctgui_test_source_path("R", "app_ui.R"), warn = FALSE),
    readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )

  static_ids <- c(
    "tipred_network", "fit_save_name", "save_fit", "active_fit_name",
    "fit_comparison", "raw_plot", "raw_plot_png", "raw_plot_pdf",
    "assign_model", "assign_fit", "download_model_rds", "download_project_rds",
    "download_fit_rds", "load_model_rds", "load_fit_rds",
    "fit_uncertainty_method", "run_uncertainty",
    "generate_from_fit", "run_cov_check", "run_kalman", "run_postpred",
    "run_residual_acf", "run_dynamics", "run_tipred_effects"
  )
  for (id in static_ids) {
    expect_equal(ui_id_count(html, id), 1L, info = paste("retained UI", id))
  }

  dynamic_ids <- c(
    "tipred_network_plot", "tipred_network_status",
    "uncertainty_status", "uncertainty_summary",
    "generated_fit_summary", "cov_check_plots", "kalman_plot",
    "postpred_plots", "residual_acf_plot", "dynamics_plot",
    "tipred_effects_plots"
  )
  for (id in dynamic_ids) {
    expect_match(source, paste0('"', id, '"'), fixed = TRUE,
      info = paste("retained workflow", id))
  }
})
