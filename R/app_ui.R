# Application UI composition -------------------------------------------------

ctgui_data_roles_ui <- function(spec, data = NULL) {
  roles <- ctgui_data_role_selection(data, spec)
  creatable <- list(create = TRUE, persist = FALSE, openOnFocus = TRUE)
  shiny::div(
    class = "control-grid",
    shiny::tags$select(
      id = "ctgui-spec-data-choices", style = "display:none",
      lapply(roles$choices, function(name) shiny::tags$option(value = name, name))
    ),
    shiny::div(
      shiny::tags$div(
        class = "form-group shiny-input-container",
        shiny::tags$label(`for` = "manifest_names", class = "control-label", "Manifest variables"),
        shiny::tags$input(
          id = "manifest_names", type = "text", class = "form-control ctgui-readonly-variable-list",
          value = paste(roles$manifest_names, collapse = ", "), readonly = "readonly",
          `aria-readonly` = "true", title = "Use Add manifest to choose the measured latent process."
        )
      ),
      shiny::tags$p(class = "help-note", "Add manifest and choose the latent process it measures."),
      shiny::actionButton("spec_add_manifest", "Add manifest", class = "ctgui-spec-add", `data-add-role` = "manifest")
    ),
    shiny::div(
      shiny::selectizeInput(
        "tdpred_names", "Time dependent predictors",
        choices = roles$tdpred_choices, selected = roles$tdpred_names,
        multiple = TRUE, options = creatable
      ),
      shiny::tags$p(class = "help-note", "Choose a dataset variable or type new names, separated by commas.")
    ),
    shiny::div(
      shiny::selectizeInput(
        "tipred_names", "Time independent predictors",
        choices = roles$tipred_choices, selected = roles$tipred_names,
        multiple = TRUE, options = creatable
      ),
      shiny::tags$p(class = "help-note", "Choose a dataset variable or type new names, separated by commas.")
    ),
    shiny::selectizeInput(
      "id", "ID column", choices = roles$id_choices, selected = roles$id,
      options = creatable
    ),
    shiny::selectizeInput(
      "time", "Time column", choices = roles$time_choices, selected = roles$time,
      options = creatable
    )
  )
}

ctgui_available_cores <- function() {
  detected <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
  if (is.na(detected) || detected < 1L) 1L else detected
}

ctgui_core_label <- function(argument, available = ctgui_available_cores()) {
  paste0(argument, " (of ", available, " CPU cores)")
}

ctgui_app_ui <- function(initial_spec, help_catalog, assets) {
  spec <- initial_spec
  available_cores <- ctgui_available_cores()
  default_cores <- min(2L, available_cores)
  visual_asset_url <- assets$visual_asset_url
  application_asset_version <- assets$application_asset_version
  if (is.null(application_asset_version) || !nzchar(application_asset_version)) {
    application_asset_version <- assets$visual_asset_version
  }
  if (is.null(application_asset_version) || !nzchar(application_asset_version)) {
    application_asset_version <- as.character(utils::packageVersion("ctsemgui"))
  }
  application_asset_url <- function(file) {
    paste0(
      "ctsemgui-assets/app/", file, "?v=",
      utils::URLencode(application_asset_version, reserved = TRUE)
    )
  }
help_link <- function(help_id) {
  ctgui_help_link(help_catalog, help_id)
}

arg_label <- function(label, help_id, title = NULL) {
  ctgui_arg_label(help_catalog, label, help_id, title)
}

ui <- shiny::fluidPage(
  id = "ctgui-app",
  shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = application_asset_url("app.css")),
    shiny::tags$script(src = application_asset_url("app.js")),
    shiny::tags$script(src = "ctsemgui-assets/visual-spec/cytoscape.min.js"),
    shiny::tags$script(src = visual_asset_url("visual-spec.js")),
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = visual_asset_url("visual-spec.css"))
  ),
  shiny::div(
    class = "app-header",
    shiny::titlePanel("ctsemgui")
  ),
  shiny::tabsetPanel(
    id = "workflow",
    selected = "Data",
    shiny::tabPanel(
      "Model",
      shiny::tabsetPanel(
        id = "model_tabs",
        shiny::tabPanel(
          "Specification",
          shiny::div(
            class = "control-band",
            shiny::tags$h4("Data roles"),
            shiny::uiOutput("explain_spec_data"),
            shiny::uiOutput("data_spec_controls")
          ),
          shiny::div(
            class = "control-band",
            shiny::tags$h4("Model variables"),
            shiny::div(
              class = "control-grid",
              shiny::textInput("latent_names", "Latent processes", paste(spec$latent_names, collapse = ", "))
            ),
            shiny::uiOutput("manifest_type_controls")
          ),
          shiny::div(
            class = "control-band",
            shiny::tags$h4("Options"),
            shiny::div(
              class = "control-grid",
              shiny::selectInput("type", arg_label("Time model", "help_gui_time_model", "Continuous or discrete time model"),
                choices = c("Continuous time (ct)" = "ct", "Discrete time (dt)" = "dt"), selected = spec$type),
              shiny::checkboxInput("tipredDefault", "Time independent predictors moderate all parameters by default (uncheck for no moderation unless specified)", value = isTRUE(spec$tipredDefault))
            )
          ),
          shiny::tableOutput("validation_table_spec")
        ),
        shiny::tabPanel(
          "Matrices",
          shiny::tabsetPanel(
            id = "matrix_group",
            type = "pills",
            shiny::tabPanel("Dynamics", shiny::uiOutput("matrix_dynamics_editor")),
            shiny::tabPanel("Measurement", shiny::uiOutput("matrix_measurement_editor")),
            shiny::tabPanel("Initial", shiny::uiOutput("matrix_initial_editor")),
            shiny::tabPanel("Predictors", shiny::uiOutput("matrix_predictor_editor")),
            shiny::tabPanel("PARS", shiny::uiOutput("matrix_pars_editor"))
          )
        ),
        shiny::tabPanel(
          "Visual Specification",
          shiny::div(
            class = "control-band",
            shiny::tags$h4("Drawable state-space specification"),
            shiny::tags$p(class = "help-note", "Draw and arrange the fitted-model structure here. Each edit updates the model specification immediately. Predictor-distribution matrices used only for data generation remain under Matrices."),
            shiny::div(class = "control-grid",
              shiny::selectInput("visual_view", "View", choices = c(
                "State space" = "state_space", "Initial state" = "initial_state",
                "Individual Differences" = "tipred_effects"
              ))
            ),
            shiny::textOutput("visual_status"),
            shiny::tags$p(class = "matrix-note", "To define paths, right click and drag from node to node or set Mode to 'draw paths' then left click and drag."),
            shiny::conditionalPanel(
              condition = "input.visual_view == 'initial_state'",
              shiny::tags$p(
                class = "matrix-note",
                "Initial-state noise maps directly to T0VAR. When a T0MEANS parameter has RandomEffects enabled, ctsem ignores the corresponding T0VAR row and column during fitting: its diagonal is fixed to 1e-6 and its off-diagonals to 0. This view shows the ignored diagonal as a dotted 1e-6 loop and omits its zero correlations."
              )
            )
          ),
          shiny::tags$div(id = "visual_spec_canvas", class = "ctgui-visual-spec"),
          shiny::uiOutput("visual_path_inspector"),
          shiny::uiOutput("visual_pars_details")
        ),
        shiny::tabPanel(
          "Equations",
          shiny::div(
            class = "control-band",
            shiny::div(
              class = "control-grid",
              shiny::sliderInput("equation_zoom", "Zoom", min = 0.5, max = 2.5, value = 1, step = 0.1),
              shiny::checkboxInput("equation_split_dynamics", "Split dynamics", value = FALSE),
              shiny::checkboxInput("equation_split_measurement", "Split measurement", value = FALSE),
              shiny::numericInput("equation_digits", "Digits", value = 2, min = 0, max = 8, step = 1)
            )
          ),
          shiny::div(class = "equation-pane", shiny::imageOutput("equation_image", inline = TRUE)),
          shiny::textOutput("equation_status"),
          shiny::tags$details(
            shiny::tags$summary("LaTeX source"),
            shiny::verbatimTextOutput("equation_source")
          )
        )
      )
    ),
    shiny::tabPanel(
      "Data",
      shiny::textOutput("data_status"),
      shiny::tabsetPanel(
        id = "data_tabs",
        shiny::tabPanel(
          "Import",
          shiny::div(
            class = "control-band",
            shiny::div(
              class = "control-grid",
              shiny::selectInput("env_data", "R data.frame or matrix", choices = character()),
              shiny::fileInput("csv_file", "Browse", accept = c(
                ".csv", "text/csv", ".rds", "application/octet-stream"
              ))
            )
          )
        ),
        shiny::tabPanel(
          "Generate",
          shiny::div(
            class = "control-band",
            shiny::tags$p(class = "help-note", "This is the data-generation workflow. TDPREDMEANS and TDPREDVAR are used here to describe generated time-dependent predictors; they are not fitted model parameters. Fitted TD predictor effects belong in TDPREDEFFECT under Model > Matrices > Predictors."),
            shiny::div(
              class = "control-grid",
              shiny::numericInput("gen_subjects", "Generated subjects", value = 20, min = 1, step = 1),
              shiny::numericInput("gen_tpoints", "Generated time points", value = spec$Tpoints %||% 10, min = 1, step = 1),
              shiny::numericInput("gen_dtmean", "Generated mean dt", value = 1, min = 0.0001, step = 0.1),
              shiny::numericInput("gen_logdtsd", arg_label("Generated logdtsd", "help_gui_logdtsd", "sd of log timeintervals"), value = 0, min = 0, step = 0.05),
              shiny::numericInput("gen_burnin", "Generated burn-in", value = 0, min = 0, step = 1),
              shiny::checkboxInput("gen_free_defaults", arg_label("Preview by replacing free labels with simple numeric values", "help_gui_generation_defaults", "Generation preview with substituted numeric values"), value = TRUE),
              shiny::actionButton("generate_data", "Generate data", class = "btn-primary")
            )
          )
        ),
        shiny::tabPanel("Preview", shiny::div(class = "data-preview", shiny::tableOutput("data_preview"))),
        shiny::tabPanel(
          "Summary",
          shiny::tags$h4("Numeric summary"),
          shiny::tableOutput("data_summary"),
          shiny::tags$h4("Missingness"),
          shiny::tableOutput("missingness_summary"),
          shiny::tags$h4("Within/between numeric variation"),
          shiny::tableOutput("within_between_summary")
        ),
        shiny::tabPanel(
          "Visuals",
          shiny::div(
            class = "control-band",
          shiny::uiOutput("explain_raw_visuals"),
            shiny::uiOutput("raw_plot_controls")
          ),
          shiny::plotOutput("raw_plot", height = 420),
          ctgui_plot_export_controls("raw_plot", 420)
        )
      )
    ),
    shiny::tabPanel(
      "Fit",
      shiny::tabsetPanel(
        id = "fit_tabs",
        shiny::tabPanel(
          "Settings",
          shiny::div(
            class = "control-band",
            shiny::div(
              class = "control-grid",
              shiny::checkboxInput("fit_optimize", arg_label("optimize", "help_fit_optimize", "Whether to optimize or use full Bayesian sampling via Stan (generally very slow!)"), value = TRUE),
              shiny::checkboxInput("fit_priors", arg_label("priors", "help_fit_priors", "ctFit argument: priors"), value = FALSE),
              shiny::numericInput("fit_cores", arg_label(ctgui_core_label("cores", available_cores), "help_fit_cores", "ctFit argument: cores"), value = default_cores, min = 1, max = available_cores, step = 1),
              shiny::textAreaInput("fit_extra_args", arg_label("Extra ctFit arguments", "help_ctFit", "Full ctFit help"), value = "", height = "70px"),
              shiny::checkboxInput("fit_completion_beep", "Play a sound when fitting finishes", value = FALSE),
              shiny::textInput("fit_save_name", "Fit name", value = "fit1"),
              shiny::actionButton("run_fit", "Fit model", class = "btn-primary"),
              shiny::actionButton("save_fit", "Save current fit")
            )
          ),
          shiny::uiOutput("explain_fit_registry"),
          shiny::textOutput("fit_status"),
          shiny::selectInput("active_fit_name", "Active saved fit", choices = character()),
          shiny::div(
            class = "fit-inline-output",
            shiny::tags$h4("Messages"),
            shiny::verbatimTextOutput("fit_log_inline"),
            shiny::tags$h4("Warnings"),
            shiny::verbatimTextOutput("fit_warnings_inline")
          ),
          shiny::div(
            class = "control-band",
            shiny::tags$h4("R session and RDS files"),
            shiny::tags$p(class = "help-note", "Return the raw ctsem model or fit to the R session, or save/load it as an .rds file. The model is created with ctsem::ctModel()."),
            shiny::div(class = "control-grid",
              shiny::textInput("model_object_name", "Model object name in R", value = "model"),
              shiny::actionButton("assign_model", "Return model to R"),
              shiny::downloadButton("download_model_rds", "Save model RDS"),
              shiny::downloadButton("download_project_rds", "Save ctsemgui project RDS"),
              shiny::fileInput("load_model_rds", "Load model RDS", accept = ".rds"),
              shiny::textInput("fit_object_name", "Fit object name in R", value = "fit"),
              shiny::actionButton("assign_fit", "Return fit to R"),
              shiny::downloadButton("download_fit_rds", "Save fit RDS"),
              shiny::fileInput("load_fit_rds", "Load fit RDS", accept = ".rds")
            )
          )
        ),
        shiny::tabPanel(
          "Uncertainty",
          shiny::div(
            class = "control-band",
            shiny::tags$h4("Optimized-fit uncertainty"),
            shiny::tags$p(class = "help-note", "These settings are used by Fit model when Optimize is selected. They can also be applied to an existing optimized fit below. Importance sampling and full bootstrap can take substantially longer."),
            shiny::div(
              class = "control-grid",
              shiny::selectInput("fit_uncertainty_method", arg_label("Method", "help_uncertainty_method", "ctOptimUncertainty argument: uncertainty"), choices = ctgui_uncertainty_method_choices(), selected = "hessian"),
              shiny::selectInput("fit_uncertainty_draws", arg_label("Approximate draws", "help_uncertainty_draws", "ctOptimUncertainty argument: draws"), choices = ctgui_uncertainty_draw_choices("hessian"), selected = "auto"),
              shiny::numericInput("fit_uncertainty_samples", arg_label("Draws / refits", "help_uncertainty_samples", "ctOptimUncertainty argument: finishsamples"), value = 1000, min = 2, step = 100),
              shiny::actionButton("run_uncertainty", "Recompute uncertainty", class = "btn-primary")
            ),
            shiny::uiOutput("uncertainty_eligibility"),
            shiny::conditionalPanel(
              "input.fit_uncertainty_method === 'fullbootstrap'",
              shiny::tags$p(class = "warning-note", "Full bootstrap refits the model once per requested sample. Recomputing will ask for confirmation.")
            ),
            shiny::tags$details(
              shiny::tags$summary(arg_label("Advanced method controls", "help_uncertainty_control", "ctOptimUncertainty argument: control")),
              shiny::div(
                class = "control-grid",
                shiny::numericInput("fit_uncertainty_ridge", "Ridge", value = 1e-8, min = 0, step = 1e-8),
                shiny::numericInput("fit_uncertainty_hessian_step", "Hessian step", value = 1e-3, min = 1e-8, step = 1e-3),
                shiny::conditionalPanel(
                  "input.fit_uncertainty_method === 'surrogate'",
                  shiny::textInput("fit_uncertainty_surrogate_npoints", "Surrogate points (blank = automatic)", value = ""),
                  shiny::numericInput("fit_uncertainty_surrogate_scale", "Surrogate scale", value = .5, min = .01, step = .1),
                  shiny::checkboxInput("fit_uncertainty_surrogate_profile", "Profile surrogate curvature", value = TRUE),
                  shiny::textInput("fit_uncertainty_surrogate_target_drop", "Profile target drop (blank = automatic)", value = ""),
                  shiny::numericInput("fit_uncertainty_surrogate_max_step", "Profile maximum step", value = 64, min = 1, step = 1)
                ),
                shiny::conditionalPanel(
                  "input.fit_uncertainty_method === 'is' || input.fit_uncertainty_draws === 'imis'",
                  shiny::numericInput("fit_uncertainty_imis_max_iter", "IMIS maximum iterations", value = 50, min = 1, step = 1),
                  shiny::numericInput("fit_uncertainty_imis_scale_init", "IMIS initial scale", value = 1.1, min = .01, step = .1),
                  shiny::numericInput("fit_uncertainty_imis_tail_scale", "IMIS tail scale", value = 1.1, min = .01, step = .1),
                  shiny::numericInput("fit_uncertainty_is_ess", "IMIS target ESS", value = 100, min = 1, step = 10),
                  shiny::numericInput("fit_uncertainty_is_itersize", "IMIS batch size", value = 1000, min = 1, step = 100)
                ),
                shiny::conditionalPanel(
                  "input.fit_uncertainty_method === 'fullbootstrap'",
                  shiny::numericInput("fit_uncertainty_bootstrap_fit_cores", "Cores per bootstrap refit", value = 1, min = 1, step = 1),
                  shiny::numericInput("fit_uncertainty_bootstrap_tol", "Bootstrap optimizer tolerance", value = 1e-5, min = 1e-10, step = 1e-5)
                )
              )
            )
          ),
          shiny::div(
            class = "fit-inline-output",
            shiny::tags$h4("Current uncertainty"),
            shiny::verbatimTextOutput("uncertainty_summary"),
            shiny::tags$h4("Status"),
            shiny::textOutput("uncertainty_status"),
            shiny::tags$h4("Messages"),
            shiny::verbatimTextOutput("uncertainty_log"),
            shiny::tags$h4("Warnings"),
            shiny::verbatimTextOutput("uncertainty_warnings")
          )
        ),
        shiny::tabPanel(
          "Equations",
          shiny::div(
            class = "control-band",
            shiny::div(
              class = "control-grid",
              shiny::sliderInput("fit_equation_zoom", "Zoom", min = 0.5, max = 2.5, value = 1, step = 0.1),
              shiny::checkboxInput("fit_equation_split_dynamics", "Split dynamics", value = FALSE),
              shiny::checkboxInput("fit_equation_split_measurement", "Split measurement", value = FALSE),
              shiny::numericInput("fit_equation_digits", "Digits", value = 2, min = 0, max = 8, step = 1)
            )
          ),
          shiny::div(class = "equation-pane", shiny::imageOutput("fit_equation_image", inline = TRUE)),
          shiny::textOutput("fit_equation_status"),
          shiny::tags$details(
            shiny::tags$summary("LaTeX source"),
            shiny::verbatimTextOutput("fit_equation_source")
          )
        )
      )
    ),
    shiny::tabPanel(
      "Diagnostics",
      shiny::textOutput("diagnostics_status"),
      shiny::tabsetPanel(
        id = "diagnostics_tabs",
        shiny::tabPanel(
          "Generate From Fit",
          shiny::div(
            class = "control-band",
            shiny::div(
              class = "control-grid",
              shiny::numericInput("fit_gen_samples", arg_label("nsamples", "help_fit_gen_nsamples", "ctGenerateFromFit argument: nsamples"), value = 200, min = 1, step = 1),
              shiny::checkboxInput("fit_gen_follow_cores", "Use fit core selection", value = TRUE),
              shiny::numericInput("fit_gen_cores", arg_label(ctgui_core_label("cores", available_cores), "help_fit_gen_cores", "ctGenerateFromFit argument: cores"), value = default_cores, min = 1, max = available_cores, step = 1),
              shiny::checkboxInput("fit_gen_fullposterior", arg_label("fullposterior", "help_fit_gen_fullposterior", "ctGenerateFromFit argument: fullposterior"), value = FALSE),
              shiny::textAreaInput("fit_gen_extra_args", arg_label("Extra ctGenerateFromFit arguments", "help_ctGenerateFromFit", "Full ctGenerateFromFit help"), value = "", height = "70px"),
              shiny::actionButton("generate_from_fit", "Generate from fit", class = "btn-primary")
            )
          ),
          shiny::verbatimTextOutput("generated_fit_summary")
        ),
        shiny::tabPanel(
          "Covariance Check",
          shiny::div(
            class = "control-band",
            shiny::div(
              class = "control-grid",
              shiny::textInput("cov_lags", arg_label("lags", "help_cov_lags", "ctFitCovCheck argument: lags"), value = ""),
              shiny::checkboxInput("cov_cor", arg_label("cor", "help_cov_cor", "ctFitCovCheck argument: cor"), value = TRUE),
              shiny::textAreaInput("cov_extra_args", arg_label("Extra ctFitCovCheck arguments", "help_ctFitCovCheck", "Full ctFitCovCheck help"), value = "", height = "70px"),
              shiny::actionButton("run_cov_check", "Run ctFitCovCheck", class = "btn-primary")
            )
          ),
          shiny::uiOutput("cov_check_plots"),
          shiny::verbatimTextOutput("cov_check_log")
        ),
        shiny::tabPanel(
          "Prediction plots",
          shiny::div(
            class = "control-band",
            shiny::uiOutput("explain_kalman"),
            shiny::div(
              class = "control-grid",
              shiny::uiOutput("kalman_default_controls"),
              shiny::textInput("kalman_remove_obs", arg_label("removeObs", "help_kalman_removeObs", "ctPredict argument: removeObs"), value = "FALSE"),
              shiny::textInput("kalman_vec", arg_label("kalmanvec", "help_kalmanvec", "Prediction plot argument: kalmanvec"), value = "y,yprior"),
              shiny::textInput("kalman_error_vec", arg_label("errorvec", "help_errorvec", "Prediction plot argument: errorvec"), value = "auto"),
              shiny::textAreaInput("kalman_extra_args", arg_label("Extra ctPredict arguments", "help_ctPredict", "Full ctPredict help"), value = "", height = "70px"),
              shiny::actionButton("run_kalman", "Run prediction plots", class = "btn-primary")
            )
          ),
          shiny::plotOutput("kalman_plot", height = 460), ctgui_plot_export_controls("kalman_plot", 460)
        ),
        shiny::tabPanel(
          "Post Predictive",
          shiny::div(
            class = "control-band",
            shiny::uiOutput("explain_postpred"),
            shiny::tags$p("ctPostPredPlots", help_link("help_ctPostPredPlots")),
            shiny::actionButton("run_postpred", "Run ctPostPredPlots", class = "btn-primary")
          ),
          shiny::uiOutput("postpred_plots"),
          shiny::verbatimTextOutput("postpred_log")
        ),
        shiny::tabPanel(
          "Residual ACF",
          shiny::div(
            class = "control-band",
            shiny::uiOutput("explain_acf"),
            shiny::div(
              class = "control-grid",
              shiny::textInput("acf_vars", arg_label("varnames", "help_acf_varnames", "ctACFresiduals argument: varnames"), value = "auto"),
              shiny::numericInput("acf_boot", arg_label("nboot", "help_acf_nboot", "ctACFresiduals argument: nboot"), value = 100, min = 0, step = 1),
              shiny::textAreaInput("acf_extra_args", arg_label("Extra ctACFresiduals arguments", "help_ctACFresiduals", "Full ctACFresiduals help"), value = "", height = "70px"),
              shiny::actionButton("run_residual_acf", "Run residual ACF", class = "btn-primary")
            )
          ),
          shiny::plotOutput("residual_acf_plot", height = 460), ctgui_plot_export_controls("residual_acf_plot", 460),
          shiny::verbatimTextOutput("residual_acf_log")
        ),
        shiny::tabPanel(
          "Dynamics",
          shiny::div(
            class = "control-band",
            shiny::uiOutput("explain_dynamics"),
            shiny::div(
              class = "control-grid",
              shiny::textInput("dynamic_subjects", arg_label("subjects", "help_dynamic_subjects", "ctDiscretePars argument: subjects"), value = "popmean"),
              shiny::textInput("dynamic_times", arg_label("times", "help_dynamic_times", "ctDiscretePars argument: times"), value = "seq(from = 0, to = 10, by = 0.1)"),
              shiny::textInput("dynamic_samples", arg_label("nsamples", "help_dynamic_nsamples", "ctDiscretePars argument: nsamples"), value = "200"),
              shiny::checkboxInput("dynamic_observational", arg_label("observational", "help_dynamic_observational", "ctDiscretePars argument: observational"), value = FALSE),
              shiny::textInput("dynamic_ylim", "Y axis limits", value = ""),
              shiny::textAreaInput("dynamic_extra_args", arg_label("Extra ctDiscretePars arguments", "help_ctDiscretePars", "Full ctDiscretePars help"), value = "", height = "70px"),
              shiny::actionButton("run_dynamics", "Plot dynamics", class = "btn-primary")
            )
          ),
          shiny::plotOutput("dynamics_plot", height = 460), ctgui_plot_export_controls("dynamics_plot", 460),
          shiny::verbatimTextOutput("dynamics_log")
        ),
        shiny::tabPanel(
          "TI moderation",
          shiny::div(
            class = "control-band",
            shiny::div(
              class = "control-grid",
              shiny::textInput("tipred_effects_preds", arg_label("tipreds", "help_tipred_tipreds", "ctPredictTIP argument: tipreds"), value = ""),
              shiny::textInput("tipred_effects_subject", arg_label("subject", "help_tipred_subject", "ctPredictTIP argument: subject"), value = ""),
              shiny::textInput("tipred_effects_timestep", arg_label("timestep", "help_tipred_timestep", "ctPredictTIP argument: timestep"), value = ""),
              shiny::textInput("tipred_effects_tipvalues", arg_label("TIPvalues", "help_tipred_tipvalues", "ctPredictTIP argument: TIPvalues"), value = ""),
              shiny::actionButton("run_tipred_effects", "Run ctPredictTIP", class = "btn-primary")
            )
          ),
          shiny::uiOutput("tipred_effects_plots"),
          shiny::verbatimTextOutput("tipred_effects_log")
        )
      )
    ),
    shiny::tabPanel(
      "Output",
      shiny::tabsetPanel(
        shiny::tabPanel("Model Code", shiny::verbatimTextOutput("code_output")),
        shiny::tabPanel("Model PARS", shiny::tableOutput("output_pars")),
        shiny::tabPanel("Fit Summary", shiny::verbatimTextOutput("fit_summary")),
        shiny::tabPanel("Summary Matrices", shiny::verbatimTextOutput("fit_summary_matrices")),
        shiny::tabPanel("Fit Comparison", shiny::tableOutput("fit_comparison")),
        shiny::tabPanel("Generated Code", shiny::verbatimTextOutput("output_code"))
      )
    )
  )
)
}
