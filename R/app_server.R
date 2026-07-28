# Application server composition ---------------------------------------------

ctgui_app_server <- function(initial_spec, help_catalog) {
  function(input, output, session) {
  arg_label <- function(label, help_id, title = NULL) {
    ctgui_arg_label(help_catalog, label, help_id, title)
  }
  current_spec <- shiny::reactiveVal(initial_spec)
current_data <- shiny::reactiveVal(NULL)
current_data_name <- shiny::reactiveVal("No data selected")
current_fit <- shiny::reactiveVal(NULL)
fit_busy <- shiny::reactiveVal(FALSE)
fit_messages <- shiny::reactiveVal("No fit has been run.")
fit_warnings <- shiny::reactiveVal("No warnings.")
fit_status_value <- shiny::reactiveVal("No fit available.")
uncertainty_status_value <- shiny::reactiveVal("No uncertainty recomputation has been run.")
uncertainty_messages <- shiny::reactiveVal("No uncertainty recomputation has been run.")
uncertainty_warnings <- shiny::reactiveVal("No warnings.")
clear_uncertainty_state <- function() {
  uncertainty_status_value("No uncertainty recomputation has been run for the active fit.")
  uncertainty_messages("No uncertainty recomputation has been run.")
  uncertainty_warnings("No warnings.")
}
generated_fit <- shiny::reactiveVal(NULL)
cov_check <- shiny::reactiveVal(NULL)
cov_check_log <- shiny::reactiveVal("No covariance check has been run.")
kalman_result <- shiny::reactiveVal(NULL)
postpred_result <- shiny::reactiveVal(NULL)
postpred_log <- shiny::reactiveVal("No posterior predictive plots have been run.")
residual_acf <- shiny::reactiveVal(NULL)
residual_acf_log <- shiny::reactiveVal("No residual ACF has been run.")
dynamics_result <- shiny::reactiveVal(NULL)
dynamics_log <- shiny::reactiveVal("No dynamics plot has been run.")
tipred_effects_result <- shiny::reactiveVal(NULL)
tipred_effects_log <- shiny::reactiveVal("No TI predictor effects plot has been run.")
fit_registry <- shiny::reactiveVal(list())
output_code_snippets <- shiny::reactiveVal(list())
diagnostics_status <- shiny::reactiveVal("No fit diagnostics have been run.")
matrix_status <- shiny::reactiveVal("Matrix edits update the current model spec.")
plot_cache <- shiny::reactiveValues()
spec_inputs_suspended <- shiny::reactiveVal(FALSE)

sync_matrix_inputs_from_spec <- function(spec) {
  ctgui_sync_matrix_inputs_from_spec(
    session, input, spec, spec_inputs_suspended
  )
}

register_plot_export <- function(id) {
  save_plot <- function(file, type) {
    plot <- plot_cache[[id]]
    if (is.null(plot)) stop("Render the plot before exporting it")
    width <- input[[paste0(id, "_export_width")]] %||% 700
    height <- input[[paste0(id, "_export_height")]] %||% 420
    dpi <- input[[paste0(id, "_export_dpi")]] %||% 96
    if (identical(type, "png")) grDevices::png(file, width = width, height = height, res = dpi) else grDevices::pdf(file, width = width / dpi, height = height / dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    grDevices::replayPlot(plot)
  }
  output[[paste0(id, "_png")]] <- shiny::downloadHandler(filename = function() paste0(id, ".png"), content = function(file) save_plot(file, "png"))
  output[[paste0(id, "_pdf")]] <- shiny::downloadHandler(filename = function() paste0(id, ".pdf"), content = function(file) save_plot(file, "pdf"))
}
lapply(c("raw_plot", "kalman_plot", "residual_acf_plot", "dynamics_plot"), register_plot_export)

parse_names <- ctgui_parse_names

parse_optional_integer <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return(NULL)
  as.integer(x)
}

manifest_type_choices <- c(
  "Continuous" = 0L,
  "Binary" = 1L
)

explanation_text <- function(key) {
  brief <- switch(key,
    spec_data = "Choose active-data columns or type new names for each ctsem data role. Typed names remain available when no data are loaded.",
    matrices = "Use fixed numeric values or free labels; add ||FALSE to disable random effects where ctsem supports it.",
    raw_visuals = "Use these plots to inspect trajectories, variable relationships, time gaps, and missingness before fitting.",
    model_visuals = "These plots show what the current model structure implies before any fit is run.",
    fit_registry = "Save fitted models here to compare several candidate specifications.",
    kalman = "Prediction plots compare observed data with model predictions or smoothed latent states using ctPredict.",
    postpred = "Posterior predictive plots compare observed data patterns against data generated from the fitted model.",
    acf = "Residual autocorrelation helps detect predictable structure left unexplained by the model.",
    dynamics = "Discrete parameter plots show model-implied impulse responses and dynamic propagation.",
    NULL
  )
  brief
}

explain_ui <- function(key) {
  text <- explanation_text(key)
  if (is.null(text) || !nzchar(text)) return(NULL)
  shiny::tags$p(class = "help-note", text)
}

output$explain_spec_data <- shiny::renderUI(explain_ui("spec_data"))
output$explain_raw_visuals <- shiny::renderUI(explain_ui("raw_visuals"))
output$explain_model_visuals <- shiny::renderUI(explain_ui("model_visuals"))
output$explain_fit_registry <- shiny::renderUI(explain_ui("fit_registry"))
output$explain_kalman <- shiny::renderUI(explain_ui("kalman"))
output$explain_postpred <- shiny::renderUI(explain_ui("postpred"))
output$explain_acf <- shiny::renderUI(explain_ui("acf"))
output$explain_dynamics <- shiny::renderUI(explain_ui("dynamics"))

show_help <- function(help) {
  text <- help$text %||% ctgui_ctsem_help_text(help$topic, help$param %||% NULL)
  title <- help$title %||% if (is.null(help$param)) paste0("ctsem::", help$topic) else paste(help$topic, "-", help$param)
  shiny::showModal(shiny::modalDialog(
    title = title,
    if (!is.null(help$text)) {
      shiny::tags$p(text)
    } else if (!is.null(help$topic)) {
      shiny::tags$div(class = "ctgui-rd-help", text)
    } else {
      text
    },
    size = if (!is.null(help$topic)) "l" else "m",
    easyClose = TRUE,
    footer = shiny::modalButton("Close")
  ))
}
register_help <- function(help_id) {
  local({
    id <- help_id
    shiny::observeEvent(input[[id]], show_help(help_catalog[[id]]), ignoreInit = TRUE)
  })
}
lapply(names(help_catalog), register_help)

manifest_type_values <- function(manifest_names = parse_names(input$manifest_names)) {
  ctgui_manifest_type_values(
    manifest_names, shiny::reactiveValuesToList(input),
    current_spec()$manifest_type
  )
}

input_spec_fields <- function(committed = NULL) {
  values <- shiny::reactiveValuesToList(input)
  if (is.list(committed)) {
    for (name in names(committed)) values[[name]] <- committed[[name]]
  }
  values$Tpoints <- NULL
  ctgui_spec_fields(values, current_spec())
}

spec_fields_changed <- ctgui_spec_fields_changed

parse_r_expression <- function(x, default) {
  if (is.null(x) || !nzchar(trimws(x))) return(default)
  tryCatch(eval(parse(text = x), envir = baseenv()), error = function(e) default)
}

parse_optional_expression <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(structure(list(), class = "ctgui_omitted_arg"))
  value <- tryCatch(eval(parse(text = x), envir = baseenv()), error = function(e) e)
  if (inherits(value, "error")) return(x)
  value
}

parse_keyword_or_expression <- function(x, keywords = character()) {
  if (is.null(x) || !nzchar(trimws(x))) return(structure(list(), class = "ctgui_omitted_arg"))
  text <- trimws(x)
  if (tolower(text) %in% tolower(keywords)) return(text)
  parse_optional_expression(text)
}

parse_text_vector <- function(x, default = character()) {
  if (is.null(x) || !nzchar(trimws(x))) return(default)
  parsed <- tryCatch(eval(parse(text = x), envir = baseenv()), error = function(e) e)
  if (!inherits(parsed, "error")) return(parsed)
  values <- trimws(unlist(strsplit(x, ","), use.names = FALSE))
  values[nzchar(values)]
}

is_omitted_arg <- function(x) inherits(x, "ctgui_omitted_arg")

generate_from_fit_cores <- function() {
  if (!identical(input$fit_gen_follow_cores, FALSE)) input$fit_cores else input$fit_gen_cores
}

shiny::observeEvent(input$fit_cores, {
  if (isTRUE(input$fit_gen_follow_cores)) {
    shiny::updateNumericInput(session, "fit_gen_cores", value = input$fit_cores)
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$fit_gen_cores, {
  if (isTRUE(input$fit_gen_follow_cores) &&
      !identical(as.integer(input$fit_gen_cores), as.integer(input$fit_cores))) {
    shiny::updateCheckboxInput(session, "fit_gen_follow_cores", value = FALSE)
  }
}, ignoreInit = TRUE)

cov_check_lags <- function() {
  text <- input$cov_lags
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  parse_r_expression(text, 0:3)
}

parse_extra_args <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(list())
  text <- trimws(x)
  expr <- if (startsWith(text, "list(")) text else paste0("list(", text, ")")
  value <- tryCatch(eval(parse(text = expr), envir = baseenv()), error = function(e) e)
  if (inherits(value, "error")) stop(conditionMessage(value), call. = FALSE)
  if (!is.list(value)) stop("Extra arguments must evaluate to a named list", call. = FALSE)
  if (length(value) && (is.null(names(value)) || any(!nzchar(names(value))))) {
    stop("Extra arguments must be named, for example standardisederrors = TRUE", call. = FALSE)
  }
  value
}

append_extra_args <- function(args, extra_text, protected = names(args)) {
  extra <- parse_extra_args(extra_text)
  if (!length(extra)) return(args)
  extra <- extra[!names(extra) %in% protected]
  c(args, extra)
}

progress_like_message <- function(text) {
  text <- trimws(text)
  if (!nzchar(text)) return(FALSE)
  grepl("\r", text, fixed = TRUE) ||
  grepl("(?i)(hessian|iter|iteration|elapsed|optim|optimization|chain|warmup|sampling|draws|gradient|stepsize|objective|progress|bootstrap|boot|\\d+\\s*/\\s*\\d+)", text, perl = TRUE)
}

compact_condition_messages <- function(messages, progress = character()) {
  messages <- trimws(messages)
  messages <- messages[nzchar(messages)]
  progress <- trimws(progress)
  progress <- progress[nzchar(progress)]
  c(messages, if (length(progress)) progress[length(progress)] else character())
}

capture_conditions <- function(expr, progress_callback = NULL) {
  messages <- character()
  progress <- character()
  warnings <- character()
  append_message <- function(text) {
    pieces <- unlist(strsplit(conditionMessage(text), "\r", fixed = TRUE), use.names = FALSE)
    pieces <- trimws(pieces)
    pieces <- pieces[nzchar(pieces)]
    for (piece in pieces) {
      if (progress_like_message(piece)) {
        progress <<- c(progress, piece)
        if (is.function(progress_callback)) progress_callback(compact_condition_messages(messages, progress))
      } else {
        messages <<- c(messages, piece)
      }
    }
  }
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    message = function(m) {
      append_message(m)
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = compact_condition_messages(messages, progress), warnings = warnings)
}

r_data_names <- function() {
  objects <- ls(envir = .GlobalEnv)
  objects[vapply(objects, function(name) {
    object <- get(name, envir = .GlobalEnv)
    is.data.frame(object) || is.matrix(object)
  }, logical(1L))]
}

update_data_choices <- function(selected = NULL) {
  choices <- r_data_names()
  current <- selected %||% shiny::isolate(input$env_data)
  if (is.null(current) || !nzchar(current) || !current %in% choices) current <- ""
  shiny::updateSelectInput(
    session,
    "env_data",
    choices = c("Select R data" = "", choices),
    selected = current
  )
}

update_fit_choices <- function(selected = NULL) {
  names <- names(fit_registry())
  shiny::updateSelectInput(session, "active_fit_name", choices = names, selected = selected %||% names[1L])
}

active_fit <- function() {
  registry <- fit_registry()
  selected <- input$active_fit_name
  if (!is.null(selected) && nzchar(selected) && selected %in% names(registry)) return(registry[[selected]])
  current_fit()
}

replace_active_fit <- function(fit) {
  current_fit(fit)
  selected <- input$active_fit_name
  if (!is.null(selected) && nzchar(selected)) {
    registry <- fit_registry()
    if (selected %in% names(registry)) {
      registry[[selected]] <- fit
      fit_registry(registry)
    }
  }
  invisible(fit)
}

uncertainty_control <- function() {
  ctgui_uncertainty_control(
    ridge = input$fit_uncertainty_ridge,
    hessian_step = input$fit_uncertainty_hessian_step,
    surrogate_npoints = input$fit_uncertainty_surrogate_npoints,
    surrogate_scale = input$fit_uncertainty_surrogate_scale,
    surrogate_profile = input$fit_uncertainty_surrogate_profile,
    surrogate_profile_target_drop = input$fit_uncertainty_surrogate_target_drop,
    surrogate_profile_max_step = input$fit_uncertainty_surrogate_max_step,
    imis_max_iter = input$fit_uncertainty_imis_max_iter,
    imis_scale_init = input$fit_uncertainty_imis_scale_init,
    imis_tail_scale = input$fit_uncertainty_imis_tail_scale,
    is_ess = input$fit_uncertainty_is_ess,
    is_itersize = input$fit_uncertainty_is_itersize,
    bootstrap_fit_cores = input$fit_uncertainty_bootstrap_fit_cores,
    bootstrap_tol = input$fit_uncertainty_bootstrap_tol
  )
}

uncertainty_optimcontrol <- function() {
  ctgui_uncertainty_optimcontrol(
    method = input$fit_uncertainty_method,
    draws = ctgui_uncertainty_default_draws(input$fit_uncertainty_method),
    finishsamples = input$fit_uncertainty_samples,
    control = uncertainty_control()
  )
}

valid_object_name <- function(x) length(x) == 1L && grepl("^[.A-Za-z][.A-Za-z0-9_]*$", x)
assign_r_object <- function(object, name, label) {
  name <- trimws(name %||% "")
  if (!valid_object_name(name)) {
    shiny::showNotification(paste("Use a valid R object name for", label), type = "error")
    return(FALSE)
  }
  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    shiny::showNotification(paste("Replaced existing R object", name), type = "warning")
  }
  assign(name, object, envir = .GlobalEnv)
  shiny::showNotification(paste("Returned", label, "as", name), type = "message")
  TRUE
}

is_ctsem_model <- function(x) !is.null(x$pars) && !is.null(x$latentNames) && !is.null(x$manifestNames)

output$uncertainty_eligibility <- shiny::renderUI({
  eligibility <- ctgui_optim_uncertainty_eligibility(active_fit())
  class <- if (isTRUE(eligibility$ok)) "help-note" else "warning-note"
  shiny::tags$p(class = class, eligibility$message)
})

output$download_model_rds <- shiny::downloadHandler(
  filename = function() "ctsem-model.rds",
  content = function(file) saveRDS(ctgui_to_ctsem_model(current_spec(), silent = TRUE), file)
)
output$download_fit_rds <- shiny::downloadHandler(
  filename = function() "ctsem-fit.rds",
  content = function(file) {
    fit <- active_fit(); if (is.null(fit)) stop("No current fit to save")
    saveRDS(fit, file)
  }
)
shiny::observeEvent(input$assign_model, {
  model <- tryCatch(ctgui_to_ctsem_model(current_spec(), silent = TRUE), error = function(e) e)
  if (inherits(model, "error")) shiny::showNotification(conditionMessage(model), type = "error") else assign_r_object(model, input$model_object_name, "ctModel object")
})
shiny::observeEvent(input$assign_fit, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("No fit is available to return", type = "error")
    return()
  }
  shiny::showModal(shiny::modalDialog(
    title = "Return fit to R",
    shiny::textInput("assign_fit_object_name", "R object name", value = "fit"),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("confirm_assign_fit", "Return fit", class = "btn-primary")
    )
  ))
})
shiny::observeEvent(input$confirm_assign_fit, {
  fit <- active_fit()
  if (!is.null(fit) && assign_r_object(fit, input$assign_fit_object_name, "fit object")) {
    shiny::removeModal()
  }
})
shiny::observeEvent(input$load_model_rds, {
  path <- input$load_model_rds$datapath; if (is.null(path)) return()
  loaded <- tryCatch(ctgui_project_spec(readRDS(path)), error = function(e) e)
  if (inherits(loaded, "error")) shiny::showNotification(conditionMessage(loaded), type = "error") else {
    commit_current_spec(loaded, reason = "load_project")
    fit_status_value("Loaded ctsem model from RDS."); shiny::showNotification("Loaded model RDS", type = "message")
  }
})
shiny::observeEvent(input$load_fit_rds, {
  path <- input$load_fit_rds$datapath; if (is.null(path)) return()
  fit <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(fit, "error") || !ctgui_ctsem_is_fit(fit)) {
    shiny::showNotification(if (inherits(fit, "error")) conditionMessage(fit) else "The RDS does not contain a ctsem fit", type = "error"); return()
  }
  current_fit(fit); clear_uncertainty_state(); fit_status_value("Loaded fit from RDS."); shiny::showNotification("Loaded fit RDS", type = "message")
})
clear_diagnostics <- function() {
  generated_fit(NULL)
  cov_check(NULL)
  kalman_result(NULL)
  postpred_result(NULL)
  residual_acf(NULL)
  dynamics_result(NULL)
  tipred_effects_result(NULL)
  postpred_log("No posterior predictive plots have been run.")
  residual_acf_log("No residual ACF has been run.")
  dynamics_log("No dynamics plot has been run.")
  tipred_effects_log("No TI predictor effects plot has been run.")
}

# The single server-side mutation boundary.  Domain modules may decide how to
# present the effects, but every authored specification first passes through
# the canonical controller so matrices, annotations, PARS and ctsem state stay
# synchronized.
commit_current_spec <- function(updated, reason = "edit", refresh_visual = NULL,
    refresh_widgets = TRUE) {
  commit <- if (inherits(updated, "ctgui_spec_commit")) {
    updated
  } else {
    ctgui_commit_spec(
      previous = current_spec(), updated = updated, reason = reason,
      refresh_visual = refresh_visual, refresh_widgets = refresh_widgets
    )
  }
  current_spec(commit$spec)
  effects <- commit$effects
  if (isTRUE(effects$invalidate_fit)) {
    current_fit(NULL)
    clear_uncertainty_state()
    clear_diagnostics()
  }
  if (isTRUE(effects$refresh_widgets)) sync_matrix_inputs_from_spec(commit$spec)
  invisible(effects)
}

visual_server <- ctgui_visual_server(
  input = input, output = output, session = session,
  current_spec = current_spec, current_data = current_data,
  commit_current_spec = commit_current_spec,
  sync_matrix_inputs_from_spec = sync_matrix_inputs_from_spec,
  fit_status_value = fit_status_value, matrix_status = matrix_status
)

matrix_id_part <- ctgui_matrix_id_part
matrix_cell_id <- ctgui_matrix_cell_id

shiny::observe(update_data_choices())

output$data_spec_controls <- shiny::renderUI({
  ctgui_data_roles_ui(current_spec(), current_data())
})

shiny::observeEvent(input$spec_add_variable, {
  item <- input$spec_add_variable
  if (!is.list(item)) return()
  commit <- tryCatch(
    ctgui_add_spec_variable(
      current_spec(), as.character(item$kind %||% ""),
      as.character(item$name %||% ""), as.character(item$measuring %||% "")
    ),
    error = function(e) e
  )
  if (inherits(commit, "error")) {
    shiny::showNotification(conditionMessage(commit), type = "error")
    return()
  }
  commit_current_spec(commit)
  fit_status_value("Model changed. Refit when ready.")
  matrix_status("Added variable to the current model specification.")
}, ignoreInit = TRUE)

output$manifest_type_controls <- shiny::renderUI({
  manifest_names <- parse_names(input$manifest_names)
  if (length(manifest_names) == 0L) return(NULL)
  current <- current_spec()$manifest_type
  if (length(current) != length(manifest_names)) current <- rep(0L, length(manifest_names))
  shiny::tagList(
    shiny::tags$h4("Manifest variable types"),
    shiny::tags$div(class = "help-note",
      shiny::tags$p("Choose how each observed manifest variable is treated by ctsem."),
      shiny::tags$ul(
        shiny::tags$li(shiny::tags$b("Continuous:"), " numeric measurement with Gaussian residual error."),
        shiny::tags$li(shiny::tags$b("Binary:"), " two-category 0/1 measurement using ctsem's binary manifest-variable handling.")
      )
    ),
    shiny::div(
      class = "manifest-type-grid",
      lapply(seq_along(manifest_names), function(i) {
        shiny::selectInput(
          paste0("manifest_type_", i),
          paste(manifest_names[i], "variable type"),
          choices = manifest_type_choices,
          selected = as.character(current[i])
        )
      })
    )
  )
})

output$matrix_builder_ui <- shiny::renderUI({
  spec <- current_spec()
  latent_choices <- spec$latent_names
  manifest_choices <- spec$manifest_names
  structure <- input$matrix_builder_structure %||% "dynamic_var"
  measurement <- input$measurement_builder_type %||% "single_indicator"
  trend_controls <- if (identical(structure, "dynamic_var_trend")) {
    shiny::tagList(
      shiny::selectizeInput("matrix_builder_trend_latents", "Trend latents",
        choices = latent_choices, selected = utils::tail(latent_choices, length(input$matrix_builder_dynamic_latents %||% latent_choices)), multiple = TRUE),
      shiny::selectInput("matrix_builder_trend_type", "Trend process",
        choices = c("Linear" = "linear", "Exponential" = "exponential"), selected = "linear"),
      shiny::selectInput("matrix_builder_trend_coupling", "Trend coupling",
        choices = c("Fixed to 1" = "fixed", "Free parameter" = "free"), selected = "fixed")
    )
  } else NULL
  measurement_controls <- if (!identical(measurement, "single_indicator")) {
    shiny::tagList(
      shiny::textInput("measurement_manifest_blocks", "Manifest blocks per factor",
        value = paste(manifest_choices, collapse = "; ")),
      if (identical(measurement, "fixed_loadings")) {
        shiny::textInput("measurement_fixed_loading", "Fixed non-marker loading", value = "0.75")
      }
    )
  } else NULL
  shiny::tagList(
    shiny::tags$h4("Matrix Builder"),
    shiny::tags$p(class = "help-note", "Specification defines model names. These controls only populate matrices for the current spec."),
    shiny::div(
      class = "control-grid",
      shiny::selectInput("matrix_builder_structure", "Dynamic matrix structure",
        choices = stats::setNames(ctgui_structures()$id, ctgui_structures()$title),
        selected = structure),
      shiny::selectizeInput("matrix_builder_dynamic_latents", "Dynamic / level latents",
        choices = latent_choices, selected = latent_choices, multiple = TRUE),
      if (identical(structure, "linear_growth")) {
        shiny::selectizeInput("matrix_builder_slope_latents", "Slope latents",
          choices = latent_choices, selected = character(), multiple = TRUE)
      },
      trend_controls,
      shiny::checkboxInput("matrix_builder_noise_cor", "Free system-noise correlations", value = TRUE),
      shiny::actionButton("matrix_builder_apply", "Apply dynamic matrices", class = "btn-primary")
    ),
    shiny::tags$hr(),
    shiny::div(
      class = "control-grid",
      shiny::selectInput("measurement_builder_type", "Measurement matrix preset",
        choices = stats::setNames(ctgui_measurements()$id, ctgui_measurements()$title),
        selected = measurement),
      shiny::selectizeInput("measurement_factor_latents", "Measured factor latents",
        choices = latent_choices, selected = utils::head(latent_choices, min(length(latent_choices), length(manifest_choices))), multiple = TRUE),
      measurement_controls,
      if (identical(structure, "dynamic_var_trend")) {
        shiny::selectizeInput("measurement_trend_latents", "Trend latents sharing measurement",
          choices = latent_choices, selected = input$matrix_builder_trend_latents %||% character(), multiple = TRUE)
      },
      shiny::actionButton("measurement_builder_apply", "Apply measurement matrices")
    )
  )
})

shiny::observeEvent(input$matrix_builder_apply, {
  spec <- current_spec()
  structure <- input$matrix_builder_structure %||% "dynamic_var"
  options <- list(
    dynamic_latents = input$matrix_builder_dynamic_latents %||% spec$latent_names,
    level_latents = input$matrix_builder_dynamic_latents %||% spec$latent_names,
    slope_latents = input$matrix_builder_slope_latents %||% character(),
    trend_latents = input$matrix_builder_trend_latents %||% character(),
    trend_type = input$matrix_builder_trend_type %||% "linear",
    trend_coupling = input$matrix_builder_trend_coupling %||% "fixed",
    free_noise_correlations = isTRUE(input$matrix_builder_noise_cor)
  )
  updated <- tryCatch(ctgui_build_matrices(spec, structure = structure, options = options), error = function(e) e)
  if (inherits(updated, "error")) {
    shiny::showNotification(conditionMessage(updated), type = "error")
    return()
  }
  commit_current_spec(updated, reason = "data_roles")
  fit_status_value("Model matrices changed. Refit when ready.")
  matrix_status(paste("Applied", structure, "matrices without changing specification names."))
  shiny::updateSelectInput(session, "model_visual_matrix", choices = ctgui_matrix_names(updated), selected = "DRIFT")
})

shiny::observeEvent(input$measurement_builder_apply, {
  spec <- current_spec()
  factors <- input$measurement_factor_latents %||% spec$latent_names
  blocks <- input$measurement_manifest_blocks %||% NULL
  fixed_value <- suppressWarnings(as.numeric(input$measurement_fixed_loading %||% 0.75))
  if (is.na(fixed_value)) fixed_value <- 0.75
  fixed_loadings <- replicate(length(factors), c(1, fixed_value), simplify = FALSE)
  updated <- tryCatch(ctgui_build_measurement_matrices(
    spec,
    measurement = input$measurement_builder_type %||% "single_indicator",
    options = list(
      factor_latents = factors,
      trend_latents = input$measurement_trend_latents %||% character(),
      manifest_blocks = blocks,
      fixed_loadings = fixed_loadings
    )
  ), error = function(e) e)
  if (inherits(updated, "error")) {
    shiny::showNotification(conditionMessage(updated), type = "error")
    return()
  }
  commit_current_spec(updated, reason = "specification")
  fit_status_value("Measurement matrices changed. Refit when ready.")
  matrix_status("Applied measurement matrices without changing specification names.")
  shiny::updateSelectInput(session, "model_visual_matrix", choices = ctgui_matrix_names(updated), selected = "LAMBDA")
})

rebuild_spec_if_needed <- function(committed = NULL) {
  # An explicit browser payload contains the values authored before a page
  # transition and must not be discarded merely because widget synchronization
  # from an earlier commit is still completing.
  if (is.null(committed) && isTRUE(spec_inputs_suspended())) {
    return(invisible(FALSE))
  }
  fields <- input_spec_fields(committed)
  spec <- current_spec()
  if (!spec_fields_changed(spec, fields)) return(invisible(FALSE))

  commit <- tryCatch(
    ctgui_commit_spec_fields(spec, fields, reason = "specification"),
    error = function(e) e
  )
  if (inherits(commit, "error")) {
    matrix_status(paste("Specification not rebuilt:", conditionMessage(commit)))
    shiny::showNotification(conditionMessage(commit), type = "error")
    return(invisible(FALSE))
  }

  commit_current_spec(commit)
  new_spec <- commit$spec
  fit_status_value("Model changed. Refit when ready.")
  shiny::updateSelectInput(session, "model_visual_matrix", choices = ctgui_matrix_names(new_spec), selected = "DRIFT")
  matrix_status("Matrix edits update the current model spec.")
  invisible(TRUE)
}
matrix_server <- ctgui_matrix_server(
  input = input, output = output, session = session,
  current_spec = current_spec, commit_current_spec = commit_current_spec,
  matrix_status = matrix_status, fit_status_value = fit_status_value,
  visual_refresh = function(spec, view) {
    visual_server$refresh(spec, view = view)
  },
  register_plot_export = register_plot_export, plot_cache = plot_cache,
  arg_label = arg_label
)

shiny::observeEvent(input$tab_commit_nonce, {
  event <- input$tab_commit_nonce
  committed <- if (is.list(event)) event$specification else NULL
  specification_changed <- rebuild_spec_if_needed(committed)
  # Matrix fields have their own atomic change event. Retain the legacy tab
  # fallback only when this transition did not rebuild the matrix schema.
  if (!isTRUE(specification_changed)) {
    matrix_server$apply_current_matrix(show_notification = FALSE)
  }
})

equation_args <- shiny::reactive({
  list(
    splitDynamics = isTRUE(input$equation_split_dynamics),
    splitMeasurement = isTRUE(input$equation_split_measurement),
    digits = input$equation_digits %||% 2
  )
})

fit_equation_args <- shiny::reactive({
  list(
    splitDynamics = isTRUE(input$fit_equation_split_dynamics),
    splitMeasurement = isTRUE(input$fit_equation_split_measurement),
    digits = input$fit_equation_digits %||% 2
  )
})

fit_model_object <- function(fit) {
  ctgui_ctsem_fit_model(fit)
}

model_latex_source <- function(model, args, fallback = NULL) {
  if (is.null(model)) return("No model object is available from the fit.")
  out <- tryCatch(ctgui_ctsem_call("ctModelLatex", .args = c(list(
    model,
    compile = FALSE,
    open = FALSE,
    equationonly = TRUE,
    includeNote = FALSE
  ), args)), error = function(e) e)
  if (inherits(out, "error") && !is.null(fallback)) return(model_latex_source(fallback, args))
  if (inherits(out, "error")) paste("Could not create equations:", conditionMessage(out)) else out
}

model_latex_png <- function(model, args, prefix, fallback = NULL) {
  if (is.null(model)) stop("No model object is available from the fit.", call. = FALSE)
  filename <- paste0(prefix, "_", Sys.getpid(), "_", as.integer(Sys.time()), "_", sample.int(1e6, 1L))
  out <- tryCatch(ctgui_ctsem_call("ctModelLatex", .args = c(list(
    model,
    compile = TRUE,
    open = FALSE,
    equationonly = FALSE,
    includeNote = FALSE,
    savepng = TRUE,
    folder = tempdir(),
    filename = filename
  ), args)), error = function(e) e)
  if (inherits(out, "error") && !is.null(fallback)) return(model_latex_png(fallback, args, prefix))
  if (inherits(out, "error")) stop(conditionMessage(out), call. = FALSE)
  png <- file.path(tempdir(), paste0(filename, ".png"))
  if (!file.exists(png)) stop("ctModelLatex did not create a PNG file", call. = FALSE)
  png
}

latex_source <- shiny::reactive({
  args <- c(list(spec = current_spec()), equation_args())
  tryCatch(do.call(ctgui_latex, args), error = function(e) paste("Could not create equations:", conditionMessage(e)))
})

equation_png <- shiny::reactive({
  args <- c(list(spec = current_spec()), equation_args())
  tryCatch(do.call(ctgui_latex_png, args), error = function(e) e)
})

output$equation_image <- shiny::renderImage({
  png <- equation_png()
  if (inherits(png, "error")) return(list(src = "", alt = conditionMessage(png)))
  list(
    src = png,
    contentType = "image/png",
    width = paste0(round(input$equation_zoom * 100), "%"),
    alt = "ctsem model equations"
  )
}, deleteFile = FALSE)

output$equation_status <- shiny::renderText({
  png <- equation_png()
  if (inherits(png, "error")) paste("Equation image unavailable:", conditionMessage(png)) else ""
})

output$equation_source <- shiny::renderText(latex_source())

fit_latex_source <- shiny::reactive({
  fit <- active_fit()
  if (is.null(fit)) return("No fit available.")
  model_latex_source(fit, fit_equation_args(), fallback = fit_model_object(fit))
})

fit_equation_png <- shiny::reactive({
  fit <- active_fit()
  if (is.null(fit)) return(simpleError("No fit available."))
  tryCatch(model_latex_png(fit, fit_equation_args(), "ctgui_fit_equations", fallback = fit_model_object(fit)), error = function(e) e)
})

output$fit_equation_image <- shiny::renderImage({
  png <- fit_equation_png()
  if (inherits(png, "error")) return(list(src = "", alt = conditionMessage(png)))
  list(
    src = png,
    contentType = "image/png",
    width = paste0(round((input$fit_equation_zoom %||% 1) * 100), "%"),
    alt = "ctsem fit equations"
  )
}, deleteFile = FALSE)

output$fit_equation_status <- shiny::renderText({
  png <- fit_equation_png()
  if (inherits(png, "error")) paste("Fit equation image unavailable:", conditionMessage(png)) else ""
})

output$fit_equation_source <- shiny::renderText(fit_latex_source())
output$validation_table_spec <- shiny::renderTable(ctgui_validate(current_spec()), rownames = FALSE)

output$kalman_default_controls <- shiny::renderUI({
  shiny::tagList(
    shiny::textInput("kalman_subjects", arg_label("subjects", "help_kalman_subjects", "ctPredict argument: subjects"), value = ""),
    shiny::textInput("kalman_timerange", arg_label("timerange", "help_kalman_timerange", "ctPredict argument: timerange"), value = ""),
    shiny::textInput("kalman_timestep", arg_label("timestep", "help_kalman_timestep", "ctPredict argument: timestep"), value = "")
  )
})

output$model_visual_controls <- shiny::renderUI({
  spec <- current_spec()
  choices <- c(
    "Temporal dynamics graph",
    "System noise graph",
    "Measurement graph",
    if (!is.null(spec$builder) && identical(spec$builder$structure, "dynamic_var_trend")) "Trend structure graph",
    "Generated trajectories"
  )
  view <- input$model_visual_type %||% choices[1L]
  if (!view %in% choices) view <- choices[1L]
  shiny::div(
    class = "control-grid",
    shiny::selectInput("model_visual_type", "View", choices = choices, selected = view),
    if (identical(view, "Generated trajectories")) {
      shiny::tagList(
        shiny::numericInput("model_visual_subjects", "Generated subjects", value = 6, min = 1, step = 1),
        shiny::numericInput("model_visual_tpoints", "Generated time points", value = 20, min = 1, step = 1)
      )
    }
  )
})

output$model_visual_plot <- shiny::renderPlot({
  spec <- current_spec()
  view <- input$model_visual_type %||% "Temporal dynamics graph"
  record_output_code("model_visual", output_code_snippet("model_visual"))
  if (view %in% c("Temporal dynamics graph", "System noise graph", "Measurement graph", "Trend structure graph")) {
    element <- switch(view,
      `Temporal dynamics graph` = "drift",
      `System noise graph` = "diffusion",
      `Measurement graph` = "measurement",
      `Trend structure graph` = "trend"
    )
    edges <- ctgui_graph_edges(spec, element)
    graphics::plot.new()
    if (nrow(edges) == 0L) {
      graphics::text(0.5, 0.5, paste("No", element, "edges to show"), cex = 0.9)
      return(invisible(NULL))
    }
    nodes <- unique(c(edges$from, edges$to))
    theta <- seq(0, 2 * pi, length.out = length(nodes) + 1L)[-length(nodes) - 1L]
    coords <- data.frame(name = nodes, x = cos(theta), y = sin(theta))
    graphics::plot.window(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3), asp = 1)
    draw_edge <- function(from, to, directed, col = "grey35") {
      from_xy <- coords[coords$name == from, ]
      to_xy <- coords[coords$name == to, ]
      if (nrow(from_xy) == 0L || nrow(to_xy) == 0L) return()
      if (identical(from, to)) {
        graphics::symbols(from_xy$x + 0.09, from_xy$y + 0.09, circles = 0.08,
          inches = FALSE, add = TRUE, fg = col)
        return()
      }
      if (isTRUE(directed)) {
        dx <- to_xy$x - from_xy$x
        dy <- to_xy$y - from_xy$y
        distance <- sqrt(dx^2 + dy^2)
        if (is.finite(distance) && distance > 0) {
          node_radius <- 0.18
          start_x <- from_xy$x + node_radius * dx / distance
          start_y <- from_xy$y + node_radius * dy / distance
          end_x <- to_xy$x - node_radius * dx / distance
          end_y <- to_xy$y - node_radius * dy / distance
          graphics::arrows(start_x, start_y, end_x, end_y,
            length = 0.1, angle = 22, code = 2, col = col, lwd = 1.6)
        }
      } else {
        graphics::segments(from_xy$x, from_xy$y, to_xy$x, to_xy$y, col = col, lwd = 1.6)
      }
    }
    edge_col <- switch(element,
      drift = "steelblue",
      diffusion = "purple4",
      measurement = "darkgreen",
      trend = "firebrick",
      "grey35"
    )
    for (i in seq_len(nrow(edges))) draw_edge(edges$from[i], edges$to[i], edges$directed[i], edge_col)
    graphics::points(coords$x, coords$y, pch = 21, bg = "white", cex = 4)
    graphics::text(coords$x, coords$y, coords$name, cex = 0.9)
    graphics::title(view)
    return(invisible(NULL))
  }
  data <- tryCatch(ctgui_generate_data(
    spec,
    n.subjects = input$model_visual_subjects,
    Tpoints = input$model_visual_tpoints,
    free_defaults = TRUE,
    wide = FALSE
  ), error = function(e) e)
  if (inherits(data, "error")) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, conditionMessage(data), cex = 0.8)
    return(invisible(NULL))
  }
  y <- spec$manifest_names[1L]
  if (!y %in% names(data)) y <- names(data)[vapply(data, is.numeric, logical(1L))][1L]
  graphics::plot(data[[spec$time]], data[[y]], type = "n", xlab = spec$time, ylab = y,
    main = "Generated trajectories from current spec")
  for (id in unique(data[[spec$id]])) {
    rows <- data[[spec$id]] %in% id
    ordered <- order(data[[spec$time]][rows])
    graphics::lines(data[[spec$time]][rows][ordered], data[[y]][rows][ordered],
      col = grDevices::adjustcolor("steelblue", 0.35))
  }
})

set_output_code_snippet <- function(key, lines) {
  snippets <- output_code_snippets()
  snippets[[key]] <- paste(lines, collapse = "\n")
  output_code_snippets(snippets)
}

record_output_code <- function(key, lines) {
  shiny::isolate(set_output_code_snippet(key, lines))
}

output_data_source <- shiny::reactive({
  data <- current_data()
  data_name <- current_data_name()
  if (identical(data_name, "Generated data")) {
    return(ctgui_output_data_source("generated", generation = list(
      n.subjects = input$gen_subjects,
      Tpoints = input$gen_tpoints,
      burnin = input$gen_burnin,
      dtmean = input$gen_dtmean,
      logdtsd = input$gen_logdtsd,
      wide = FALSE
    )))
  }
  if (startsWith(data_name, "R data: ")) {
    return(ctgui_output_data_source(
      "r_object", sub("^R data: ", "", data_name)))
  }
  if (startsWith(data_name, "R data.frame: ")) {
    return(ctgui_output_data_source(
      "r_object", sub("^R data\\.frame: ", "", data_name)))
  }
  if (startsWith(data_name, "CSV: ")) {
    return(ctgui_output_data_source("csv", sub("^CSV: ", "", data_name)))
  }
  if (startsWith(data_name, "RDS: ")) {
    return(ctgui_output_data_source("rds", sub("^RDS: ", "", data_name)))
  }
  if (!is.null(data)) return(ctgui_output_data_source("session"))
  ctgui_output_data_source("none")
})

output_code_options <- function(action) {
  switch(action,
    fit = list(
      optimize = input$fit_optimize,
      priors = input$fit_priors,
      cores = input$fit_cores,
      uncertainty = input$fit_uncertainty_method,
      uncertainty_draws = ctgui_uncertainty_default_draws(input$fit_uncertainty_method),
      finishsamples = input$fit_uncertainty_samples,
      uncertainty_control = uncertainty_control(),
      extra_args = parse_extra_args(input$fit_extra_args)
    ),
    uncertainty = list(
      uncertainty = input$fit_uncertainty_method,
      draws = ctgui_uncertainty_default_draws(input$fit_uncertainty_method),
      finishsamples = input$fit_uncertainty_samples,
      cores = input$fit_cores,
      uncertainty_control = uncertainty_control()
    ),
    raw_plot = list(
      plot_type = input$raw_plot_type,
      time = input$raw_plot_time,
      variables = input$raw_plot_vars,
      id = input$raw_plot_subject,
      colour = input$raw_plot_colour
    ),
    model_visual = list(visual_type = input$model_visual_type),
    generate_from_fit = list(
      nsamples = input$fit_gen_samples,
      fullposterior = input$fit_gen_fullposterior,
      cores = generate_from_fit_cores(),
      extra_args = parse_extra_args(input$fit_gen_extra_args)
    ),
    cov_check = list(
      lags = cov_check_lags(),
      cor = input$cov_cor,
      cores = 1L,
      extra_args = parse_extra_args(input$cov_extra_args)
    ),
    kalman = list(
      subjects = input$kalman_subjects,
      timerange = input$kalman_timerange,
      timestep = input$kalman_timestep,
      removeObs = input$kalman_remove_obs,
      kalmanvec = parse_text_vector(input$kalman_vec, c("y", "yprior")),
      errorvec = parse_text_vector(input$kalman_error_vec, "auto"),
      extra_args = parse_extra_args(input$kalman_extra_args)
    ),
    residual_acf = list(
      varnames = parse_text_vector(input$acf_vars, "auto"),
      nboot = input$acf_boot,
      extra_args = parse_extra_args(input$acf_extra_args)
    ),
    dynamics = list(
      subjects = input$dynamic_subjects,
      times = input$dynamic_times,
      nsamples = input$dynamic_samples,
      observational = input$dynamic_observational,
      cores = 1L,
      ylim = input$dynamic_ylim,
      extra_args = parse_extra_args(input$dynamic_extra_args)
    ),
    tipred = list(
      tipreds = input$tipred_effects_preds,
      subject = input$tipred_effects_subject,
      timestep = input$tipred_effects_timestep,
      TIPvalues = input$tipred_effects_tipvalues
    ),
    list()
  )
}

output_code_snippet <- function(action) {
  ctgui_output_snippet(action, output_code_options(action), current_spec())
}

workflow_code <- shiny::reactive({
  ctgui_output_workflow_code(
    current_spec(), output_data_source(), output_code_snippets())
})

model_code <- shiny::reactive({
  paste(c("# Model specification", ctgui_export_code(current_spec())), collapse = "\n")
})

output$code_output <- shiny::renderText(model_code())
output$output_code <- shiny::renderText(workflow_code())

output$output_pars <- shiny::renderTable({
  pars <- current_spec()$pars
  if (is.null(pars)) return(data.frame(message = "No model pars available"))
  record_output_code("model_pars", output_code_snippet("model_pars"))
  pars
}, rownames = FALSE)

fit_comparison_stats <- ctgui_fit_comparison_stats

output$fit_comparison <- shiny::renderTable({
  registry <- fit_registry()
  if (length(registry) == 0L) return(data.frame(message = "No stored fits. Store current fits from the Fit tab."))
  record_output_code("fit_comparison", output_code_snippet("fit_comparison"))
  do.call(rbind, lapply(names(registry), function(name) {
    fit <- registry[[name]]
    model_base <- ctgui_ctsem_fit_model(fit, list())
    stats <- fit_comparison_stats(fit)
    data.frame(
      fit = name,
      class = "ctsem fit",
      manifests = length(model_base$manifestNames %||% character()),
      latents = length(model_base$latentNames %||% character()),
      TDpreds = length(model_base$TDpredNames %||% character()),
      TIpreds = length(model_base$TIpredNames %||% character()),
      logLik = stats$loglik,
      logPosterior = stats$logposterior,
      npars = stats$npars,
      nobs = stats$nobs,
      AIC = stats$aic,
      BIC = stats$bic,
      notes = stats$note,
      row.names = NULL
    )
  }))
}, rownames = FALSE)

shiny::observeEvent(input$env_data, {
  if (is.null(input$env_data) || !nzchar(input$env_data)) {
    return()
  }
  data <- get(input$env_data, envir = .GlobalEnv)
  if (!is.data.frame(data) && !is.matrix(data)) {
    shiny::showNotification("Selected object is no longer a data.frame or matrix", type = "error")
    update_data_choices()
    return()
  }
  current_data(data)
  current_data_name(paste0("R data: ", input$env_data))
  shiny::updateTabsetPanel(session, "data_tabs", selected = "Preview")
}, ignoreInit = TRUE)

shiny::observeEvent(input$csv_file, {
  file_name <- input$csv_file$name %||% ""
  extension <- tolower(tools::file_ext(file_name))
  data <- tryCatch(
    if (identical(extension, "rds")) {
      readRDS(input$csv_file$datapath)
    } else if (identical(extension, "csv")) {
      utils::read.csv(input$csv_file$datapath, stringsAsFactors = FALSE)
    } else {
      stop("Browse accepts .csv or .rds files.", call. = FALSE)
    },
    error = function(e) e
  )
  if (inherits(data, "error")) {
    shiny::showNotification(conditionMessage(data), type = "error")
    return()
  }
  if (!is.data.frame(data) && !is.matrix(data)) {
    shiny::showNotification("The selected file must contain a data.frame or matrix", type = "error")
    return()
  }
  current_data(data)
  current_data_name(paste0(if (identical(extension, "rds")) "RDS: " else "CSV: ", file_name))
  shiny::updateTabsetPanel(session, "data_tabs", selected = "Preview")
})

shiny::observeEvent(input$load_ctsem_test_data, {
  data <- ctsem::ctstantestdat
  current_data(data)
  current_data_name("ctsem::ctstantestdat")
  shiny::updateTabsetPanel(session, "data_tabs", selected = "Preview")
})

shiny::observeEvent(input$generate_data, {
  current_data_name("Generating data...")
  data <- NULL
  shiny::withProgress(message = "Generating data", value = 0.2, {
    data <- tryCatch(
      ctgui_generate_data(
        current_spec(),
        n.subjects = input$gen_subjects,
        Tpoints = input$gen_tpoints,
        burnin = input$gen_burnin,
        dtmean = input$gen_dtmean,
        logdtsd = input$gen_logdtsd,
        wide = FALSE,
        free_defaults = input$gen_free_defaults
      ),
      error = function(e) e
    )
    shiny::incProgress(0.8, detail = "Generation returned")
  })
  if (inherits(data, "error")) {
    current_data_name("No data selected")
    shiny::showNotification(conditionMessage(data), type = "error")
    return()
  }
  current_data(data)
  current_data_name("Generated data")
  shiny::updateTabsetPanel(session, "data_tabs", selected = "Preview")
})

output$data_status <- shiny::renderText({
  data <- current_data()
  if (is.null(data)) return(current_data_name())
  paste0(current_data_name(), " | ", nrow(data), " rows x ", ncol(data), " columns")
})

data_preview_table <- function() {
  ctgui_data_preview(current_data())
}

output$data_preview <- shiny::renderTable(data_preview_table(), rownames = FALSE)

output$data_summary <- shiny::renderTable({
  ctgui_data_summary(current_data())
}, rownames = FALSE)

output$missingness_summary <- shiny::renderTable({
  ctgui_missingness_summary(current_data())
}, rownames = FALSE)

output$within_between_summary <- shiny::renderTable({
  ctgui_within_between_summary(current_data(), current_spec())
}, rownames = FALSE)

output$raw_plot_controls <- shiny::renderUI({
  data <- current_data()
  if (is.null(data)) return(shiny::helpText("Load or generate data before plotting."))
  names <- names(data)
  numeric_names <- names[vapply(data, is.numeric, logical(1L))]
  plot_type <- input$raw_plot_type %||% "Subject trajectories"
  plot_choices <- c("Subject trajectories", "Scatter plot", "Time gaps", "Missingness")
  if (!plot_type %in% plot_choices) plot_type <- "Subject trajectories"
  manifest_selected <- intersect(current_spec()$manifest_names, numeric_names)
  if (!length(manifest_selected) && length(numeric_names)) manifest_selected <- numeric_names[1L]
  colour_choices <- c("(plotted variable)", "(none)", names)
  colour_selected <- "(plotted variable)"
  controls <- list(
    shiny::selectInput("raw_plot_type", "Plot type", choices = plot_choices, selected = plot_type)
  )
  if (identical(plot_type, "Subject trajectories")) {
    controls <- c(controls, list(
      shiny::selectInput("raw_plot_time", "Time column", choices = numeric_names, selected = current_spec()$time),
      shiny::selectizeInput("raw_plot_vars", "Variables to plot", choices = numeric_names,
        selected = manifest_selected, multiple = TRUE),
      shiny::selectInput("raw_plot_subject", "Subject ID column", choices = names, selected = current_spec()$id),
      shiny::selectInput("raw_plot_colour", "Colour by", choices = colour_choices, selected = colour_selected),
      shiny::numericInput("raw_plot_n_subjects", "Subjects to show", value = 12, min = 1, step = 1)
    ))
  } else if (identical(plot_type, "Scatter plot")) {
    controls <- c(controls, list(
      shiny::selectInput("raw_plot_x", "X variable", choices = numeric_names, selected = current_spec()$time),
      shiny::selectizeInput("raw_plot_vars", "Y variables", choices = numeric_names,
        selected = manifest_selected, multiple = TRUE),
      shiny::selectInput("raw_plot_colour", "Colour by", choices = colour_choices, selected = colour_selected)
    ))
  }
  do.call(shiny::div, c(list(class = "control-grid"), controls))
})

plot_colour_values <- function(data, colour_var) {
  if (is.null(colour_var) || identical(colour_var, "(none)") || !colour_var %in% names(data)) {
    return(list(values = rep("#2f6f9f", nrow(data)), legend = NULL, cols = NULL))
  }
  raw <- data[[colour_var]]
  if (is.numeric(raw) && length(unique(stats::na.omit(raw))) > 12L) {
    rng <- range(raw, na.rm = TRUE)
    scaled <- if (diff(rng) == 0) rep(0.5, length(raw)) else (raw - rng[1L]) / diff(rng)
    pal <- grDevices::hcl.colors(100, "Viridis")
    idx <- pmax(1L, pmin(100L, floor(scaled * 99) + 1L))
    return(list(values = pal[idx], legend = NULL, cols = NULL))
  }
  groups <- as.character(raw)
  levels <- unique(groups[!is.na(groups)])
  cols <- stats::setNames(grDevices::hcl.colors(max(1L, length(levels)), "Dark 3"), levels)
  list(values = unname(cols[groups]), legend = levels, cols = cols)
}

plotted_variable_colours <- function(vars) {
  stats::setNames(grDevices::hcl.colors(max(1L, length(vars)), "Dark 3"), vars)
}

output$raw_plot <- shiny::renderPlot({
  on.exit({ plot_cache$raw_plot <- grDevices::recordPlot() }, add = TRUE)
  data <- current_data()
  if (is.null(data) || is.null(input$raw_plot_type)) return(invisible(NULL))
  record_output_code("raw_plot", output_code_snippet("raw_plot"))
  if (identical(input$raw_plot_type, "Missingness")) {
    miss <- vapply(data, function(x) mean(is.na(x)), numeric(1L))
    graphics::barplot(miss, las = 2, ylab = "Proportion missing", col = "grey70")
    return(invisible(NULL))
  }
  if (identical(input$raw_plot_type, "Time gaps")) {
    spec <- current_spec()
    if (!spec$id %in% names(data) || !spec$time %in% names(data)) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "ID/time columns not found")
      return(invisible(NULL))
    }
    gaps <- unlist(lapply(split(data[[spec$time]], data[[spec$id]]), function(x) diff(sort(unique(x)))), use.names = FALSE)
    graphics::hist(gaps, main = "Time gaps", xlab = paste("Difference in", spec$time), col = "grey75", border = "white")
    return(invisible(NULL))
  }
  if (identical(input$raw_plot_type, "Scatter plot")) {
    vars <- input$raw_plot_vars
    vars <- vars[vars %in% names(data)]
    if (is.null(input$raw_plot_x) || !length(vars)) return(invisible(NULL))
    yrange <- range(unlist(data[vars], use.names = FALSE), na.rm = TRUE)
    graphics::plot(data[[input$raw_plot_x]], data[[vars[1L]]], type = "n",
      xlab = input$raw_plot_x, ylab = "Value", ylim = yrange)
    pchs <- seq(16, length.out = length(vars))
    var_cols <- plotted_variable_colours(vars)
    colour <- if (identical(input$raw_plot_colour, "(plotted variable)")) NULL else plot_colour_values(data, input$raw_plot_colour)
    for (i in seq_along(vars)) {
      graphics::points(data[[input$raw_plot_x]], data[[vars[i]]],
        pch = pchs[i], cex = 0.65,
        col = if (is.null(colour)) var_cols[vars[i]] else colour$values)
    }
    graphics::legend("topright", legend = vars, pch = pchs,
      col = if (is.null(colour)) var_cols[vars] else "grey20", bty = "n", cex = 0.8)
    if (!is.null(colour$legend) && length(colour$legend) <= 12L) {
      graphics::legend("bottomright", legend = colour$legend, pch = 16,
        col = colour$cols[colour$legend], bty = "n", cex = 0.75, title = input$raw_plot_colour)
    }
    return(invisible(NULL))
  }
  vars <- input$raw_plot_vars
  vars <- vars[vars %in% names(data)]
  if (is.null(input$raw_plot_time) || !length(vars) || is.null(input$raw_plot_subject)) return(invisible(NULL))
  x <- data[[input$raw_plot_time]]
  yrange <- range(unlist(data[vars], use.names = FALSE), na.rm = TRUE)
  graphics::plot(x, data[[vars[1L]]], type = "n", xlab = input$raw_plot_time, ylab = "Value", ylim = yrange)
  if (input$raw_plot_subject %in% names(data)) {
    groups <- unique(data[[input$raw_plot_subject]])
    groups <- utils::head(groups, input$raw_plot_n_subjects %||% length(groups))
    line_types <- if (identical(input$raw_plot_colour, "(plotted variable)")) rep(1L, length(vars)) else seq_along(vars)
    var_cols <- plotted_variable_colours(vars)
    colour_data <- data[data[[input$raw_plot_subject]] %in% groups, , drop = FALSE]
    colour <- if (identical(input$raw_plot_colour, "(plotted variable)")) NULL else plot_colour_values(colour_data, input$raw_plot_colour)
    subject_cols <- stats::setNames(rep("#2f6f9f", length(groups)), as.character(groups))
    if (!is.null(colour) && !is.null(input$raw_plot_colour) && input$raw_plot_colour %in% names(data)) {
      for (group in groups) {
        group_rows <- colour_data[[input$raw_plot_subject]] %in% group
        if (any(group_rows)) subject_cols[as.character(group)] <- colour$values[which(group_rows)[1L]]
      }
    } else if (is.null(colour)) {
      subject_cols <- stats::setNames(rep("#333333", length(groups)), as.character(groups))
    } else {
      subject_cols <- stats::setNames(grDevices::hcl.colors(length(groups), "Dark 3"), as.character(groups))
    }
    for (i in seq_along(groups)) {
      group <- groups[i]
      rows <- data[[input$raw_plot_subject]] %in% group
      ordered <- order(x[rows])
      for (j in seq_along(vars)) {
        y <- data[[vars[j]]]
        line_col <- if (is.null(colour)) var_cols[vars[j]] else subject_cols[as.character(group)]
        graphics::lines(x[rows][ordered], y[rows][ordered],
          col = grDevices::adjustcolor(line_col, 0.75), lty = line_types[j])
        graphics::points(x[rows], y[rows], pch = 16 + ((j - 1L) %% 6L), cex = 0.5,
          col = grDevices::adjustcolor(line_col, 0.85))
      }
    }
    graphics::legend("topright", legend = vars, lty = line_types, pch = 16 + ((seq_along(vars) - 1L) %% 6L),
      col = if (is.null(colour)) var_cols[vars] else "grey20", bty = "n", cex = 0.8)
    if (!is.null(colour) && length(groups) <= 12L) graphics::legend("bottomright", legend = groups,
      col = subject_cols[as.character(groups)], lty = 1, pch = 16, bty = "n", cex = 0.75,
      title = input$raw_plot_colour %||% input$raw_plot_subject)
  } else {
    var_cols <- plotted_variable_colours(vars)
    colour <- if (identical(input$raw_plot_colour, "(plotted variable)")) NULL else plot_colour_values(data, input$raw_plot_colour)
    for (j in seq_along(vars)) {
      graphics::points(x, data[[vars[j]]], pch = 16 + ((j - 1L) %% 6L), cex = 0.6,
        col = if (is.null(colour)) var_cols[vars[j]] else colour$values)
    }
  }
})

shiny::observeEvent(input$run_fit, {
  if (isTRUE(fit_busy())) return()
  # The browser disables the fit form immediately on click. Register cleanup
  # before every precondition so guarded exits restore the form as well.
  fit_busy(TRUE)
  on.exit({
    fit_busy(FALSE)
    session$sendCustomMessage(
      "ctgui-fit-finished",
      list(beep = isTRUE(input$fit_completion_beep))
    )
  }, add = TRUE)
  data <- current_data()
  if (is.null(data)) {
    shiny::showNotification("Load or generate data before fitting", type = "error")
    return()
  }

  current_fit(NULL)
  clear_uncertainty_state()
  fit_status_value("Fitting...")
  fit_messages("Fitting...")
  fit_warnings("No warnings.")

  result <- NULL
  shiny::withProgress(message = "Fitting ctsem model", value = 0.1, {
    model <- ctgui_to_ctsem_model(current_spec(), silent = TRUE)
    shiny::incProgress(0.2, detail = "Calling ctFit")
    result <- capture_conditions({
      args <- list(
        datalong = data,
        model = model,
        optimize = input$fit_optimize,
        priors = input$fit_priors,
        cores = input$fit_cores,
        plot = FALSE
      )
      extra <- parse_extra_args(input$fit_extra_args)
      if (isTRUE(input$fit_optimize)) {
        supplied_optimcontrol <- extra$optimcontrol
        extra$optimcontrol <- NULL
        args$optimcontrol <- ctgui_uncertainty_merge_optimcontrol(
          uncertainty_optimcontrol(), supplied_optimcontrol)
      }
      args <- c(args, extra[!names(extra) %in% names(args)])
      ctgui_ctsem_call("ctFit", .args = args)
    }, progress_callback = function(lines) {
      fit_messages(paste(lines, collapse = "\n"))
    })
    shiny::incProgress(0.7, detail = "Fit call returned")
  })

  if (inherits(result$value, "error")) {
    fit_status_value("Fit failed.")
    fit_messages(paste(c(result$messages, conditionMessage(result$value)), collapse = "\n"))
    fit_warnings(if (length(result$warnings)) paste(result$warnings, collapse = "\n") else "No warnings.")
    shiny::showNotification(conditionMessage(result$value), type = "error")
    return()
  }

  current_fit(result$value)
  clear_diagnostics()
  fit_status_value("Fit available (ctsem::ctFit result).")
  uncertainty_status_value("Uncertainty was estimated as part of fitting.")
  fit_messages(if (length(result$messages)) paste(result$messages, collapse = "\n") else "Fit complete.")
  fit_warnings(if (length(result$warnings)) paste(result$warnings, collapse = "\n") else "No warnings.")
  record_output_code("fit", output_code_snippet("fit"))
  shiny::showNotification("Fit complete", type = "message")
})

shiny::observeEvent(input$store_fit, {
  fit <- current_fit()
  if (is.null(fit)) {
    shiny::showNotification("No current fit to save", type = "error")
    return()
  }
  shiny::showModal(shiny::modalDialog(
    title = "Store fit for comparison",
    shiny::textInput(
      "store_fit_name", "Fit name",
      value = paste0("fit", length(fit_registry()) + 1L)
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("confirm_store_fit", "Store fit", class = "btn-primary")
    )
  ))
})

shiny::observeEvent(input$confirm_store_fit, {
  fit <- current_fit()
  if (is.null(fit)) return()
  name <- trimws(input$store_fit_name %||% "")
  if (!nzchar(name)) {
    shiny::showNotification("Enter a fit name", type = "error")
    return()
  }
  registry <- fit_registry()
  registry[[name]] <- fit
  fit_registry(registry)
  update_fit_choices(selected = name)
  shiny::removeModal()
  shiny::showNotification(paste("Saved fit", name), type = "message")
})

shiny::observeEvent(input$active_fit_name, {
  registry <- fit_registry()
  selected <- input$active_fit_name
  if (!is.null(selected) && selected %in% names(registry)) {
    current_fit(registry[[selected]])
    clear_uncertainty_state()
    clear_diagnostics()
    fit_status_value(paste("Active fit:", selected))
  }
}, ignoreInit = TRUE)

output$fit_status <- shiny::renderText(fit_status_value())
output$fit_log_inline <- shiny::renderText(fit_messages())
output$fit_warnings_inline <- shiny::renderText(fit_warnings())

run_uncertainty_update <- function() {
  fit <- active_fit()
  eligibility <- ctgui_optim_uncertainty_eligibility(fit)
  if (!isTRUE(eligibility$ok)) {
    uncertainty_status_value(eligibility$message)
    shiny::showNotification(eligibility$message, type = "error")
    return(invisible(NULL))
  }
  if (!isTRUE(ctgui_ctsem_capabilities()$optional[["ctOptimUncertainty"]])) {
    message <- "The loaded ctsem package does not provide ctOptimUncertainty. Load a current ctsem source tree."
    uncertainty_status_value(message)
    shiny::showNotification(message, type = "error")
    return(invisible(NULL))
  }

  uncertainty_status_value("Recomputing optimized-fit uncertainty...")
  uncertainty_messages("Recomputing optimized-fit uncertainty...")
  uncertainty_warnings("No warnings.")
  result <- NULL
  shiny::withProgress(message = "Recomputing optimized-fit uncertainty", value = .1, {
    result <- capture_conditions({
      ctgui_ctsem_call("ctOptimUncertainty", .args = list(
        fit = fit,
        uncertainty = input$fit_uncertainty_method,
        draws = ctgui_uncertainty_default_draws(input$fit_uncertainty_method),
        finishsamples = input$fit_uncertainty_samples,
        cores = input$fit_cores,
        control = uncertainty_control()
      ))
    }, progress_callback = function(lines) {
      uncertainty_messages(paste(lines, collapse = "\n"))
    })
    shiny::incProgress(.9, detail = "Uncertainty call returned")
  })
  if (inherits(result$value, "error")) {
    uncertainty_status_value("Uncertainty recomputation failed.")
    uncertainty_messages(paste(c(result$messages, conditionMessage(result$value)), collapse = "\n"))
    uncertainty_warnings(if (length(result$warnings)) paste(result$warnings, collapse = "\n") else "No warnings.")
    shiny::showNotification(conditionMessage(result$value), type = "error")
    return(invisible(NULL))
  }
  replace_active_fit(result$value)
  clear_diagnostics()
  uncertainty_status_value("Optimized-fit uncertainty updated.")
  uncertainty_messages(if (length(result$messages)) paste(result$messages, collapse = "\n") else "Uncertainty updated.")
  uncertainty_warnings(if (length(result$warnings)) paste(result$warnings, collapse = "\n") else "No warnings.")
  record_output_code("uncertainty", output_code_snippet("uncertainty"))
  shiny::showNotification("Optimized-fit uncertainty updated", type = "message")
  invisible(result$value)
}

shiny::observeEvent(input$run_uncertainty, {
  if (identical(input$fit_uncertainty_method, "fullbootstrap")) {
    shiny::showModal(shiny::modalDialog(
      title = "Confirm full bootstrap",
      paste("This will refit the model", input$fit_uncertainty_samples, "times."),
      easyClose = TRUE,
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("confirm_uncertainty", "Run full bootstrap", class = "btn-danger")
      )
    ))
  } else {
    run_uncertainty_update()
  }
})

shiny::observeEvent(input$confirm_uncertainty, {
  shiny::removeModal()
  run_uncertainty_update()
})

output$uncertainty_status <- shiny::renderText(uncertainty_status_value())
output$uncertainty_log <- shiny::renderText(uncertainty_messages())
output$uncertainty_warnings <- shiny::renderText(uncertainty_warnings())
output$uncertainty_summary <- shiny::renderText({
  fit <- active_fit()
  if (is.null(fit)) return("No fit available.")
  ctgui_uncertainty_summary(fit)
})

shiny::observeEvent(input$generate_from_fit, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("Fit the model before generating from fit", type = "error")
    return()
  }
  diagnostics_status("Generating data from fit...")
  out <- NULL
  shiny::withProgress(message = "Generating from fit", value = 0.2, {
    out <- capture_conditions({
      args <- list(
        fit = fit,
        nsamples = input$fit_gen_samples,
        fullposterior = input$fit_gen_fullposterior,
        cores = generate_from_fit_cores()
      )
      args <- append_extra_args(args, input$fit_gen_extra_args)
      ctgui_ctsem_call("ctGenerateFromFit", .args = args)
    })
    shiny::incProgress(0.8, detail = "Generation returned")
  })
  if (inherits(out$value, "error")) {
    diagnostics_status(conditionMessage(out$value))
    shiny::showNotification(conditionMessage(out$value), type = "error")
    return()
  }
  replace_active_fit(out$value)
  generated_fit(ctgui_ctsem_fit_generated(out$value))
  diagnostics_status(paste(c("Fit-generated data available.", out$messages, out$warnings), collapse = "\n"))
  record_output_code("generate_from_fit", output_code_snippet("generate_from_fit"))
})

shiny::observeEvent(input$run_cov_check, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("Fit the model first", type = "error")
    return()
  }
  if (is.null(ctgui_ctsem_fit_generated(fit))) {
    shiny::showNotification("Run Generate from fit before ctFitCovCheck", type = "error")
    return()
  }
  lags <- cov_check_lags()
  diagnostics_status("Running covariance check...")
  cov_check_log("Running ctFitCovCheck...")
  out <- NULL
  shiny::withProgress(message = "Running ctFitCovCheck", value = 0.2, {
    out <- capture_conditions({
      args <- list(
        fit = fit,
        cor = input$cov_cor,
        plot = FALSE,
        cores = 1
      )
      if (!is.null(lags)) args$lags <- lags
      args <- append_extra_args(args, input$cov_extra_args)
      ctgui_ctsem_call("ctFitCovCheck", .args = args)
    })
    shiny::incProgress(0.8, detail = "Covariance check returned")
  })
  if (inherits(out$value, "error")) {
    cov_check_log(conditionMessage(out$value))
    shiny::showNotification(conditionMessage(out$value), type = "error")
    return()
  }
  cov_check(out$value)
  cov_check_log(paste(c("ctFitCovCheck complete.", out$messages, out$warnings), collapse = "\n"))
  record_output_code("cov_check", output_code_snippet("cov_check"))
})

shiny::observeEvent(input$run_kalman, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("Fit the model first", type = "error")
    return()
  }
  subjects <- parse_optional_expression(input$kalman_subjects)
  timerange <- parse_optional_expression(input$kalman_timerange)
  timestep <- parse_optional_expression(input$kalman_timestep)
  remove_obs <- parse_optional_expression(input$kalman_remove_obs)
  diagnostics_status("Running prediction plots with ctPredict...")
  out <- NULL
  shiny::withProgress(message = "Running ctPredict", value = 0.2, {
    out <- capture_conditions({
      args <- list(fit = fit, plot = FALSE)
      if (!is_omitted_arg(subjects)) args$subjects <- subjects
      if (!is_omitted_arg(timerange)) args$timerange <- timerange
      if (!is_omitted_arg(timestep)) args$timestep <- timestep
      if (!is_omitted_arg(remove_obs)) args$removeObs <- remove_obs
      args <- append_extra_args(args, input$kalman_extra_args)
      ctgui_ctsem_call("ctPredict", .args = args)
    })
    shiny::incProgress(0.8, detail = "ctPredict returned")
  })
  if (inherits(out$value, "error")) {
    diagnostics_status(conditionMessage(out$value))
    shiny::showNotification(conditionMessage(out$value), type = "error")
    return()
  }
  kalman_result(out$value)
  diagnostics_status(paste(c("Prediction plot data available from ctPredict.", out$messages, out$warnings), collapse = "\n"))
  record_output_code("kalman", output_code_snippet("kalman"))
})
shiny::observeEvent(input$run_postpred, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("Fit the model first", type = "error")
    return()
  }
  postpred_log("Running ctPostPredPlots...")
  out <- NULL
  shiny::withProgress(message = "Running ctPostPredPlots", value = 0.2, {
    out <- capture_conditions({
      ctgui_ctsem_call("ctPostPredPlots", fit)
    })
    shiny::incProgress(0.8, detail = "Posterior predictive plots returned")
  })
  if (inherits(out$value, "error")) {
    postpred_log(conditionMessage(out$value))
    shiny::showNotification(conditionMessage(out$value), type = "error")
    return()
  }
  postpred_result(out$value)
  postpred_log(paste(c("ctPostPredPlots complete.", out$messages, out$warnings), collapse = "\n"))
  record_output_code("postpred", output_code_snippet("postpred"))
})

shiny::observeEvent(input$run_residual_acf, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("Fit the model first", type = "error")
    return()
  }
  vars <- parse_text_vector(input$acf_vars, "auto")
  residual_acf_log("Running ctACFresiduals...")
  out <- NULL
  shiny::withProgress(message = "Running residual ACF", value = 0.2, {
    out <- capture_conditions({
      args <- list(fit = fit, varnames = vars, nboot = input$acf_boot, plot = FALSE)
      args <- append_extra_args(args, input$acf_extra_args)
      ctgui_ctsem_call("ctACFresiduals", .args = args)
    }, progress_callback = function(lines) {
      residual_acf_log(paste(lines, collapse = "\n"))
    })
    shiny::incProgress(0.8, detail = "Residual ACF returned")
  })
  if (inherits(out$value, "error")) {
    residual_acf_log(conditionMessage(out$value))
    shiny::showNotification(conditionMessage(out$value), type = "error")
    return()
  }
  residual_acf(out$value)
  residual_acf_log(paste(c("ctACFresiduals complete.", out$messages, out$warnings), collapse = "\n"))
  record_output_code("residual_acf", output_code_snippet("residual_acf"))
})

shiny::observeEvent(input$run_dynamics, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("Fit the model first", type = "error")
    return()
  }
  subjects <- parse_optional_expression(input$dynamic_subjects)
  times <- parse_optional_expression(input$dynamic_times)
  nsamples <- parse_optional_expression(input$dynamic_samples)
  dynamics_log("Running ctDiscretePars...")
  out <- NULL
  shiny::withProgress(message = "Plotting dynamics", value = 0.2, {
    out <- capture_conditions({
      args <- list(fit = fit, observational = input$dynamic_observational, plot = TRUE, cores = 1)
      if (!is_omitted_arg(subjects)) args$subjects <- subjects
      if (!is_omitted_arg(times)) args$times <- times
      if (!is_omitted_arg(nsamples)) args$nsamples <- nsamples
      args <- append_extra_args(args, input$dynamic_extra_args)
      ctgui_ctsem_call("ctDiscretePars", .args = args)
    })
    shiny::incProgress(0.8, detail = "Dynamics plot returned")
  })
  if (inherits(out$value, "error")) {
    dynamics_log(conditionMessage(out$value))
    shiny::showNotification(conditionMessage(out$value), type = "error")
    return()
  }
  dynamics_result(out$value)
  dynamics_log(paste(c("ctDiscretePars complete.", out$messages, out$warnings), collapse = "\n"))
  record_output_code("dynamics", output_code_snippet("dynamics"))
})

shiny::observeEvent(input$run_tipred_effects, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("Fit the model first", type = "error")
    return()
  }
  if (length(current_spec()$tipred_names) == 0L) {
    shiny::showNotification("Add TI predictors before plotting TI effects", type = "error")
    return()
  }
  tipreds <- parse_keyword_or_expression(input$tipred_effects_preds, keywords = "all")
  subject <- parse_optional_expression(input$tipred_effects_subject)
  timestep <- parse_keyword_or_expression(input$tipred_effects_timestep, keywords = "auto")
  tipvalues <- parse_optional_expression(input$tipred_effects_tipvalues)
  tipred_effects_log("Running ctPredictTIP...")
  out <- NULL
  shiny::withProgress(message = "Running ctPredictTIP", value = 0.2, {
    out <- capture_conditions({
      args <- list(sf = fit)
      if (!is_omitted_arg(tipreds)) args$tipreds <- tipreds
      if (!is_omitted_arg(subject)) args$subject <- subject
      if (!is_omitted_arg(timestep)) args$timestep <- timestep
      if (!is_omitted_arg(tipvalues)) args$TIPvalues <- tipvalues
      ctgui_ctsem_call("ctPredictTIP", .args = args)
    })
    shiny::incProgress(0.8, detail = "ctPredictTIP returned")
  })
  if (inherits(out$value, "error")) {
    tipred_effects_log(conditionMessage(out$value))
    shiny::showNotification(conditionMessage(out$value), type = "error")
    return()
  }
  tipred_effects_result(out$value)
  tipred_effects_log(paste(c("ctPredictTIP complete.", out$messages, out$warnings), collapse = "\n"))
  record_output_code("tipred", output_code_snippet("tipred"))
})

output$diagnostics_status <- shiny::renderText(diagnostics_status())

output$generated_fit_summary <- shiny::renderText({
  gen <- generated_fit()
  if (is.null(gen)) return("No fit-generated data available.")
  paste(utils::capture.output(utils::str(gen, max.level = 2)), collapse = "\n")
})

cov_check_plot_list <- shiny::reactive({
  out <- cov_check()
  if (is.null(out)) return(NULL)
  lags <- cov_check_lags()
  plot_args <- c(list(out), if (is.null(lags)) list() else list(maxlag = max(lags)),
    list(cor = input$cov_cor))
  plots <- tryCatch(
    do.call(ctgui_ctsem_call, c(list("ctFitCovCheckPlot"), plot_args)),
    error = function(e) e
  )
  if (inherits(plots, "error")) return(plots)
  ctgui_plot_collection(plots)
})

output$cov_check_plots <- shiny::renderUI({
  plots <- cov_check_plot_list()
  if (is.null(plots)) return(shiny::helpText("Run ctFitCovCheck to show plots."))
  if (inherits(plots, "error")) return(shiny::helpText(conditionMessage(plots)))
  if (length(plots) == 0L) return(shiny::helpText("ctFitCovCheckPlot returned no plots."))
  record_output_code("cov_check", output_code_snippet("cov_check"))
  ids <- paste0("cov_check_plot_", seq_along(plots))
  shiny::tagList(lapply(seq_along(plots), function(i) {
    plot_title <- names(plots)[i]
    if (is.null(plot_title) || !nzchar(plot_title)) plot_title <- paste("Plot", i)
    local({
      plot_index <- i
      output_id <- ids[plot_index]
      register_plot_export(output_id)
      output[[output_id]] <- shiny::renderPlot({
        on.exit({ plot_cache[[output_id]] <- grDevices::recordPlot() }, add = TRUE)
        plot_list <- cov_check_plot_list()
        if (is.null(plot_list) || inherits(plot_list, "error")) return(invisible(NULL))
        ctgui_draw_plot(plot_list[[plot_index]])
      }, height = 430)
    })
    shiny::div(
      class = "matrix-block",
      shiny::tags$h4(plot_title),
      shiny::plotOutput(ids[i], height = 430), ctgui_plot_export_controls(ids[i], 430)
    )
  }))
})

output$cov_check_log <- shiny::renderText(cov_check_log())

output$kalman_plot <- shiny::renderPlot({
  on.exit({ plot_cache$kalman_plot <- grDevices::recordPlot() }, add = TRUE)
  out <- kalman_result()
  if (is.null(out)) return(invisible(NULL))
  record_output_code("kalman", output_code_snippet("kalman"))
  kalmanvec <- parse_text_vector(input$kalman_vec, c("y", "yprior"))
  errorvec <- parse_text_vector(input$kalman_error_vec, "auto")
  plot_result <- try(plot(out, kalmanvec = kalmanvec, errorvec = errorvec), silent = TRUE)
  if (inherits(plot_result, "try-error")) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, as.character(plot_result), cex = 0.8)
  } else if (!is.null(plot_result)) {
    print(plot_result)
  }
})

output$postpred_plots <- shiny::renderUI({
  plots <- postpred_result()
  if (is.null(plots)) return(shiny::helpText("Run ctPostPredPlots to show plots."))
  plots <- ctgui_plot_collection(plots)
  if (length(plots) == 0L) return(shiny::helpText("ctPostPredPlots returned no plots."))
  record_output_code("postpred", output_code_snippet("postpred"))
  ids <- paste0("postpred_plot_", seq_along(plots))
  shiny::tagList(lapply(seq_along(plots), function(i) {
    local({
      plot_index <- i
      output_id <- ids[plot_index]
      register_plot_export(output_id)
      output[[output_id]] <- shiny::renderPlot({
        on.exit({ plot_cache[[output_id]] <- grDevices::recordPlot() }, add = TRUE)
        plot_list <- ctgui_plot_collection(postpred_result())
        if (is.null(plot_list) || length(plot_list) < plot_index) return(invisible(NULL))
        ctgui_draw_plot(plot_list[[plot_index]])
      }, height = 430)
    })
    shiny::div(
      class = "matrix-block",
      shiny::tags$h4(names(plots)[i] %||% paste("Plot", i)),
      shiny::plotOutput(ids[i], height = 430), ctgui_plot_export_controls(ids[i], 430)
    )
  }))
})

output$postpred_log <- shiny::renderText(postpred_log())

output$residual_acf_plot <- shiny::renderPlot({
  on.exit({ plot_cache$residual_acf_plot <- grDevices::recordPlot() }, add = TRUE)
  out <- residual_acf()
  if (is.null(out)) return(invisible(NULL))
  record_output_code("residual_acf", output_code_snippet("residual_acf"))
  plot_result <- try(ctgui_ctsem_call("plotctACF", out), silent = TRUE)
  if (inherits(plot_result, "try-error")) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, as.character(plot_result), cex = 0.8)
  } else {
    print(plot_result)
  }
})

output$residual_acf_log <- shiny::renderText(residual_acf_log())

output$dynamics_plot <- shiny::renderPlot({
  on.exit({ plot_cache$dynamics_plot <- grDevices::recordPlot() }, add = TRUE)
  out <- dynamics_result()
  if (is.null(out)) return(invisible(NULL))
  record_output_code("dynamics", output_code_snippet("dynamics"))
  ylim <- parse_optional_expression(input$dynamic_ylim)
  if (!is_omitted_arg(ylim) && inherits(out, "ggplot")) {
    out <- out + getExportedValue("ggplot2", "coord_cartesian")(ylim = ylim)
  }
  print(out)
})

output$dynamics_log <- shiny::renderText(dynamics_log())

output$tipred_effects_plots <- shiny::renderUI({
  plots <- tipred_effects_result()
  if (is.null(plots)) return(shiny::helpText("Run ctPredictTIP to show trajectory and dynamics plots."))
  record_output_code("tipred", output_code_snippet("tipred"))
  group_ui <- function(group_name, group_plots) {
    flat <- ctgui_plot_collection(group_plots)
    if (!length(flat)) return(shiny::helpText(paste("No", tolower(group_name), "plots returned.")))
    ids <- paste0("tipred_", tolower(group_name), "_plot_", seq_along(flat))
    shiny::tagList(lapply(seq_along(flat), function(i) {
      local({
        plot_index <- i
        output_id <- ids[i]
        register_plot_export(output_id)
        output[[output_id]] <- shiny::renderPlot({
          on.exit({ plot_cache[[output_id]] <- grDevices::recordPlot() }, add = TRUE)
          current <- tipred_effects_result()
          current_group <- if (is.list(current) && group_name %in% names(current)) current[[group_name]] else current
          current_flat <- ctgui_plot_collection(current_group)
          if (length(current_flat) < plot_index) return(invisible(NULL))
          ctgui_draw_plot(current_flat[[plot_index]])
        }, height = 430)
      })
      shiny::div(
        class = "matrix-block",
        shiny::tags$h4(names(flat)[i] %||% paste(group_name, "plot", i)),
        shiny::plotOutput(ids[i], height = 430), ctgui_plot_export_controls(ids[i], 430)
      )
    }))
  }
  process <- if (is.list(plots) && "Process" %in% names(plots)) plots$Process else NULL
  dynamics <- if (is.list(plots) && "Dynamics" %in% names(plots)) plots$Dynamics else NULL
  shiny::tabsetPanel(
    type = "pills",
    shiny::tabPanel("Process", group_ui("Process", process %||% plots)),
    shiny::tabPanel("Dynamics", group_ui("Dynamics", dynamics))
  )
})

output$tipred_effects_log <- shiny::renderText(tipred_effects_log())

capture_output_wide <- function(expr, width = 240L) {
  old <- options(width = width)
  on.exit(options(old), add = TRUE)
  utils::capture.output(expr)
}

fit_summary_text <- function() {
  fit <- active_fit()
  if (is.null(fit)) return("No fit available.")
  record_output_code("summary", output_code_snippet("summary"))
  paste(capture_output_wide(summary(fit)), collapse = "\n")
}

fit_summary_matrices_text <- function() {
  fit <- active_fit()
  if (is.null(fit)) return("No fit available.")
  if (!isTRUE(ctgui_ctsem_capabilities()$optional[["ctSummaryMatrices"]])) {
    return("ctsem::ctSummaryMatrices() is not available in the loaded ctsem version.")
  }
  result <- tryCatch(
    capture_output_wide(ctgui_ctsem_call("ctSummaryMatrices", fit)),
    error = function(e) paste("ctSummaryMatrices failed:", conditionMessage(e))
  )
  record_output_code("summary_matrices", output_code_snippet("summary_matrices"))
  paste(result, collapse = "\n")
}

output$fit_summary <- shiny::renderText(fit_summary_text())
output$fit_summary_matrices <- shiny::renderText(fit_summary_matrices_text())
  }
  }
