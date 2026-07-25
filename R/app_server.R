# Application server composition ---------------------------------------------

ctgui_app_server <- function(initial_spec, help_catalog) {
  function(input, output, session) {
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
visual_drafts <- shiny::reactiveVal(list())
visual_status_value <- shiny::reactiveVal("Visual editor is loading the current matrices.")
spec_inputs_suspended <- shiny::reactiveVal(FALSE)

sync_matrix_inputs_from_spec <- function(spec) {
  spec_inputs_suspended(TRUE)
  for (matrix_name in ctgui_matrix_names(spec)) {
    mat <- spec$matrices[[matrix_name]]
    if (!is.matrix(mat)) next
    for (row in seq_len(nrow(mat))) for (col in seq_len(ncol(mat))) {
      shiny::updateTextInput(session, matrix_cell_id(matrix_name, row, col),
        value = as.character(mat[row, col]))
    }
  }
  shiny::updateSelectInput(session, "model_visual_matrix", choices = ctgui_matrix_names(spec),
    selected = input$model_visual_matrix %||% "DRIFT")
  shiny::updateTextInput(session, "latent_names", value = paste(spec$latent_names, collapse = ", "))
  shiny::updateTextInput(session, "manifest_names", value = paste(spec$manifest_names, collapse = ", "))
  shiny::updateTextInput(session, "tdpred_names", value = paste(spec$tdpred_names, collapse = ", "))
  shiny::updateTextInput(session, "tipred_names", value = paste(spec$tipred_names, collapse = ", "))
  shiny::updateCheckboxInput(session, "tipredDefault", value = isTRUE(spec$tipredDefault))
  session$onFlushed(function() {
    spec_inputs_suspended(FALSE)
  }, once = TRUE)
}

visual_graph_for_view <- function(view = input$visual_view %||% "state_space") {
  drafts <- visual_drafts()
  graph <- drafts[[view]]
  if (is.null(graph)) graph <- ctgui_visual_graph(current_spec(), view)
  graph
}
visual_data_columns <- function() {
  data <- shiny::isolate(current_data())
  if (is.null(data)) character() else names(data)
}
send_visual_graph <- function(view = input$visual_view %||% "state_space") {
  session$sendCustomMessage("ctgui-visual-load", list(
    id = "visual_spec_canvas", graph = visual_graph_for_view(view),
    data_columns = visual_data_columns()
  ))
}
reset_visual_drafts <- function(message = "Reloaded visual editor from matrices.") {
  spec <- ctgui_visual_ensure(current_spec())
  commit_current_spec(spec, reason = "visual_reset", refresh_visual = FALSE,
    refresh_widgets = FALSE)
  drafts <- list(
    state_space = ctgui_visual_graph(spec, "state_space"),
    initial_state = ctgui_visual_graph(spec, "initial_state"),
    tipred_effects = ctgui_visual_graph(spec, "tipred_effects")
  )
  visual_drafts(drafts); visual_status_value(message)
  send_visual_graph()
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
    spec_data = "Map active data columns to ctsem roles; these selectors update the editable name fields.",
    spec_options = "Core ctModel options control the continuous/discrete time model, manifest variable types, and default TI predictor behavior.",
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
output$explain_spec_options <- shiny::renderUI(explain_ui("spec_options"))
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
    if (!is.null(help$topic)) shiny::tags$pre(style = "white-space: pre-wrap;", text) else text,
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

input_spec_fields <- function() {
  ctgui_spec_fields(
    c(shiny::reactiveValuesToList(input), list(Tpoints = NULL)),
    current_spec()
  )
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
    grepl("(?i)(hessian|iter|iteration|elapsed|optim|optimization|chain|warmup|sampling|draws|gradient|stepsize|objective|progress|\\d+\\s*/\\s*\\d+)", text, perl = TRUE)
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

data_frame_names <- function() {
  objects <- ls(envir = .GlobalEnv)
  objects[vapply(objects, function(name) {
    is.data.frame(get(name, envir = .GlobalEnv))
  }, logical(1L))]
}

update_data_choices <- function() {
  choices <- data_frame_names()
  shiny::updateSelectInput(session, "env_data", choices = choices, selected = choices[1L])
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
    draws = input$fit_uncertainty_draws,
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
is_ctsem_fit <- function(x) is.list(x) && (!is.null(x$stanfit) || !is.null(x$model) || !is.null(x$ctstanmodel))

shiny::observeEvent(input$fit_uncertainty_method, {
  method <- input$fit_uncertainty_method %||% "hessian"
  choices <- ctgui_uncertainty_draw_choices(method)
  selected <- input$fit_uncertainty_draws
  if (is.null(selected) || !selected %in% unname(choices)) selected <- unname(choices)[1L]
  shiny::updateSelectInput(session, "fit_uncertainty_draws", choices = choices, selected = selected)
}, ignoreInit = FALSE)

output$uncertainty_eligibility <- shiny::renderUI({
  eligibility <- ctgui_optim_uncertainty_eligibility(active_fit())
  class <- if (isTRUE(eligibility$ok)) "help-note" else "warning-note"
  shiny::tags$p(class = class, eligibility$message)
})

output$download_model_rds <- shiny::downloadHandler(
  filename = function() "ctsem-model.rds",
  content = function(file) saveRDS(ctgui_to_ctsem_model(current_spec(), silent = TRUE), file)
)
output$download_project_rds <- shiny::downloadHandler(
  filename = function() "ctsemgui-project.rds",
  content = function(file) saveRDS(ctgui_visual_ensure(current_spec()), file)
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
  if (is.null(fit)) shiny::showNotification("No fit is available to return", type = "error") else assign_r_object(fit, input$fit_object_name, "fit object")
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
  if (inherits(fit, "error") || !is_ctsem_fit(fit)) {
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

matrix_id_part <- ctgui_matrix_id_part
matrix_cell_id <- ctgui_matrix_cell_id

shiny::observe(update_data_choices())

output$data_spec_controls <- shiny::renderUI({
  data <- current_data()
  if (is.null(data)) return(shiny::helpText("Load or generate data to select model roles from columns."))
  roles <- ctgui_data_role_selection(data, current_spec())
  shiny::div(
    class = "control-grid",
    shiny::selectizeInput("data_manifest_names", "Manifest variables from active data",
      choices = roles$choices, selected = roles$manifest_names, multiple = TRUE),
    shiny::selectizeInput("data_tdpred_names", "Time dependent predictors from active data",
      choices = roles$choices, selected = roles$tdpred_names, multiple = TRUE),
    shiny::selectizeInput("data_tipred_names", "Time independent predictors from active data",
      choices = roles$choices, selected = roles$tipred_names, multiple = TRUE),
    shiny::selectInput("data_id", "ID column from active data",
      choices = roles$choices, selected = roles$id),
    shiny::selectInput("data_time", "Time column from active data",
      choices = roles$choices, selected = roles$time)
  )
})

output$tipred_network <- shiny::renderUI({
  spec <- current_spec()
  if (!length(spec$tipred_names)) return(shiny::helpText("Add time-independent predictors to show their subject-level correlations and moderated parameters."))
  shiny::div(class = "control-band",
    shiny::tags$h4("Time-independent predictor network"),
    shiny::tags$p(class = "help-note", "Undirected edges are Pearson correlations between the first available value for each subject. Directed arrows show which free model parameters are moderated by each predictor."),
    shiny::plotOutput("tipred_network_plot", height = 320),
    shiny::textOutput("tipred_network_status")
  )
})

tipred_subject_data <- ctgui_tipred_subject_data

output$tipred_network_status <- shiny::renderText({
  info <- tipred_subject_data(current_data(), current_spec())
  if (is.null(info)) return("Load data with the selected ID column and TI predictors to calculate correlations.")
  notes <- character()
  if (length(info$missing)) notes <- c(notes, paste("Missing from data:", paste(info$missing, collapse = ", ")))
  if (length(info$varying)) notes <- c(notes, paste("These selected TI predictors vary within subject; correlations use each subject's first row:", paste(info$varying, collapse = ", ")))
  if (!length(notes)) "" else paste(notes, collapse = " ")
})

output$tipred_network_plot <- shiny::renderPlot({
  spec <- current_spec(); info <- tipred_subject_data(current_data(), spec)
  metadata <- spec$parameter_metadata
  predictors <- spec$tipred_names
  targets <- character()
  if (!is.null(metadata) && nrow(metadata)) for (tipred in predictors) {
    field <- paste0(tipred, "_effect")
    if (field %in% names(metadata)) targets <- c(targets, paste(metadata$matrix[metadata[[field]]], metadata$row[metadata[[field]]], metadata$col[metadata[[field]]], sep = ":"))
  }
  nodes <- unique(c(predictors, targets))
  graphics::plot.new()
  if (!length(nodes)) { graphics::text(.5, .5, "No TI predictor effects are selected yet."); return(invisible(NULL)) }
  theta <- seq(0, 2*pi, length.out = length(nodes) + 1L)[-length(nodes) - 1L]
  coords <- data.frame(name = nodes, x = cos(theta), y = sin(theta))
  graphics::plot.window(xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), asp = 1)
  if (!is.null(info) && length(predictors) > 1L) {
    numeric <- predictors[vapply(predictors, function(x) x %in% names(info$values) && is.numeric(info$values[[x]]), logical(1L))]
    if (length(numeric) > 1L) {
      cor <- stats::cor(info$values[numeric], use = "pairwise.complete.obs")
      for (r in 2:length(numeric)) for (c in seq_len(r - 1L)) if (is.finite(cor[r, c])) {
        a <- coords[coords$name == numeric[r], ]; b <- coords[coords$name == numeric[c], ]
        graphics::segments(a$x, a$y, b$x, b$y, col = grDevices::adjustcolor("#7c3aed", .45), lwd = 1 + 2 * abs(cor[r,c]))
        graphics::text((a$x+b$x)/2, (a$y+b$y)/2, sprintf("r = %.2f", cor[r,c]), cex = .65, col = "#5b21b6", pos = 3)
      }
    }
  }
  if (!is.null(metadata) && nrow(metadata)) for (tipred in predictors) {
    field <- paste0(tipred, "_effect"); if (!field %in% names(metadata)) next
    for (i in which(metadata[[field]])) {
      target <- paste(metadata$matrix[i], metadata$row[i], metadata$col[i], sep = ":")
      a <- coords[coords$name == tipred, ]; b <- coords[coords$name == target, ]; dx <- b$x-a$x; dy <- b$y-a$y; d <- sqrt(dx^2+dy^2)
      if (d > 0) graphics::arrows(a$x+.16*dx/d, a$y+.16*dy/d, b$x-.16*dx/d, b$y-.16*dy/d, length=.08, col="#0f766e", lwd=1.6)
    }
  }
  is_pred <- nodes %in% predictors
  graphics::points(coords$x, coords$y, pch = 21, bg = ifelse(is_pred, "#dbeafe", "#dcfce7"), cex = 3.2)
  graphics::text(coords$x, coords$y, nodes, cex = .72)
})

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

shiny::observeEvent(input$data_manifest_names, {
  shiny::updateTextInput(session, "manifest_names", value = paste(input$data_manifest_names, collapse = ", "))
}, ignoreInit = TRUE)

shiny::observeEvent(input$data_tdpred_names, {
  shiny::updateTextInput(session, "tdpred_names", value = paste(input$data_tdpred_names, collapse = ", "))
}, ignoreInit = TRUE)

shiny::observeEvent(input$data_tipred_names, {
  shiny::updateTextInput(session, "tipred_names", value = paste(input$data_tipred_names, collapse = ", "))
}, ignoreInit = TRUE)

shiny::observeEvent(input$data_id, {
  shiny::updateTextInput(session, "id", value = input$data_id)
}, ignoreInit = TRUE)

shiny::observeEvent(input$data_time, {
  shiny::updateTextInput(session, "time", value = input$data_time)
}, ignoreInit = TRUE)

rebuild_spec_if_needed <- function() {
  if (isTRUE(spec_inputs_suspended())) return(invisible(FALSE))
  fields <- input_spec_fields()
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

visual_selected_edge <- shiny::reactive({
  selected <- input$visual_spec_canvas_selection
  if (is.null(selected) || is.null(selected$id)) return(NULL)
  graph <- visual_graph_for_view(selected$view %||% input$visual_view %||% "state_space")
  if (isTRUE(selected$parameter_node)) {
    spec <- current_spec(); matrix <- as.character(selected$matrix)
    mat <- spec$matrices[[matrix]]
    if (is.null(mat)) return(NULL)
    row <- match(as.character(selected$row), rownames(mat)); col <- match(as.character(selected$col), colnames(mat))
    if (is.na(row) || is.na(col)) return(NULL)
    style <- ctgui_visual_edge_style(spec, matrix, rownames(mat)[row], colnames(mat)[col], mat[row, col])
    return(c(list(id = NULL, matrix = matrix, row = rownames(mat)[row], col = colnames(mat)[col],
      source = NULL, target = NULL, directed = TRUE, edge_kind = "parameter"), style))
  }
  edges <- graph$edges %||% list()
  index <- which(vapply(edges, function(edge) identical(as.character(edge$id), as.character(selected$id)), logical(1L)))
  if (!length(index)) return(NULL)
  edge <- edges[[index[1L]]]
  if (isTRUE(edge$visual_only) || identical(edge$edge_kind, "noise_input")) return(NULL)
  edge
})

output$visual_path_inspector <- shiny::renderUI({
  edge <- visual_selected_edge()
  if (is.null(edge)) return(shiny::div(class = "matrix-cell-inspector", shiny::tags$p("Select a path to edit its parameter settings.")))
  shiny::div(class = "matrix-cell-inspector visual-path-inspector",
    shiny::tags$h5(paste(edge$matrix, "[", edge$row, ",", edge$col, "]", sep = "")),
    shiny::div(class = "control-grid",
      shiny::textInput("visual_path_value", "Value / parameter label / expression", value = if (identical(edge$value, "__free__")) ctgui_auto_label(edge$matrix, edge$row, edge$col) else edge$value %||% ""),
      shiny::checkboxInput("visual_path_random", "RandomEffects", value = isTRUE(edge$indvarying)),
      shiny::textInput("visual_path_transform", "Transform", value = ctgui_display_transform(edge$transform)),
      shiny::numericInput("visual_path_sdscale", "RandomEffectsScale", value = suppressWarnings(as.numeric(edge$sdscale %||% 1)), step = 0.1),
      if (length(current_spec()$tipred_names)) shiny::selectizeInput("visual_path_tipreds", "Time Independent Predictors", choices = current_spec()$tipred_names, selected = edge$tipred_effects %||% character(), multiple = TRUE),
      shiny::textInput("visual_path_extra_pars", "PARS (free parameters in expression)", value = edge$extra_pars %||% "", placeholder = "e.g. nonlinear_a, nonlinear_b")
    )
  )
})

output$visual_pars_details <- shiny::renderUI({
  edge <- visual_selected_edge()
  if (is.null(edge)) return(NULL)
  used <- ctgui_split_pars(edge$extra_pars)
  if (!length(used)) return(NULL)
  spec <- current_spec(); pars <- spec$matrices[["PARS"]]
  if (is.null(pars)) return(NULL)
  rows <- which(as.character(pars[, 1L, drop = TRUE]) %in% used)
  if (!length(rows)) return(NULL)
  cards <- lapply(rows, function(row) {
    meta <- matrix_metadata(spec, "PARS", rownames(pars)[row], colnames(pars)[1L])
    if (is.null(meta)) return(NULL)
    tipreds <- spec$tipred_names[vapply(spec$tipred_names, function(tipred) {
      field <- paste0(tipred, "_effect")
      field %in% names(meta) && isTRUE(meta[[field]][1L])
    }, logical(1L))]
    scale <- suppressWarnings(as.numeric(meta$sdscale[1L])); if (is.na(scale)) scale <- 1
    prefix <- paste0("visual_pars_", row)
    shiny::div(class = "matrix-cell-inspector visual-path-inspector",
      shiny::tags$h5(as.character(pars[row, 1L])),
      shiny::div(class = "control-grid",
        shiny::checkboxInput(paste0(prefix, "_indvarying"), "RandomEffects", value = isTRUE(meta$indvarying[1L])),
        shiny::textInput(paste0(prefix, "_transform"), "Transform", value = ctgui_display_transform(meta$transform[1L])),
        shiny::numericInput(paste0(prefix, "_sdscale"), "RandomEffectsScale", value = scale, step = 0.1),
        if (length(spec$tipred_names)) shiny::selectizeInput(paste0(prefix, "_tipreds"), "Time Independent Predictors", choices = spec$tipred_names, selected = tipreds, multiple = TRUE)
      )
    )
  })
  shiny::div(class = "matrix-pars-details", shiny::tags$h5("PARS parameter metadata"), cards)
})

shiny::observeEvent(input$visual_spec_canvas_graph, {
  graph <- input$visual_spec_canvas_graph
  if (is.null(graph$view)) return()
  if (isTRUE(graph$layout_only)) {
    updated <- ctgui_visual_save_layout(current_spec(), graph)
    drafts <- visual_drafts(); drafts[[graph$view]] <- graph; visual_drafts(drafts)
    commit_current_spec(updated, reason = "visual_layout", refresh_visual = FALSE)
    visual_status_value("Visual layout saved.")
    return()
  }
  updated <- tryCatch(ctgui_visual_apply_graph(current_spec(), graph), error = function(e) e)
  if (inherits(updated, "error")) {
    shiny::showNotification(conditionMessage(updated), type = "error")
    return()
  }
  drafts <- list(
    state_space = ctgui_visual_graph(updated, "state_space"),
    initial_state = ctgui_visual_graph(updated, "initial_state"),
    tipred_effects = ctgui_visual_graph(updated, "tipred_effects")
  )
  visual_drafts(drafts)
  commit_current_spec(updated, reason = "visual_graph")
  sync_matrix_inputs_from_spec(updated)
  fit_status_value("Visual model changed. Refit when ready.")
  matrix_status("Visual change applied to model matrices.")
  visual_status_value("Visual changes are applied directly to the current model.")
  send_visual_graph(graph$view)
}, ignoreInit = TRUE)

shiny::observeEvent(input$visual_view, {
  if (!length(visual_drafts())) reset_visual_drafts() else send_visual_graph(input$visual_view)
}, ignoreInit = TRUE)
# The visual tab is a view of the current matrix specification. Rebuild its
# drafts when it is opened so edits made in Specification or Matrices cannot
# leave the browser showing the startup/default graph.
shiny::observeEvent(input$model_tabs, {
  if (identical(input$model_tabs, "Visual Specification")) {
    reset_visual_drafts("Loaded visual editor from the current model specification.")
  }
}, ignoreInit = TRUE)
shiny::observeEvent(current_data(), {
  if (length(visual_drafts())) send_visual_graph(input$visual_view %||% "state_space")
}, ignoreInit = TRUE)

update_visual_path <- function() {
  selected <- input$visual_spec_canvas_selection; edge <- visual_selected_edge()
  if (is.null(selected) || is.null(edge)) return(invisible(NULL))
  committed_input <- function(id) {
    payload <- input$visual_path_commit
    if (is.list(payload) && id %in% names(payload)) payload[[id]] else input[[id]]
  }
  previous_pars <- current_spec()$matrices[["PARS"]]
  previous_par_values <- if (is.null(previous_pars)) character() else {
    as.character(previous_pars[, 1L, drop = TRUE])
  }
  drafts <- visual_drafts(); view <- selected$view %||% input$visual_view %||% "state_space"; graph <- drafts[[view]]
  index <- which(vapply(graph$edges, function(item) identical(as.character(item$id), as.character(edge$id)), logical(1L)))[1L]
  value <- trimws(committed_input("visual_path_value") %||% "")
  if (!nzchar(value)) value <- "__free__"
  item <- if (is.na(index)) edge else graph$edges[[index]]
  item$value <- value; item$label <- if (identical(value, "__free__")) "free" else value
  item$fixed <- !is.na(suppressWarnings(as.numeric(strsplit(value, "|", fixed = TRUE)[[1L]][1L])))
  item$custom <- nzchar(committed_input("visual_path_extra_pars") %||% "")
  item$indvarying <- isTRUE(committed_input("visual_path_random"))
  item$transform <- committed_input("visual_path_transform") %||% ""
  item$sdscale <- committed_input("visual_path_sdscale") %||% 1
  item$tipred_effects <- committed_input("visual_path_tipreds") %||% character()
  item$extra_pars <- committed_input("visual_path_extra_pars") %||% ""
  if (!is.na(index)) { graph$edges[[index]] <- item; drafts[[view]] <- graph; visual_drafts(drafts) }
  updated <- tryCatch(ctgui_visual_update_edge(current_spec(), item), error = function(e) e)
  if (inherits(updated, "error")) { shiny::showNotification(conditionMessage(updated), type = "error"); return(invisible(NULL)) }
  pars <- updated$matrices[["PARS"]]
  used <- ctgui_split_pars(item$extra_pars)
  if (!is.null(pars) && length(used)) for (row in which(as.character(pars[, 1L, drop = TRUE]) %in% used)) {
    parameter_name <- as.character(pars[row, 1L])
    if (!(parameter_name %in% previous_par_values)) next
    prefix <- paste0("visual_pars_", row)
    updated <- ctgui_set_parameter_metadata(updated, "PARS", rownames(pars)[row], colnames(pars)[1L],
      transform = committed_input(paste0(prefix, "_transform")) %||% NULL,
      indvarying = committed_input(paste0(prefix, "_indvarying")) %||% NULL,
      sdscale = committed_input(paste0(prefix, "_sdscale")) %||% NULL,
      tipred_effects = committed_input(paste0(prefix, "_tipreds")) %||% NULL)
  }
  drafts <- list(
    state_space = ctgui_visual_graph(updated, "state_space"),
    initial_state = ctgui_visual_graph(updated, "initial_state"),
    tipred_effects = ctgui_visual_graph(updated, "tipred_effects")
  )
  visual_drafts(drafts)
  commit_current_spec(updated, reason = "visual_path")
  sync_matrix_inputs_from_spec(updated)
  fit_status_value("Visual model changed. Refit when ready.")
  matrix_status("Visual path updated in model matrices.")
  visual_status_value("Visual changes are applied directly to the current model.")
  # T0MEANS RandomEffects determines which T0VAR paths are visible in the
  # initial-state graph. Reload that graph rather than patching only the
  # edited mean path, so restored variance/correlation edges appear.
  if (identical(item$matrix, "T0MEANS")) {
    send_visual_graph("initial_state")
  } else {
    session$sendCustomMessage("ctgui-visual-update-edge", list(id = "visual_spec_canvas", edge = item))
  }
  invisible(NULL)
}
shiny::observeEvent(input$visual_path_commit, update_visual_path(), ignoreInit = TRUE)
output$visual_status <- shiny::renderText(visual_status_value())
# onFlushed() itself is not a reactive consumer.  Isolate the initial
# reactive reads while the browser-side editor is being populated.
session$onFlushed(function() shiny::isolate(reset_visual_drafts()), once = TRUE)

matrix_group_names <- function(spec, group = input$matrix_group) {
  ctgui_matrix_group_names(spec, group)
}
matrix_note <- ctgui_matrix_note
fixed_matrix_value <- ctgui_fixed_matrix_value
indvarying_t0means <- ctgui_indvarying_t0means
matrix_metadata <- ctgui_matrix_metadata_row
matrix_meta_id <- ctgui_matrix_meta_id
active_matrix_cell <- ctgui_active_matrix_cell

matrix_network_id <- function(matrix_name, field) {
  paste0("matrix_network_", matrix_id_part(matrix_name), "_", field)
}

matrix_network_controls <- function(matrix_name) {
  shiny::div(
    class = "matrix-network-controls",
    shiny::sliderInput(matrix_network_id(matrix_name, "spread"), "Node spacing", min = 0.4, max = 4.5, value = 1.5, step = 0.1),
    shiny::sliderInput(matrix_network_id(matrix_name, "general_scale"), "General scale", min = 0.35, max = 3.5, value = 1, step = 0.05),
    shiny::sliderInput(matrix_network_id(matrix_name, "text_scale"), "Text scale", min = 0.35, max = 3.5, value = 1, step = 0.05),
    shiny::sliderInput(matrix_network_id(matrix_name, "label_offset"), "Path-label offset", min = 0, max = 1.2, value = 0.18, step = 0.02),
    shiny::checkboxInput(matrix_network_id(matrix_name, "self_effects"), "Show self-effect arrows", value = TRUE)
  )
}

matrix_network_options <- function(matrix_name) {
  list(
    spread = input[[matrix_network_id(matrix_name, "spread")]] %||% 1.5,
    general_scale = input[[matrix_network_id(matrix_name, "general_scale")]] %||% 1,
    text_scale = input[[matrix_network_id(matrix_name, "text_scale")]] %||% 1,
    label_offset = input[[matrix_network_id(matrix_name, "label_offset")]] %||% 0.18,
    self_effects = isTRUE(input[[matrix_network_id(matrix_name, "self_effects")]] %||% TRUE)
  )
}

network_matrix_cell_active <- function(value) {
  value <- trimws(as.character(value %||% ""))
  if (!nzchar(value) || grepl("\\|\\|\\s*FALSE\\s*$", value, ignore.case = TRUE)) return(FALSE)
  numeric_value <- suppressWarnings(as.numeric(value))
  if (!is.na(numeric_value)) return(!isTRUE(all.equal(numeric_value, 0)))
  TRUE
}

draw_matrix_network <- function(spec, matrix_name, options = matrix_network_options(matrix_name)) {
  mat <- ctgui_matrix(spec, matrix_name)
  covariance <- matrix_name %in% c("T0VAR", "DIFFUSION", "MANIFESTVAR", "TDPREDVAR")
  edges <- data.frame(from = character(), to = character(), label = character(), loop = logical(), fixed = logical(), stringsAsFactors = FALSE)
  for (r in seq_len(nrow(mat))) for (c in seq_len(ncol(mat))) {
    value <- as.character(mat[r, c])
    if (!network_matrix_cell_active(value) || (covariance && c > r)) next
    from <- if (matrix_name == "LAMBDA") colnames(mat)[c] else if (matrix_name %in% c("CINT", "T0MEANS", "MANIFESTMEANS", "TDPREDMEANS")) "constant" else colnames(mat)[c]
    to <- rownames(mat)[r]
    loop <- identical(from, to)
    if (loop && !isTRUE(options$self_effects)) next
    fixed <- !is.na(suppressWarnings(as.numeric(trimws(value))))
    edges <- rbind(edges, data.frame(from = from, to = to,
      label = strsplit(value, "|", fixed = TRUE)[[1L]][1L], loop = loop, fixed = fixed, stringsAsFactors = FALSE))
  }
  if (!nrow(edges)) {
    return(ggplot2::ggplot() + ggplot2::annotate("text", x = 0, y = 0, label = "No active paths in this matrix") + ggplot2::theme_void())
  }
  pair_key <- vapply(seq_len(nrow(edges)), function(i) paste(sort(c(edges$from[i], edges$to[i])), collapse = "\r"), character(1L))
  edges$label_pos <- 0.5
  for (key in unique(pair_key[!edges$loop])) {
    index <- which(pair_key == key & !edges$loop)
    if (length(index) > 1L) edges$label_pos[index] <- seq(0.32, 0.68, length.out = length(index))
  }
  nodes <- unique(c(edges$from, edges$to))
  sources <- unique(edges$from)
  targets <- unique(edges$to)
  bipartite <- length(setdiff(sources, targets)) > 0L && length(setdiff(targets, sources)) > 0L
  if (bipartite) {
    coords <- rbind(
      data.frame(name = sources, x = rep(-options$spread, length(sources)), y = seq(1, -1, length.out = length(sources)) * options$spread),
      data.frame(name = targets, x = rep(options$spread, length(targets)), y = seq(1, -1, length.out = length(targets)) * options$spread)
    )
    coords <- coords[!duplicated(coords$name), , drop = FALSE]
  } else {
    radius <- options$spread * (1 + 0.06 * max(0, length(nodes) - 4L))
    theta <- seq(pi / 2, pi / 2 + 2 * pi, length.out = length(nodes) + 1L)[-length(nodes) - 1L]
    coords <- data.frame(name = nodes, x = radius * cos(theta), y = radius * sin(theta))
  }
  coords$node_type <- if (identical(matrix_name, "LAMBDA")) {
    ifelse(coords$name %in% colnames(mat), "latent", "observed")
  } else "process"
  arrow_end <- if (!covariance) grid::arrow(length = grid::unit(5 * options$general_scale, "mm"), type = "closed") else NULL
  arrow_loop <- if (!covariance) grid::arrow(length = grid::unit(5 * options$general_scale, "mm"), type = "closed", ends = "both") else NULL
  path_strength <- 0.5 + options$label_offset * 2.5
  label_size <- max(2.5, 3.5 * options$text_scale)
  # These values are edge attributes because ggraph evaluates edge
  # aesthetics from the igraph object, not the local `edges` data frame.
  edges$label_size <- label_size
  edges$edge_colour <- ifelse(edges$fixed, "#94a3b8", if (covariance) "#7c3aed" else "#2563eb")
  edges$edge_linetype <- ifelse(edges$fixed, "dashed", "solid")
  graph <- igraph::graph_from_data_frame(
    edges[, c("from", "to", "label", "loop", "fixed", "label_pos", "label_size", "edge_colour", "edge_linetype")],
    directed = !covariance, vertices = coords
  )
  plot <- ggraph::ggraph(graph, layout = "manual", x = coords$x, y = coords$y) +
    ggraph::geom_edge_fan(
      ggplot2::aes(label = label, label_pos = label_pos, label_size = label_size, edge_colour = edge_colour, edge_linetype = edge_linetype, filter = !loop), arrow = arrow_end,
      strength = path_strength, label_dodge = grid::unit(options$label_offset, "cm"),
      edge_width = 0.8 * options$general_scale
    ) +
    ggraph::geom_edge_loop(
      ggplot2::aes(label = label, label_size = label_size, edge_colour = edge_colour, edge_linetype = edge_linetype, filter = loop), arrow = arrow_loop,
      label_dodge = grid::unit(options$label_offset, "cm"),
      edge_width = 0.8 * options$general_scale
    ) +
    ggraph::scale_edge_colour_identity() +
    ggraph::scale_edge_linetype_identity()
  if (identical(matrix_name, "LAMBDA")) {
    plot <- plot +
      ggraph::geom_node_point(ggplot2::aes(filter = node_type == "latent"), shape = 21, size = 8 * options$general_scale, fill = "#dbeafe", colour = "#1d4ed8", stroke = 0.8) +
      ggraph::geom_node_text(ggplot2::aes(label = name, filter = node_type == "latent"), size = label_size) +
      ggraph::geom_node_label(ggplot2::aes(label = name, filter = node_type == "observed"), size = label_size, fill = "#f8fafc", colour = "#334155", label.size = 0.5)
  } else {
    plot <- plot +
      ggraph::geom_node_point(shape = 21, size = 8 * options$general_scale, fill = "#dbeafe", colour = "#1d4ed8", stroke = 0.8) +
      ggraph::geom_node_text(ggplot2::aes(label = name), size = label_size)
  }
  plot + ggplot2::coord_fixed(clip = "off") + ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(16, 32, 16, 32)) +
    ggplot2::ggtitle(paste(matrix_name, if (covariance) "covariances" else "paths"))
}

matrix_metadata_badges <- function(spec, matrix_name, row_name, col_name) {
  meta <- matrix_metadata(spec, matrix_name, row_name, col_name)
  if (is.null(meta)) return(NULL)
  tipreds <- spec$tipred_names[vapply(spec$tipred_names, function(tipred) {
    field <- paste0(tipred, "_effect")
    field %in% names(meta) && isTRUE(meta[[field]][1L])
  }, logical(1L))]
  scale <- suppressWarnings(as.numeric(meta$sdscale[1L]))
  if (is.na(scale)) scale <- if (isTRUE(meta$sdscale[1L])) 1 else 0
  shiny::div(
    class = "matrix-cell-metadata",
    shiny::div("Transform: ", ctgui_display_transform(meta$transform[1L])),
    shiny::div("RandomEffects: ", if (isTRUE(meta$indvarying[1L])) "TRUE" else "FALSE"),
    shiny::div("RandomEffectsScale: ", format(scale, trim = TRUE)),
    shiny::div("Time Independent Predictors: ", if (length(tipreds)) paste(tipreds, collapse = ", ") else "-")
  )
}

selected_matrix_cell <- shiny::reactive({
  selected <- input$matrix_selected_cell
  if (is.null(selected) || !is.list(selected)) return(NULL)
  row <- suppressWarnings(as.integer(selected$row))
  col <- suppressWarnings(as.integer(selected$col))
  if (is.null(selected$matrix) || is.na(row) || is.na(col) || row < 1L || col < 1L) return(NULL)
  list(matrix = as.character(selected$matrix), row = row, col = col)
})

matrix_cell_inspector <- function(matrix_name) {
  selected <- selected_matrix_cell()
  if (is.null(selected) || !identical(selected$matrix, matrix_name)) {
    return(shiny::div(class = "matrix-cell-inspector", shiny::helpText("Select a free matrix cell to view and edit its parameter settings.")))
  }
  spec <- current_spec()
  mat <- ctgui_matrix(spec, matrix_name)
  if (selected$row > nrow(mat) || selected$col > ncol(mat)) return(NULL)
  value <- input[[matrix_cell_id(matrix_name, selected$row, selected$col)]] %||% mat[selected$row, selected$col]
  if (!active_matrix_cell(value)) {
    return(shiny::div(class = "matrix-cell-inspector", shiny::helpText("Fixed numeric cells do not have parameter settings. Enter a free parameter label to edit RandomEffects, Transform, RandomEffectsScale, or Time Independent Predictors.")))
  }
  row_name <- rownames(mat)[selected$row]
  col_name <- colnames(mat)[selected$col]
  meta <- matrix_metadata(spec, matrix_name, row_name, col_name)
  if (is.null(meta)) return(shiny::div(class = "matrix-cell-inspector", shiny::helpText("Apply the free parameter label before editing its settings.")))
  tipreds <- spec$tipred_names[vapply(spec$tipred_names, function(tipred) {
    field <- paste0(tipred, "_effect")
    field %in% names(meta) && isTRUE(meta[[field]][1L])
  }, logical(1L))]
  random_effects_scale <- suppressWarnings(as.numeric(meta$sdscale[1L]))
  if (is.na(random_effects_scale)) random_effects_scale <- 1
  shiny::div(
    class = "matrix-cell-inspector",
    shiny::tags$h5("Cell Specific Details"),
    shiny::tags$p(class = "help-note", paste0("Selected cell: ", matrix_name, " [", row_name, ", ", col_name, "]. Settings apply to the free parameter in this cell and are saved automatically.")),
    shiny::div(class = "control-grid",
      shiny::checkboxInput(matrix_meta_id(matrix_name, selected$row, selected$col, "indvarying"), arg_label("RandomEffects", "help_matrix_random_effects"), value = isTRUE(meta$indvarying[1L])),
      shiny::textInput(matrix_meta_id(matrix_name, selected$row, selected$col, "transform"), arg_label("Transform", "help_matrix_transform"), value = ctgui_display_transform(meta$transform[1L])),
      shiny::numericInput(matrix_meta_id(matrix_name, selected$row, selected$col, "sdscale"), arg_label("RandomEffectsScale", "help_matrix_random_effects_scale"), value = random_effects_scale, step = 0.1),
      if (length(spec$tipred_names)) shiny::selectizeInput(matrix_meta_id(matrix_name, selected$row, selected$col, "tipreds"), arg_label("Time Independent Predictors", "help_matrix_time_independent_predictors"), choices = spec$tipred_names, selected = tipreds, multiple = TRUE),
      shiny::textInput(matrix_meta_id(matrix_name, selected$row, selected$col, "extra_pars"), "PARS (free parameters in expression)", value = meta$extra_pars[1L] %||% "", placeholder = "e.g. nonlinear_a, nonlinear_b")
    )
  )
}

matrix_pars_details <- function(matrix_name) {
  spec <- current_spec()
  metadata <- spec$parameter_metadata
  if (is.null(metadata) || !nrow(metadata) || !"extra_pars" %in% names(metadata)) return(NULL)
  parent <- metadata[metadata$matrix == matrix_name, , drop = FALSE]
  used <- ctgui_split_pars(parent$extra_pars)
  if (!length(used)) return(NULL)
  pars <- spec$matrices[["PARS"]]
  if (is.null(pars)) return(NULL)
  rows <- which(as.character(pars[, 1L, drop = TRUE]) %in% used)
  if (!length(rows)) return(NULL)
  cards <- lapply(rows, function(row) {
    meta <- matrix_metadata(spec, "PARS", rownames(pars)[row], colnames(pars)[1L])
    if (is.null(meta)) return(shiny::helpText(paste("Parameter", pars[row, 1L], "will be available after the next model update.")))
    tipreds <- spec$tipred_names[vapply(spec$tipred_names, function(tipred) {
      field <- paste0(tipred, "_effect")
      field %in% names(meta) && isTRUE(meta[[field]][1L])
    }, logical(1L))]
    scale <- suppressWarnings(as.numeric(meta$sdscale[1L])); if (is.na(scale)) scale <- 1
    shiny::div(class = "matrix-cell-inspector",
      shiny::tags$h5(as.character(pars[row, 1L])),
      shiny::div(class = "control-grid",
        shiny::checkboxInput(matrix_meta_id("PARS", row, 1L, "indvarying"), "RandomEffects", value = isTRUE(meta$indvarying[1L])),
        shiny::textInput(matrix_meta_id("PARS", row, 1L, "transform"), "Transform", value = ctgui_display_transform(meta$transform[1L])),
        shiny::numericInput(matrix_meta_id("PARS", row, 1L, "sdscale"), "RandomEffectsScale", value = scale, step = 0.1),
        if (length(spec$tipred_names)) shiny::selectizeInput(matrix_meta_id("PARS", row, 1L, "tipreds"), "Time Independent Predictors", choices = spec$tipred_names, selected = tipreds, multiple = TRUE)
      )
    )
  })
  shiny::div(class = "matrix-pars-details",
    shiny::tags$h5(paste("PARS parameters used by", matrix_name)),
    shiny::tags$p(class = "matrix-note", "These additional free parameters are included once in the combined PARS vector. Their settings are saved automatically."),
    cards
  )
}

matrix_editor_block <- function(spec, matrix_name) {
  mat <- ctgui_matrix(spec, matrix_name)
  inactive_names <- if (identical(matrix_name, "T0VAR")) indvarying_t0means(spec) else character()
  header <- shiny::tags$tr(shiny::tags$th(""), lapply(colnames(mat), shiny::tags$th))
  rows <- lapply(seq_len(nrow(mat)), function(row) {
    shiny::tags$tr(
      shiny::tags$th(rownames(mat)[row]),
      lapply(seq_len(ncol(mat)), function(col) {
        inactive <- identical(matrix_name, "T0VAR") &&
          (rownames(mat)[row] %in% inactive_names || colnames(mat)[col] %in% inactive_names)
        shiny::tags$td(
          class = if (inactive) "matrix-inactive" else NULL,
          shiny::div(
            class = "matrix-cell",
            `data-matrix` = matrix_name,
            `data-row` = row,
            `data-col` = col,
            shiny::textInput(
              matrix_cell_id(matrix_name, row, col),
              label = NULL,
              value = as.character(mat[row, col]),
              width = "100%"
            ) |> shiny::tagAppendAttributes(disabled = if (inactive) "disabled" else NULL),
            if (!inactive && active_matrix_cell(mat[row, col])) matrix_metadata_badges(spec, matrix_name, rownames(mat)[row], colnames(mat)[col])
          )
        )
      })
    )
  })
  shiny::div(
    class = "matrix-block",
    shiny::tags$h4(matrix_name),
    shiny::tags$p(class = "matrix-note", matrix_note(matrix_name)),
    if (identical(matrix_name, "T0VAR") && length(inactive_names)) {
      shiny::tags$p(class = "matrix-note",
        paste("Inactive cells involve", paste(inactive_names, collapse = ", "),
          "because those T0MEANS entries are individual-varying. ctsem fixes the corresponding T0VAR rows and columns."))
    },
    shiny::div(class = "matrix-editor", shiny::tags$table(class = "table table-condensed", shiny::tags$thead(header), shiny::tags$tbody(rows))),
    shiny::uiOutput(paste0("matrix_cell_inspector_", matrix_id_part(matrix_name))),
    shiny::uiOutput(paste0("matrix_pars_details_", matrix_id_part(matrix_name))),
    shiny::div(class = "matrix-network",
      shiny::tags$h5("Network diagram"),
      shiny::div(class = "matrix-network-layout",
        matrix_network_controls(matrix_name),
        shiny::div(class = "matrix-network-plot", shiny::plotOutput(paste0("matrix_network_", matrix_id_part(matrix_name)), height = 380))
      ),
      plot_export_controls(paste0("matrix_network_", matrix_id_part(matrix_name)), 380)
    )
  )
}

matrix_section_tabs <- function(group) {
  spec <- current_spec()
  names <- matrix_group_names(spec, group)
  if (length(names) == 0L) {
    if (identical(group, "Predictors")) {
      return(shiny::div(class = "matrix-block", shiny::helpText("Add time-dependent predictors in Specification to edit predictor matrices.")))
    }
    return(shiny::div(class = "matrix-block", shiny::helpText("No matrices are available for this model section.")))
  }
  panels <- lapply(names, function(matrix_name) {
    shiny::tabPanel(matrix_name, value = matrix_name, matrix_editor_block(spec, matrix_name))
  })
  do.call(
    shiny::tabsetPanel,
    c(list(id = paste0("matrix_", tolower(group), "_tabs"), type = "pills"), panels)
  )
}

output$matrix_dynamics_editor <- shiny::renderUI(matrix_section_tabs("Dynamics"))
output$matrix_measurement_editor <- shiny::renderUI(matrix_section_tabs("Measurement"))
output$matrix_initial_editor <- shiny::renderUI(matrix_section_tabs("Initial"))
output$matrix_predictor_editor <- shiny::renderUI(matrix_section_tabs("Predictors"))

shiny::observe({
  spec <- current_spec()
  for (matrix_name in setdiff(ctgui_matrix_names(spec), "PARS")) local({
    name <- matrix_name
    output_id <- paste0("matrix_network_", matrix_id_part(name))
    inspector_id <- paste0("matrix_cell_inspector_", matrix_id_part(name))
    pars_details_id <- paste0("matrix_pars_details_", matrix_id_part(name))
    register_plot_export(output_id)
    output[[inspector_id]] <- shiny::renderUI(matrix_cell_inspector(name))
    output[[pars_details_id]] <- shiny::renderUI(matrix_pars_details(name))
    output[[output_id]] <- shiny::renderPlot({
      on.exit({ plot_cache[[output_id]] <- grDevices::recordPlot() }, add = TRUE)
      print(draw_matrix_network(current_spec(), name, matrix_network_options(name)))
    }, height = 380)
  })
})

pars_vector <- ctgui_pars_vector

output$matrix_pars_editor <- shiny::renderUI({
  spec <- current_spec()
  shiny::div(
    class = "matrix-block pars-editor",
    shiny::tags$h4("PARS"),
    shiny::tags$p(class = "matrix-note",
      "Extra parameter vector for nonlinear or custom expressions. Enter one fixed value or free label per line."),
    shiny::textAreaInput("pars_vector", "PARS vector", value = paste(pars_vector(spec), collapse = "\n"),
      width = "100%", height = "180px")
  )
})

output$matrix_quick_editor <- shiny::renderUI({
  spec <- current_spec()
  matrix_names <- setdiff(ctgui_matrix_names(spec), "PARS")
  if (!length(matrix_names)) return(NULL)
  matrix_name <- input$quick_matrix %||% matrix_names[1L]
  if (!matrix_name %in% matrix_names) matrix_name <- matrix_names[1L]
  mat <- ctgui_matrix(spec, matrix_name)
  row_choices <- rownames(mat) %||% as.character(seq_len(nrow(mat)))
  col_choices <- colnames(mat) %||% as.character(seq_len(ncol(mat)))
  shiny::div(
    class = "control-grid",
    shiny::selectInput("quick_matrix", "Structured edit matrix", choices = matrix_names, selected = matrix_name),
    shiny::selectInput("quick_row", "Row", choices = row_choices),
    shiny::selectInput("quick_col", "Column", choices = col_choices),
    shiny::selectInput("quick_mode", "Cell mode", choices = c(
      "Fixed numeric" = "fixed",
      "Free parameter" = "free",
      "Free + random effects" = "random",
      "Free + TI moderation" = "ti",
      "Custom expression" = "custom"
    )),
    shiny::textInput("quick_label", "Label / expression", value = ""),
    shiny::textInput("quick_value", "Fixed value / TI predictors", value = "0"),
    shiny::actionButton("quick_apply", "Apply structured edit")
  )
})

compose_quick_value <- function() {
  mode <- input$quick_mode %||% "fixed"
  label <- trimws(input$quick_label %||% "")
  value <- trimws(input$quick_value %||% "")
  if (identical(mode, "fixed")) return(list(value = suppressWarnings(as.numeric(value)), label = NULL))
  if (!nzchar(label)) label <- ctgui_auto_label(input$quick_matrix, input$quick_row, input$quick_col)
  if (identical(mode, "free")) return(list(value = NULL, label = label))
  if (identical(mode, "random")) return(list(value = NULL, label = paste0(label, "||TRUE")))
  if (identical(mode, "ti")) {
    moderators <- value
    if (!nzchar(moderators)) moderators <- paste(current_spec()$tipred_names, collapse = ",")
    return(list(value = NULL, label = paste0(label, "||TRUE||", moderators)))
  }
  list(value = NULL, label = label)
}

shiny::observeEvent(input$quick_apply, {
  spec <- current_spec()
  if (is.null(input$quick_matrix) || !input$quick_matrix %in% ctgui_matrix_names(spec)) return()
  new_value <- compose_quick_value()
  if (!is.null(new_value$value) && (length(new_value$value) != 1L || is.na(new_value$value))) {
    shiny::showNotification("Fixed value must be numeric", type = "error")
    return()
  }
  updated <- tryCatch(
    ctgui_set_matrix_value(spec, input$quick_matrix, input$quick_row, input$quick_col,
      value = new_value$value, label = new_value$label),
    error = function(e) e
  )
  if (inherits(updated, "error")) {
    shiny::showNotification(conditionMessage(updated), type = "error")
    return()
  }
  commit_current_spec(updated, reason = "matrix_metadata")
  matrix_status(paste("Structured edit applied to", input$quick_matrix))
})

parse_pars_vector <- ctgui_parse_pars_vector
set_spec_matrix <- ctgui_set_spec_matrix

matrix_input_values <- shiny::reactive({
  spec <- current_spec()
  if (identical(input$matrix_group, "PARS")) {
    if (is.null(input$pars_vector)) return(NULL)
    return(list(PARS = parse_pars_vector(input$pars_vector)))
  }
  names <- matrix_group_names(spec)
  if (length(names) == 0L) return(list())
  out <- list()
  for (matrix_name in names) {
    mat <- ctgui_matrix(spec, matrix_name)
    values <- matrix("", nrow = nrow(mat), ncol = ncol(mat), dimnames = dimnames(mat))
    for (row in seq_len(nrow(mat))) {
      for (col in seq_len(ncol(mat))) {
        value <- input[[matrix_cell_id(matrix_name, row, col)]]
        if (is.null(value)) value <- mat[row, col]
        values[row, col] <- if (!nzchar(value)) "0" else value
      }
    }
    out[[matrix_name]] <- values
  }
  out
})

apply_matrix_metadata_inputs <- function(spec) {
  names <- unique(c(matrix_group_names(spec), if (!is.null(spec$matrices[["PARS"]])) "PARS" else character()))
  for (matrix_name in names) {
    mat <- spec$matrices[[matrix_name]]
    if (is.null(mat)) next
    for (row in seq_len(nrow(mat))) for (col in seq_len(ncol(mat))) {
      meta <- matrix_metadata(spec, matrix_name, rownames(mat)[row], colnames(mat)[col])
      if (is.null(meta)) next
      transform <- input[[matrix_meta_id(matrix_name, row, col, "transform")]]
      indvarying <- input[[matrix_meta_id(matrix_name, row, col, "indvarying")]]
      sdscale <- input[[matrix_meta_id(matrix_name, row, col, "sdscale")]]
      tipreds <- input[[matrix_meta_id(matrix_name, row, col, "tipreds")]]
      extra_pars <- input[[matrix_meta_id(matrix_name, row, col, "extra_pars")]]
      if (is.null(transform)) transform <- meta$transform
      if (is.null(indvarying)) indvarying <- meta$indvarying
      if (is.null(sdscale)) sdscale <- meta$sdscale
      if (is.null(tipreds)) {
        tipreds <- spec$tipred_names[vapply(spec$tipred_names, function(tipred) isTRUE(meta[[paste0(tipred, "_effect")]]), logical(1L))]
      }
      if (is.null(extra_pars)) extra_pars <- meta$extra_pars %||% ""
      spec <- ctgui_set_parameter_metadata(spec, matrix_name, rownames(mat)[row], colnames(mat)[col],
        transform = transform, indvarying = indvarying, sdscale = sdscale,
        tipred_effects = tipreds, extra_pars = extra_pars, sync = FALSE)
    }
  }
  spec
}

apply_current_matrix <- function(show_notification = FALSE) {
  spec <- current_spec()
  matrix_values <- matrix_input_values()
  if (is.null(matrix_values)) return(invisible(FALSE))
  updated <- spec
  changed <- character()
  for (matrix_name in names(matrix_values)) {
    value <- matrix_values[[matrix_name]]
    old <- updated$matrices[[matrix_name]]
    if (is.null(value) && is.null(old)) next
    if (!is.null(value) && !is.null(old) && identical(as.character(old), as.character(value))) next
    next_spec <- tryCatch(set_spec_matrix(updated, matrix_name, value), error = function(e) e)
    if (inherits(next_spec, "error")) {
      matrix_status(conditionMessage(next_spec))
      if (show_notification) shiny::showNotification(conditionMessage(next_spec), type = "error")
      return(invisible(FALSE))
    }
    updated <- next_spec
    changed <- c(changed, matrix_name)
  }
  metadata_updated <- tryCatch(apply_matrix_metadata_inputs(updated), error = function(e) e)
  if (inherits(metadata_updated, "error")) {
    matrix_status(conditionMessage(metadata_updated))
    return(invisible(FALSE))
  }
  if (!identical(updated$parameter_metadata, metadata_updated$parameter_metadata)) changed <- unique(c(changed, "parameter options"))
  updated <- metadata_updated
  if (length(changed) == 0L) return(invisible(FALSE))
  commit_current_spec(updated, reason = "matrix_edit")
  # Matrix metadata can change which T0VAR cells ctsem uses. Rebuild all
  # visual drafts so toggling T0MEANS RandomEffects restores or suppresses
  # the corresponding initial-state variance and correlation paths.
  visual_drafts(list(
    state_space = ctgui_visual_graph(updated, "state_space"),
    initial_state = ctgui_visual_graph(updated, "initial_state"),
    tipred_effects = ctgui_visual_graph(updated, "tipred_effects")
  ))
  send_visual_graph(input$visual_view %||% "state_space")
  fit_status_value("Model changed. Refit when ready.")
  matrix_status(paste("Updated", paste(changed, collapse = ", "), "at", format(Sys.time(), "%H:%M:%S")))
  if (show_notification) shiny::showNotification("Matrix edits applied", type = "message")
  invisible(TRUE)
}

shiny::observeEvent(input$matrix_commit_nonce, {
  apply_current_matrix(show_notification = FALSE)
})

shiny::observeEvent(input$matrix_metadata_commit, {
  apply_current_matrix(show_notification = FALSE)
})

shiny::observeEvent(input$tab_commit_nonce, {
  apply_current_matrix(show_notification = FALSE)
  rebuild_spec_if_needed()
})

output$matrix_status <- shiny::renderText(matrix_status())

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
  fit$model %||% fit$ctstanmodel %||% fit$modelbase %||% fit$ctstanmodelbase
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
output$validation_table <- shiny::renderTable(ctgui_validate(current_spec()), rownames = FALSE)
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
  record_output_code("model_visual", model_visual_code_snippet())
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

code_value <- function(x) paste(ctgui_deparse(x), collapse = "\n")

optional_arg_line <- function(name, text, comma = TRUE) {
  if (is.null(text) || !nzchar(trimws(text))) return(character())
  paste0("  ", name, " = ", code_value(parse_optional_expression(text)), if (comma) "," else "")
}

set_output_code_snippet <- function(key, lines) {
  snippets <- output_code_snippets()
  snippets[[key]] <- paste(lines, collapse = "\n")
  output_code_snippets(snippets)
}

record_output_code <- function(key, lines) {
  shiny::isolate(set_output_code_snippet(key, lines))
}

base_output_code <- shiny::reactive({
  data <- current_data()
  data_name <- current_data_name()
  lines <- c(
    "# Model specification",
    "# Explanations are shown in the GUI.",
    ctgui_export_code(current_spec()),
    "",
    "# Data"
  )

  if (identical(data_name, "Generated data")) {
    lines <- c(lines,
      "# In the app this was generated from the active ctsemgui specification.",
      "# Recreate from the GUI spec object if available, or use ctsem::ctGenerate()",
      "# with the exported `model` object after replacing free labels with numeric values.",
      "# data <- ctsemgui::ctgui_generate_data(",
      "#   spec,",
      paste0("#   n.subjects = ", code_value(input$gen_subjects), ","),
      paste0("#   Tpoints = ", code_value(input$gen_tpoints), ","),
      paste0("#   burnin = ", code_value(input$gen_burnin), ","),
      paste0("#   dtmean = ", code_value(input$gen_dtmean), ","),
      paste0("#   logdtsd = ", code_value(input$gen_logdtsd), ","),
      paste0("#   free_defaults = ", code_value(input$gen_free_defaults)),
      "# )"
    )
  } else if (startsWith(data_name, "R data.frame: ")) {
    object_name <- sub("^R data\\.frame: ", "", data_name)
    lines <- c(lines, paste0("data <- ", object_name))
  } else if (startsWith(data_name, "CSV: ")) {
    file_name <- sub("^CSV: ", "", data_name)
    lines <- c(lines,
      paste0("# Imported CSV was named ", code_value(file_name), " in the Shiny session."),
      "data <- utils::read.csv(\"path/to/data.csv\", stringsAsFactors = FALSE)"
    )
  } else if (!is.null(data)) {
    lines <- c(lines, "# Active data exists in the Shiny session; assign it here as `data` before fitting.")
  } else {
    lines <- c(lines, "# No data is currently active.")
  }
  lines
})

uncertainty_control_code <- function(indent = "    ") {
  control <- uncertainty_control()
  control <- control[!vapply(control, is.null, logical(1))]
  lines <- vapply(names(control), function(name) {
    paste0(indent, name, " = ", code_value(control[[name]]))
  }, character(1))
  if (length(lines) > 1L) lines[-length(lines)] <- paste0(lines[-length(lines)], ",")
  c(paste0(indent, "uncertaintyControl = list("),
    paste0(indent, "  ", lines), paste0(indent, ")"))
}

fit_optimcontrol_code <- function(indent = "  ") {
  lines <- c(
    paste0(indent, "uncertainty = ", code_value(input$fit_uncertainty_method), ","),
    paste0(indent, "uncertaintyDraws = ", code_value(input$fit_uncertainty_draws), ","),
    paste0(indent, "finishsamples = ", code_value(input$fit_uncertainty_samples), ","),
    uncertainty_control_code(paste0(indent, "  "))
  )
  c(paste0(indent, "optimcontrol = list("), lines, paste0(indent, ")"))
}

fit_code_snippet <- function() {
  args <- c(
    "# Fit",
    "fit <- ctsem::ctFit(",
    "  datalong = data,",
    "  model = model,",
    paste0("  optimize = ", code_value(input$fit_optimize), ","),
    paste0("  priors = ", code_value(input$fit_priors), ","),
    paste0("  cores = ", code_value(input$fit_cores), ",")
  )
  if (isTRUE(input$fit_optimize)) {
    optim_lines <- fit_optimcontrol_code("  ")
    optim_lines[length(optim_lines)] <- paste0(optim_lines[length(optim_lines)], ",")
    args <- c(args, optim_lines)
  }
  args <- c(args, "  plot = FALSE",
    ")",
    "",
    "# Output",
    "summary(fit)",
    "ctsem::ctSummaryMatrices(fit)"
  )
  args
}

uncertainty_code_snippet <- function() {
  control <- uncertainty_control_code("  ")
  c(
    "# Recompute optimized-fit uncertainty",
    "fit <- ctsem::ctOptimUncertainty(",
    "  fit = fit,",
    paste0("  uncertainty = ", code_value(input$fit_uncertainty_method), ","),
    paste0("  draws = ", code_value(input$fit_uncertainty_draws), ","),
    paste0("  finishsamples = ", code_value(input$fit_uncertainty_samples), ","),
    paste0("  cores = ", code_value(input$fit_cores), ","),
    control,
    ")"
  )
}

summary_code_snippet <- function() {
  c(
    "# Fit summary",
    "summary(fit)"
  )
}

summary_matrices_code_snippet <- function() {
  c(
    "# Fit summary matrices",
    "ctsem::ctSummaryMatrices(fit)"
  )
}

fit_pars_code_snippet <- function() {
  c(
    "# Model parameter table",
    "model$pars"
  )
}

fit_comparison_code_snippet <- function() {
  c(
    "# Fit comparison",
    "# Save candidate fits in a named list, then use ctsem summaries to compare them.",
    "fits <- list(fit1 = fit)",
    "lapply(fits, summary)"
  )
}

raw_plot_code_snippet <- function() {
  c(
    "# Data visualisation",
    paste0("# Plot type: ", code_value(input$raw_plot_type %||% "Subject trajectories")),
    paste0("# Time column: ", code_value(input$raw_plot_time %||% current_spec()$time)),
    paste0("# Plotted variables: ", code_value(input$raw_plot_vars %||% current_spec()$manifest_names[1L])),
    paste0("# Subject ID column: ", code_value(input$raw_plot_subject %||% current_spec()$id)),
    paste0("# Colour variable: ", code_value(input$raw_plot_colour %||% "(plotted variable)")),
    "# Use the Data > Visuals settings above to reproduce the current GUI plot."
  )
}

model_visual_code_snippet <- function() {
  c(
    "# Model visualisation",
    paste0("# Visual type: ", code_value(input$model_visual_type %||% "Temporal dynamics graph")),
    "# The GUI graph is extracted from the active model matrices.",
    "# Temporal dynamics use DRIFT, system-noise paths use DIFFUSION, and measurement paths use LAMBDA."
  )
}

generate_from_fit_code_snippet <- function() {
  c(
    "# Generate from fit for diagnostics",
    "fit <- ctsem::ctGenerateFromFit(",
    "  fit = fit,",
    paste0("  nsamples = ", code_value(input$fit_gen_samples), ","),
    paste0("  fullposterior = ", code_value(input$fit_gen_fullposterior), ","),
    paste0("  cores = ", code_value(input$fit_gen_cores)),
    ")"
  )
}

cov_check_code_snippet <- function() {
  c(
    "# Covariance check",
    paste0("cov_lags <- ", input$cov_lags %||% "0:3"),
    "cov_check <- ctsem::ctFitCovCheck(",
    "  fit = fit,",
    paste0("  cor = ", code_value(input$cov_cor), ","),
    "  lags = cov_lags,",
    "  plot = FALSE,",
    "  cores = 1",
    ")",
    "cov_check_plots <- ctsem::ctFitCovCheckPlot(",
    "  cov_check,",
    "  maxlag = max(cov_lags),",
    paste0("  cor = ", code_value(input$cov_cor)),
    ")",
    "lapply(cov_check_plots, print)"
  )
}

kalman_code_snippet <- function() {
  kalman_optional_lines <- c(
    optional_arg_line("subjects", input$kalman_subjects),
    optional_arg_line("timerange", input$kalman_timerange),
    optional_arg_line("timestep", input$kalman_timestep),
    optional_arg_line("removeObs", input$kalman_remove_obs)
  )
  c(
    "# Prediction plots using ctPredict",
    "prediction <- ctsem::ctPredict(",
    "  fit = fit,",
    kalman_optional_lines,
    "  plot = FALSE",
    ")",
    "plot(",
    "  prediction,",
    paste0("  kalmanvec = ", code_value(parse_text_vector(input$kalman_vec, c("y", "yprior"))), ","),
    paste0("  errorvec = ", code_value(parse_text_vector(input$kalman_error_vec, "auto"))),
    ")"
  )
}

postpred_code_snippet <- function() {
  c(
    "# Posterior predictive checks",
    "postpred_plots <- ctsem::ctPostPredPlots(fit)",
    "lapply(postpred_plots, print)"
  )
}

residual_acf_code_snippet <- function() {
  c(
    "# Residual autocorrelation",
    "residual_acf <- ctsem::ctACFresiduals(",
    "  fit,",
    paste0("  varnames = ", code_value(parse_text_vector(input$acf_vars, "auto")), ","),
    paste0("  nboot = ", code_value(input$acf_boot), ","),
    "  plot = FALSE",
    ")",
    "print(ctsem::plotctACF(residual_acf))"
  )
}

dynamics_code_snippet <- function() {
  dynamic_optional_lines <- c(
    optional_arg_line("subjects", input$dynamic_subjects),
    optional_arg_line("times", input$dynamic_times),
    optional_arg_line("nsamples", input$dynamic_samples)
  )
  c(
    "# Dynamics / impulse-response style plot",
    "dynamics <- ctsem::ctDiscretePars(",
    "  fit = fit,",
    dynamic_optional_lines,
    paste0("  observational = ", code_value(input$dynamic_observational), ","),
    "  plot = TRUE,",
    "  cores = 1",
    ")",
    if (!is_omitted_arg(parse_optional_expression(input$dynamic_ylim))) {
      paste0("# Apply y limits post hoc when the returned plot object supports it: ylim = ",
        code_value(parse_optional_expression(input$dynamic_ylim)))
    },
    "print(dynamics)"
  )
}

tipred_code_snippet <- function() {
  tipreds <- parse_keyword_or_expression(input$tipred_effects_preds, keywords = "all")
  subject <- parse_optional_expression(input$tipred_effects_subject)
  timestep <- parse_keyword_or_expression(input$tipred_effects_timestep, keywords = "auto")
  tipvalues <- parse_optional_expression(input$tipred_effects_tipvalues)
  args <- c(
    "  sf = fit",
    if (!is_omitted_arg(tipreds)) paste0("  tipreds = ", code_value(tipreds)),
    if (!is_omitted_arg(subject)) paste0("  subject = ", code_value(subject)),
    if (!is_omitted_arg(timestep)) paste0("  timestep = ", code_value(timestep)),
    if (!is_omitted_arg(tipvalues)) paste0("  TIPvalues = ", code_value(tipvalues))
  )
  if (length(args) > 1L) args[-length(args)] <- paste0(args[-length(args)], ",")
  c(
    "# TI predictor effects",
    "tip_plots <- ctsem::ctPredictTIP(",
    args,
    ")",
    "# tip_plots$Process and tip_plots$Dynamics contain the returned plot groups."
  )
}

workflow_code <- shiny::reactive({
  snippets <- output_code_snippets()
  lines <- base_output_code()
  if (length(snippets)) {
    lines <- c(lines, "", "# Actions run in the GUI")
    for (key in names(snippets)) lines <- c(lines, "", snippets[[key]])
  } else {
    lines <- c(lines, "", "# Fit or run diagnostics in the GUI to add reproducible action code here.")
  }
  paste(lines, collapse = "\n")
})

model_code <- shiny::reactive({
  paste(c("# Model specification", ctgui_export_code(current_spec())), collapse = "\n")
})

output$code_output <- shiny::renderText(model_code())
output$output_code <- shiny::renderText(workflow_code())

output$pars_table <- shiny::renderTable({
  pars <- current_spec()$pars
  if (is.null(pars)) return(data.frame(message = "Install/load ctsem to show the pars-backed model table"))
  utils::head(pars, 30L)
}, rownames = FALSE)

output$output_pars <- shiny::renderTable({
  pars <- current_spec()$pars
  if (is.null(pars)) return(data.frame(message = "No model pars available"))
  record_output_code("model_pars", fit_pars_code_snippet())
  pars
}, rownames = FALSE)

fit_comparison_stats <- ctgui_fit_comparison_stats

output$fit_comparison <- shiny::renderTable({
  registry <- fit_registry()
  if (length(registry) == 0L) return(data.frame(message = "No saved fits. Save current fits from the Fit tab."))
  record_output_code("fit_comparison", fit_comparison_code_snippet())
  do.call(rbind, lapply(names(registry), function(name) {
    fit <- registry[[name]]
    model_base <- fit$modelbase %||% fit$model %||% fit$ctstanmodelbase %||% fit$ctstanmodel
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

shiny::observeEvent(input$refresh_env_data, update_data_choices())

shiny::observeEvent(input$load_env_data, {
  if (is.null(input$env_data) || !nzchar(input$env_data)) {
    shiny::showNotification("No data.frame selected", type = "error")
    return()
  }
  data <- get(input$env_data, envir = .GlobalEnv)
  if (!is.data.frame(data)) {
    shiny::showNotification("Selected object is no longer a data.frame", type = "error")
    update_data_choices()
    return()
  }
  current_data(data)
  current_data_name(paste0("R data.frame: ", input$env_data))
})

shiny::observeEvent(input$csv_file, {
  data <- tryCatch(utils::read.csv(input$csv_file$datapath, stringsAsFactors = FALSE), error = function(e) e)
  if (inherits(data, "error")) {
    shiny::showNotification(conditionMessage(data), type = "error")
    return()
  }
  current_data(data)
  current_data_name(paste0("CSV: ", input$csv_file$name))
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

output$data_preview_import <- shiny::renderTable(data_preview_table(), rownames = FALSE)
output$data_preview_generate <- shiny::renderTable(data_preview_table(), rownames = FALSE)
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
  record_output_code("raw_plot", raw_plot_code_snippet())
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
  data <- current_data()
  if (is.null(data)) {
    shiny::showNotification("Load or generate data before fitting", type = "error")
    return()
  }

  fit_busy(TRUE)
  on.exit({ fit_busy(FALSE); session$sendCustomMessage("ctgui-fit-finished", list(beep = isTRUE(input$fit_completion_beep))) }, add = TRUE)
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
  record_output_code("fit", fit_code_snippet())
  shiny::showNotification("Fit complete", type = "message")
})

shiny::observeEvent(input$save_fit, {
  fit <- current_fit()
  if (is.null(fit)) {
    shiny::showNotification("No current fit to save", type = "error")
    return()
  }
  name <- trimws(input$fit_save_name %||% "")
  if (!nzchar(name)) name <- paste0("fit", length(fit_registry()) + 1L)
  registry <- fit_registry()
  registry[[name]] <- fit
  fit_registry(registry)
  update_fit_choices(selected = name)
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
output$fit_log <- shiny::renderText(fit_messages())
output$fit_warnings <- shiny::renderText(fit_warnings())
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
        draws = input$fit_uncertainty_draws,
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
  record_output_code("uncertainty", uncertainty_code_snippet())
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
        cores = input$fit_gen_cores
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
  generated_fit(out$value$generated)
  diagnostics_status(paste(c("Fit-generated data available.", out$messages, out$warnings), collapse = "\n"))
  record_output_code("generate_from_fit", generate_from_fit_code_snippet())
})

shiny::observeEvent(input$run_cov_check, {
  fit <- active_fit()
  if (is.null(fit)) {
    shiny::showNotification("Fit the model first", type = "error")
    return()
  }
  if (is.null(fit$generated)) {
    shiny::showNotification("Run Generate from fit before ctFitCovCheck", type = "error")
    return()
  }
  lags <- parse_r_expression(input$cov_lags, 0:3)
  diagnostics_status("Running covariance check...")
  cov_check_log("Running ctFitCovCheck...")
  out <- NULL
  shiny::withProgress(message = "Running ctFitCovCheck", value = 0.2, {
    out <- capture_conditions({
      args <- list(
        fit = fit,
        cor = input$cov_cor,
        lags = lags,
        plot = FALSE,
        cores = 1
      )
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
  record_output_code("cov_check", cov_check_code_snippet())
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
  record_output_code("kalman", kalman_code_snippet())
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
  record_output_code("postpred", postpred_code_snippet())
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
  record_output_code("residual_acf", residual_acf_code_snippet())
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
  record_output_code("dynamics", dynamics_code_snippet())
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
  record_output_code("tipred", tipred_code_snippet())
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
  lags <- parse_r_expression(input$cov_lags, 0:3)
  plots <- tryCatch(
    ctgui_ctsem_call("ctFitCovCheckPlot", out,
      maxlag = max(lags),
      cor = input$cov_cor
    ),
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
  record_output_code("cov_check", cov_check_code_snippet())
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
      shiny::plotOutput(ids[i], height = 430), plot_export_controls(ids[i], 430)
    )
  }))
})

output$cov_check_log <- shiny::renderText(cov_check_log())

output$kalman_plot <- shiny::renderPlot({
  on.exit({ plot_cache$kalman_plot <- grDevices::recordPlot() }, add = TRUE)
  out <- kalman_result()
  if (is.null(out)) return(invisible(NULL))
  record_output_code("kalman", kalman_code_snippet())
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
  record_output_code("postpred", postpred_code_snippet())
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
      shiny::plotOutput(ids[i], height = 430), plot_export_controls(ids[i], 430)
    )
  }))
})

output$postpred_log <- shiny::renderText(postpred_log())

output$residual_acf_plot <- shiny::renderPlot({
  on.exit({ plot_cache$residual_acf_plot <- grDevices::recordPlot() }, add = TRUE)
  out <- residual_acf()
  if (is.null(out)) return(invisible(NULL))
  record_output_code("residual_acf", residual_acf_code_snippet())
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
  record_output_code("dynamics", dynamics_code_snippet())
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
  record_output_code("tipred", tipred_code_snippet())
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
        shiny::plotOutput(ids[i], height = 430), plot_export_controls(ids[i], 430)
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
  record_output_code("summary", summary_code_snippet())
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
  record_output_code("summary_matrices", summary_matrices_code_snippet())
  paste(result, collapse = "\n")
}

output$fit_summary <- shiny::renderText(fit_summary_text())
output$fit_summary_diagnostics <- shiny::renderText(fit_summary_text())
output$fit_summary_matrices <- shiny::renderText(fit_summary_matrices_text())
output$fit_summary_matrices_diagnostics <- shiny::renderText(fit_summary_matrices_text())
  }
  }
