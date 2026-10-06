# Application server composition ---------------------------------------------

ctgui_app_server <- function(initial_spec, help_catalog) {
  function(input, output, session) {
  arg_label <- function(label, help_id, title = NULL) {
    ctgui_arg_label(help_catalog, label, help_id, title)
  }
  current_spec <- shiny::reactiveVal(initial_spec)
spec_history <- shiny::reactiveVal(ctgui_history_new(initial_spec))
current_data <- shiny::reactiveVal(NULL)
current_data_name <- shiny::reactiveVal("No data selected")
current_fit <- shiny::reactiveVal(NULL)
fit_busy <- shiny::reactiveVal(FALSE)
# One background R process for everything that works on a fit, so its Julia
# and the model shapes compiled in it last as long as this session.
worker <- ctgui_worker()
fit_process <- shiny::reactiveVal(NULL)
fit_log_path <- shiny::reactiveVal(NULL)
fit_log_offset <- shiny::reactiveVal(0)
fit_log_state <- shiny::reactiveVal(ctgui_fit_log_state())
gen_process <- shiny::reactiveVal(NULL)
gen_log_path <- shiny::reactiveVal(NULL)
gen_log_offset <- shiny::reactiveVal(0)
gen_log_state <- shiny::reactiveVal(ctgui_fit_log_state())
gen_messages <- shiny::reactiveVal("Nothing has been generated from a fit yet.")
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
# The specification each stored fit was produced from, kept beside the registry
# rather than inside it so the registry's shape, and everything that reads it,
# stays as it was. Without this the comparison table can only report names and
# fit statistics, which is not enough to know what is actually being compared.
fit_specs <- shiny::reactiveVal(list())
# The specification a running fit was started from: the model can be edited
# while a background fit runs, and the fit belongs to the one it was given.
pending_fit_spec <- shiny::reactiveVal(NULL)
fit_counter <- shiny::reactiveVal(0L)
output_code_snippets <- shiny::reactiveVal(list())
diagnostics_status <- shiny::reactiveVal("No fit diagnostics have been run.")
matrix_status <- shiny::reactiveVal("Matrix edits update the current model spec.")
plot_cache <- shiny::reactiveValues()
spec_inputs_suspended <- shiny::reactiveVal(FALSE)
fit_gen_cores_follow_fit <- shiny::reactiveVal(TRUE)

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

explain_ui <- function(key) {
  ctgui_explanation_ui(key)
}

# Surviving a crash -----------------------------------------------------------

# Read before anything can overwrite it. The autosave observer below fires as
# the session initialises, so a state read later would already be this
# session's empty starting model rather than the one being recovered.
recoverable_state <- ctgui_autosave_read()

# The model is the one thing in a session that cannot be reconstructed from
# anything else, so it is written to the cache directory whenever it changes.
# Commits are discrete events rather than keystrokes, so this does not write on
# every character typed.
#
# An empty starting model is not written at all: it holds nothing anyone would
# miss, and saving it would overwrite a real model left by an earlier session
# before its owner had the chance to recover it.
shiny::observe({
  spec <- current_spec()
  if (!ctgui_autosave_worth_offering(list(spec = spec))) return()
  shiny::isolate({
    ctgui_autosave_write(
      spec = spec,
      history = spec_history(),
      data = current_data(),
      data_name = current_data_name()
    )
  })
})

restore_autosave <- function(state) {
  spec <- state$spec
  if (!is.null(state$data)) {
    current_data(state$data)
    current_data_name(state$data_name %||% "Recovered data")
  }
  current_spec(spec)
  spec_history(state$history %||% ctgui_history_new(spec))
  sync_matrix_inputs_from_spec(spec)
  refresh_visual_editor(spec)
  fit_status_value("Model recovered. Refit when ready.")
  shiny::showNotification("Recovered the model from your last session.", type = "message")
}

shiny::observeEvent(input$autosave_restore, {
  shiny::removeModal()
  if (is.null(recoverable_state)) {
    shiny::showNotification("There was nothing left to recover.", type = "warning")
    return()
  }
  restore_autosave(recoverable_state)
})

shiny::observeEvent(input$autosave_discard, {
  ctgui_autosave_clear()
  shiny::removeModal()
})

# Asked once, at the start, and only when there is something worth asking
# about: a model from a different R process that has variables in it.
if (ctgui_autosave_worth_offering(recoverable_state) &&
    ctgui_autosave_from_other_session(recoverable_state)) {
  session$onFlushed(function() {
    shiny::showModal(shiny::modalDialog(
      title = "Recover your last model?",
      shiny::tags$p(
        "A model from an earlier session was saved automatically and has not been opened since."
      ),
      shiny::tags$p(class = "help-note", ctgui_autosave_describe(recoverable_state)),
      shiny::tags$p(
        class = "help-note",
        "Fits are not saved. Recovering brings back the model, its history and any data small enough to keep."
      ),
      footer = shiny::tagList(
        shiny::actionButton("autosave_discard", "Start fresh"),
        shiny::actionButton("autosave_restore", "Recover", class = "btn-primary")
      )
    ))
  }, once = TRUE)
}

# The visual editor rebuilds itself when the Model sub-tab is opened, which is
# enough while the user is navigating but not when the whole model is replaced
# from somewhere else. Opening an example or applying a template switches the
# top-level tab, so the sub-tab never changes and the canvas keeps showing the
# model that is no longer there -- blank, for a model that started empty.
#
# This is called only where a wholesale replacement happens. Refreshing on
# every commit would send the graph back mid-interaction and undo a drag the
# user was in the middle of.
refresh_visual_editor <- function(spec = current_spec()) {
  invisible(tryCatch(visual_server$refresh(spec), error = function(e) NULL))
}

# Model history ---------------------------------------------------------------

# Moving through history restores a specification that was already committed,
# so it applies the spec directly instead of going back through
# commit_current_spec(), which would push the restored state on as a new entry
# and make undo unreachable.
restore_history <- function(history) {
  spec_history(history)
  spec <- ctgui_history_current(history)
  current_spec(spec)
  current_fit(NULL)
  clear_uncertainty_state()
  clear_diagnostics()
  sync_matrix_inputs_from_spec(spec)
  # visual_server is created further down this closure; restore_history only
  # runs from an observer, which is long after the server body has finished.
  refresh_visual_editor(spec)
  fit_status_value("The model changed. Refit when ready.")
}

shiny::observeEvent(input$history_undo, {
  history <- spec_history()
  if (!ctgui_history_can_undo(history)) {
    shiny::showNotification("Nothing to undo.", type = "warning")
    return()
  }
  restore_history(ctgui_history_undo(history))
})

shiny::observeEvent(input$history_redo, {
  history <- spec_history()
  if (!ctgui_history_can_redo(history)) {
    shiny::showNotification("Nothing to redo.", type = "warning")
    return()
  }
  restore_history(ctgui_history_redo(history))
})

shiny::observeEvent(input$history_go, {
  restore_history(ctgui_history_go(spec_history(), input$history_go))
})

output$history_log <- shiny::renderTable(
  ctgui_history_log(spec_history()),
  rownames = FALSE
)

output$history_status <- shiny::renderText({
  history <- spec_history()
  paste0(
    "Step ", history$position, " of ", length(history$entries), ". ",
    if (ctgui_history_can_undo(history)) "" else "Nothing to undo. ",
    if (ctgui_history_can_redo(history)) "There are undone steps you can redo." else ""
  )
})

# Worked examples -------------------------------------------------------------

selected_example <- shiny::reactive({
  tryCatch(ctgui_example(input$example_id %||% "coupled"), error = function(e) e)
})

output$example_guidance <- shiny::renderText({
  example <- selected_example()
  if (inherits(example, "error")) return(conditionMessage(example))
  paste(c(example$look_for, "", ctgui_example_truth_note(example)), collapse = "\n")
})

shiny::observeEvent(input$example_load, {
  example <- selected_example()
  if (inherits(example, "error")) {
    shiny::showNotification(conditionMessage(example), type = "error")
    return()
  }
  loaded <- NULL
  shiny::withProgress(message = paste("Opening", example$title), value = 0.3, {
    loaded <- tryCatch(
      list(
        spec = ctgui_example_spec(example),
        data = ctgui_example_data(example)
      ),
      error = function(e) e
    )
  })
  if (inherits(loaded, "error")) {
    shiny::showNotification(conditionMessage(loaded), type = "error")
    return()
  }
  current_data(loaded$data)
  current_data_name(ctgui_example_data_label(example))
  commit_current_spec(loaded$spec, reason = "example")
  refresh_visual_editor(loaded$spec)
  fit_status_value("Example loaded. Fit when ready.")
  shiny::showNotification(paste(example$title, "opened."), type = "message")
  shiny::updateTabsetPanel(session, "workflow", selected = "Model")
})

# Model templates ------------------------------------------------------------

build_mode <- function() {
  if (identical(input$build_mode, "extend")) "extend" else "replace"
}

# Keyed by position, so renaming a process keeps the variables chosen for it.
build_manifest_id <- function(index) paste0("build_manifests_", index)

output$build_manifest_selectors <- shiny::renderUI({
  processes <- ctgui_parse_names(input$build_processes)
  if (!length(processes)) return(NULL)
  choices <- ctgui_build_manifest_choices(current_data(), current_spec(), build_mode())
  shiny::tagList(lapply(seq_along(processes), function(index) {
    id <- build_manifest_id(index)
    kept <- as.character(shiny::isolate(input[[id]]) %||% character())
    ctgui_variable_select_ui(
      id, paste("Manifest variables measuring", processes[index]),
      choices = unique(c(choices, kept)), selected = kept, note = NULL,
      placeholder = paste0("Data variables or new names (empty: ", processes[index], "_1, ...)")
    )
  }))
})

# Invalid input is normal while a name is being typed, so the blueprint reports
# why it cannot be built and the panel says so, rather than erroring.
current_blueprint <- shiny::reactive({
  processes <- ctgui_parse_names(input$build_processes)
  tryCatch(
    ctgui_blueprint(
      structure = input$build_structure %||% "coupled",
      processes = processes,
      manifests = lapply(seq_along(processes), function(index) input[[build_manifest_id(index)]]),
      indicators = input$build_indicators %||% 1L,
      free_noise_correlations = isTRUE(input$build_noise_correlations),
      connect_existing = isTRUE(input$build_connect_existing),
      indicator_type = input$build_indicator_type %||% 0L,
      indicator_ncategories = input$build_indicator_ncategories %||% 5L,
      indicator_censormin = input$build_indicator_censormin %||% NA_real_,
      indicator_censormax = input$build_indicator_censormax %||% NA_real_
    ),
    error = function(e) e
  )
})

output$build_indicator_note <- shiny::renderUI({
  entry <- ctgui_manifest_type_entry(input$build_indicator_type %||% 0L)
  shiny::tagList(
    shiny::tags$p(class = "help-note", entry$short),
    shiny::tags$p(class = "help-note ctgui-explain-detail", entry$detail)
  )
})

output$build_summary <- shiny::renderText({
  blueprint <- current_blueprint()
  if (inherits(blueprint, "error")) return(conditionMessage(blueprint))
  ctgui_blueprint_summary(current_spec(), blueprint, build_mode())
})

shiny::observeEvent(input$build_apply, {
  blueprint <- current_blueprint()
  if (inherits(blueprint, "error")) {
    shiny::showNotification(conditionMessage(blueprint), type = "error")
    return()
  }
  updated <- tryCatch(
    ctgui_blueprint_apply(current_spec(), blueprint, mode = build_mode()),
    error = function(e) e
  )
  if (inherits(updated, "error")) {
    shiny::showNotification(conditionMessage(updated), type = "error")
    return()
  }
  commit_current_spec(updated, reason = "blueprint")
  refresh_visual_editor(updated)
  fit_status_value("The model changed. Refit when ready.")
  matrix_status("Built from a template. Every matrix cell was set by the template.")
  shiny::showNotification(
    paste0(
      ctgui_blueprint_structure(blueprint$structure)$title,
      if (identical(build_mode(), "extend")) " added." else " built."
    ),
    type = "message"
  )
  shiny::updateTabsetPanel(session, "workflow", selected = "Model")
})

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

shiny::observeEvent(input$close_gui, {
  shiny::stopApp()
}, ignoreInit = TRUE)

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

is_omitted_arg <- function(x) inherits(x, "ctgui_omitted_arg")

# An argument field, as the value to pass, or NULL when it is left to ctsem.
arg_field <- function(id, help_id) ctgui_arg_value(input[[id]], ctgui_help_arg(help_catalog, help_id))
compact_args <- function(args) args[!vapply(args, is.null, logical(1L))]

# What each diagnostic is called with. The run handler and the reproducible
# code both read these, so the code shows what ran.
kalman_call_args <- function() compact_args(list(
  subjects = arg_field("kalman_subjects", "help_kalman_subjects"),
  timerange = arg_field("kalman_timerange", "help_kalman_timerange"),
  timestep = arg_field("kalman_timestep", "help_kalman_timestep"),
  removeObs = arg_field("kalman_remove_obs", "help_kalman_removeObs")
))
kalman_plot_args <- function() compact_args(list(
  kalmanvec = arg_field("kalman_vec", "help_kalmanvec"),
  errorvec = arg_field("kalman_error_vec", "help_errorvec")
))
acf_call_args <- function() compact_args(list(
  varnames = arg_field("acf_vars", "help_acf_varnames"),
  nboot = arg_field("acf_boot", "help_acf_nboot")
))
dynamics_call_args <- function() {
  impulse <- ctgui_dynamics_impulse_param()
  args <- list(
    subjects = arg_field("dynamic_subjects", "help_dynamic_subjects"),
    times = arg_field("dynamic_times", "help_dynamic_times"),
    nsamples = arg_field("dynamic_samples", "help_dynamic_nsamples")
  )
  args[[impulse]] <- arg_field("dynamic_impulse", paste0("help_dynamic_", impulse))
  compact_args(args)
}
tipred_call_args <- function() compact_args(list(
  tipreds = arg_field("tipred_effects_preds", "help_tipred_tipreds"),
  subject = arg_field("tipred_effects_subject", "help_tipred_subject"),
  timestep = arg_field("tipred_effects_timestep", "help_tipred_timestep"),
  TIPvalues = arg_field("tipred_effects_tipvalues", "help_tipred_tipvalues")
))

# Fields whose choices come from the active fit: its manifest and TI
# predictor names.
shiny::observe({
  fit <- active_fit()
  model <- if (is.null(fit)) NULL else ctgui_ctsem_fit_model(fit, NULL)
  fields <- list(
    acf_vars = list("help_acf_varnames", model$manifestNames %||% current_spec()$manifest_names),
    tipred_effects_preds = list("help_tipred_tipreds", model$TIpredNames %||% current_spec()$tipred_names)
  )
  for (id in names(fields)) {
    ctgui_update_arg_choices(session, id, fields[[id]][[2L]], shiny::isolate(input[[id]]),
      ctgui_help_arg(help_catalog, fields[[id]][[1L]]))
  }
})

# Subjects are too many to choose from a list, so the fields are typed and
# the fit's subjects are described beneath them. A ctsem function with a
# realid argument takes the data's ids, as ctPredict always has and the others
# do from ctsem 3.12; one without takes ctsem's numbering 1..N.
subject_hint <- function(topic) shiny::renderUI({
  fit <- active_fit()
  if (is.null(fit)) return(NULL)
  realid <- isTRUE(ctgui_ctsem_arg(topic, "realid")$found)
  ids <- ctgui_fit_subjects(fit)[[if (realid) "original" else "internal"]]
  if (!length(ids)) return(NULL)
  shiny::helpText(paste0(if (realid) "Subject ids in the fitted data: " else "Subjects, numbered by ctsem: ",
    ctgui_describe_ids(ids)))
})
output$kalman_subjects_hint <- subject_hint("ctPredict")
output$dynamic_subjects_hint <- subject_hint("ctDiscretePars")
output$tipred_effects_subject_hint <- subject_hint("ctPredictTIP")

generate_from_fit_cores <- function() {
  if (isTRUE(fit_gen_cores_follow_fit())) input$fit_cores else input$fit_gen_cores
}

shiny::observeEvent(input$fit_cores, {
  if (isTRUE(fit_gen_cores_follow_fit())) {
    shiny::updateNumericInput(session, "fit_gen_cores", value = input$fit_cores)
  }
}, ignoreInit = TRUE)

shiny::observeEvent(input$fit_gen_cores, {
  if (isTRUE(fit_gen_cores_follow_fit()) &&
      !identical(as.integer(input$fit_gen_cores), as.integer(input$fit_cores))) {
    fit_gen_cores_follow_fit(FALSE)
  }
}, ignoreInit = TRUE)

cov_check_lags <- function() {
  text <- input$cov_lags
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  parse_r_expression(text, 0:3)
}

# What a panel's More arguments section passes (R/more_args.R); an argument
# the call already sets is not overridden.
more_args <- function(panel, role = "call") ctgui_more_args_values(input, help_catalog, panel, role)
append_more_args <- function(args, panel, role = "call", protected = names(args)) {
  extra <- more_args(panel, role)
  c(args, extra[!names(extra) %in% protected])
}

# Every in-session call renders its messages as the fit log does, carriage
# returns applied (see ctgui_run_result). This used to sort messages by keyword
# instead, keeping only the last that looked like progress: a counter without
# one of the keywords flooded the box, and ctsem's own messages that happened
# to contain one -- "Hessian", "draws" -- were dropped.
capture_conditions <- function(expr, progress_callback = NULL) {
  ctgui_run_result(function() expr, progress_callback)
}

# A ctsem call on a fit, made in the background process when it is free, where
# the model's shape is compiled already (see ctgui_worker_run). The arguments
# are built first, so a mistake in them is reported the same way as a failure
# of the call.
run_on_fit <- function(name, build_args) {
  args <- tryCatch(build_args(), error = function(e) e)
  if (inherits(args, "error")) {
    return(list(value = args, messages = character(), warnings = character()))
  }
  ctgui_worker_run(worker, name, args)
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

# Every fit joins the list as it completes, with the specification it was
# started from, and becomes the active one. A refit therefore never loses the
# last fit, and the comparison table always has both. Fits stay in memory for
# the session, so the list can be pruned with Remove fit.
keep_fit <- function(fit, spec, name = NULL) {
  shiny::isolate({
    registry <- fit_registry()
    if (is.null(name)) {
      number <- fit_counter() + 1L
      while (paste0("fit", number) %in% names(registry)) number <- number + 1L
      fit_counter(number)
      name <- paste0("fit", number)
    }
    name <- ctgui_unique_name(name, names(registry))
    registry[[name]] <- fit
    fit_registry(registry)
    specs <- fit_specs()
    specs[[name]] <- spec
    fit_specs(specs)
    current_fit(fit)
    update_fit_choices(selected = name)
  })
  name
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

output$fit_expression_warning <- shiny::renderUI({
  if (!ctgui_spec_has_expressions(current_spec())) return(NULL)
  shiny::tags$p(
    class = "warning-note",
    "This model uses expression parameters. On the Stan engine it is compiled before fitting, which needs R set up to compile C++ and can take a few minutes."
  )
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
  spec <- tryCatch(ctgui_spec_from_model(ctgui_ctsem_fit_model(fit)), error = function(e) NULL)
  name <- keep_fit(fit, spec, name = sub("\\.rds$", "", input$load_fit_rds$name %||% "loaded", ignore.case = TRUE))
  clear_uncertainty_state(); clear_diagnostics()
  fit_status_value(paste0("Loaded fit from RDS as ", name, ".")); shiny::showNotification("Loaded fit RDS", type = "message")
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
  # Only model changes become history entries. Recording every commit would
  # fill the log with steps that changed nothing the user can see -- a graph
  # relayout, most often -- and make undo appear not to work when pressed.
  history <- spec_history()
  if (isTRUE(commit$effects$changed) &&
      ctgui_history_is_model_change(ctgui_history_current(history), commit$spec)) {
    spec_history(ctgui_history_push(history, commit$spec, reason = reason))
  }
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
  spec <- current_spec()
  # Read the live controls rather than the committed spec, so the extra
  # arguments appear as soon as a type is chosen rather than after a commit.
  measurement <- ctgui_measurement_input_values(manifest_names, input, spec)

  shiny::tagList(
    shiny::tags$h4("How each variable was measured"),
    ctgui_explanation_ui("measurement"),
    shiny::uiOutput("measurement_backend_note"),
    shiny::div(
      class = "manifest-type-grid",
      lapply(seq_along(manifest_names), function(i) {
        ctgui_manifest_measurement_ui(
          index = i, name = manifest_names[i],
          type = measurement$manifest_type[i],
          ncategories = measurement$ncategories[i],
          censormin = measurement$censormin[i],
          censormax = measurement$censormax[i]
        )
      })
    )
  )
})

# Which engine a model needs follows from its measurement types, so it is
# reported where the types are chosen and again beside the engine selector. A
# user who picks ordinal and then meets a Stan error about a backend they never
# chose has been let down twice.
measurement_backend_note <- function(spec = current_spec()) {
  message <- ctgui_measurement_backend_message(spec$manifest_names, spec$manifest_type)
  if (!nzchar(message)) return(NULL)
  julia <- shiny::isolate(julia_status())
  available <- isTRUE(julia$available)
  shiny::div(
    class = if (available) "help-note" else "warning-note",
    message,
    if (!available) {
      paste(" Julia is not available here, so this model cannot be fitted as specified.",
        paste0(ctgui_julia_remedy(), ", or choose continuous or binary types instead."))
    }
  )
}

output$measurement_backend_note <- shiny::renderUI(measurement_backend_note())
output$fit_measurement_note <- shiny::renderUI(measurement_backend_note())

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
  # A visual/matrix commit updates the hidden Specification controls
  # asynchronously.  Only use a tab payload to rebuild canonical state when
  # the user actually authored one of those controls.
  specification_authored <- is.list(event) && isTRUE(event$specification_authored)
  committed <- if (specification_authored) event$specification else NULL
  if (specification_authored) rebuild_spec_if_needed(committed)
  # Matrix fields commit their user-authored value in `matrix_commit_nonce`.
  # Replaying the last such payload here would let stale hidden inputs undo a
  # visual-editor mutation as the user enters the Specification tab.
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

latex_source <- shiny::reactive({
  args <- c(list(spec = current_spec()), equation_args())
  tryCatch(do.call(ctgui_latex, args), error = function(e) paste("Could not create equations:", conditionMessage(e)))
})

# Any reason the equations cannot be shown as rows is reported by the view
# itself, beside the source it is explaining, so there is no separate status
# line to keep in step with it.
output$equation_blocks <- shiny::renderUI({
  ctgui_equation_view_ui(
    ctgui_equation_view(latex_source()),
    empty_message = "Add latent processes and manifest variables to see the model equations."
  )
})

output$equation_source <- shiny::renderText(latex_source())

fit_latex_source <- shiny::reactive({
  fit <- active_fit()
  if (is.null(fit)) return("No fit available.")
  model_latex_source(fit, fit_equation_args(), fallback = fit_model_object(fit))
})

output$fit_equation_blocks <- shiny::renderUI({
  latex <- fit_latex_source()
  if (identical(latex, "No fit available.")) {
    return(ctgui_equation_view_ui(
      list(status = "empty", blocks = list(), source = "", message = ""),
      empty_message = "Fit a model to see its estimated equations."
    ))
  }
  ctgui_equation_view_ui(ctgui_equation_view(latex))
})

output$fit_equation_source <- shiny::renderText(fit_latex_source())
output$validation_table_spec <- shiny::renderTable(ctgui_validate(current_spec()), rownames = FALSE)

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
      priors = arg_field("fit_priors", "help_fit_priors"),
      cores = input$fit_cores,
      uncertainty = input$fit_uncertainty_method,
      uncertainty_draws = ctgui_uncertainty_default_draws(input$fit_uncertainty_method),
      finishsamples = input$fit_uncertainty_samples,
      uncertainty_control = uncertainty_control(),
      extra_args = more_args("fit")
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
      nsamples = arg_field("fit_gen_samples", "help_fit_gen_nsamples"),
      fullposterior = input$fit_gen_fullposterior,
      cores = generate_from_fit_cores(),
      extra_args = more_args("generate_from_fit")
    ),
    cov_check = list(
      lags = cov_check_lags(),
      cor = input$cov_cor,
      cores = 1L,
      extra_args = more_args("cov_check")
    ),
    kalman = c(kalman_call_args(), kalman_plot_args(),
      list(extra_args = more_args("kalman"), plot_extra_args = more_args("kalman", "plot"))),
    postpred = list(extra_args = more_args("postpred")),
    residual_acf = c(acf_call_args(),
      list(extra_args = more_args("residual_acf"))),
    dynamics = c(dynamics_call_args(), list(
      cores = 1L,
      ylim = input$dynamic_ylim,
      extra_args = more_args("dynamics")
    )),
    tipred = c(tipred_call_args(), list(extra_args = more_args("tipred"))),
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

# The same material as the generated code, arranged for someone to read. It is
# written out as Quarto source rather than rendered, so it needs no Quarto or
# pandoc installation and can be edited before it is rendered.
output$download_report <- shiny::downloadHandler(
  filename = function() {
    paste0("ctsemgui-report-", format(Sys.time(), "%Y%m%d-%H%M"), ".qmd")
  },
  content = function(file) {
    fit <- active_fit()
    spec <- current_spec()
    notes <- if (is.null(fit)) list() else tryCatch(
      ctgui_interpret_fit(fit, current_data(), spec$id, spec$time),
      error = function(e) list()
    )
    writeLines(
      ctgui_report_document(
        spec = spec,
        source = output_data_source(),
        snippets = output_code_snippets(),
        notes = notes,
        warnings = fit_warnings(),
        latex = tryCatch(latex_source(), error = function(e) NULL),
        has_fit = !is.null(fit)
      ),
      file
    )
  }
)

fit_comparison_stats <- ctgui_fit_comparison_stats

output$fit_comparison <- shiny::renderTable({
  registry <- fit_registry()
  if (length(registry) == 0L) return(data.frame(message = "No fits yet. Each fit appears here when it completes."))
  record_output_code("fit_comparison", output_code_snippet("fit_comparison"))
  specs <- fit_specs()
  # Comparing fit statistics only tells you which number is larger. The first
  # fit in the list is the baseline and every other row says how its model differs
  # from it, so the reader can see what the difference in fit is buying.
  baseline_name <- names(registry)[1L]
  baseline_spec <- specs[[baseline_name]]
  do.call(rbind, lapply(names(registry), function(name) {
    fit <- registry[[name]]
    model_base <- ctgui_ctsem_fit_model(fit, list())
    stats <- fit_comparison_stats(fit)
    spec <- specs[[name]]
    model_difference <- if (identical(name, baseline_name)) {
      "baseline"
    } else if (is.null(spec) || is.null(baseline_spec)) {
      "model not recorded"
    } else if (!ctgui_history_is_model_change(baseline_spec, spec)) {
      "same model as baseline"
    } else {
      ctgui_history_describe(baseline_spec, spec, reason = "specification")
    }
    data.frame(
      fit = name,
      model = model_difference,
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

# Answering starts Julia, which costs seconds and may build the engine. The
# question is asked in the background process as soon as the session loads and
# the application carries on without the answer: by the time anyone reaches
# the Fit panel it has arrived, and the Julia it started is the one the first
# fit will use.
julia_status <- shiny::reactiveVal(ctgui_julia_known() %||% ctgui_julia_pending())
julia_check <- shiny::reactiveVal(NULL)

julia_check_log <- shiny::reactiveVal(NULL)

if (ctgui_julia_is_pending(shiny::isolate(julia_status()))) {
  if (requireNamespace("callr", quietly = TRUE)) {
    check_log <- ctgui_fit_log_path()
    started <- tryCatch(
      ctgui_background_start(worker, ctgui_julia_check_worker, list(), check_log),
      error = function(e) NULL
    )
    julia_check_log(check_log)
    if (is.null(started)) {
      julia_status(ctgui_julia_remember(list(
        available = FALSE, message = "Could not check for the Julia engine."
      )))
    } else {
      julia_check(started)
    }
  } else {
    julia_status(ctgui_julia_remember(list(
      available = FALSE,
      message = "Checking for the Julia engine needs the callr package."
    )))
  }
}

shiny::observe({
  process <- julia_check()
  if (is.null(process)) return()
  shiny::invalidateLater(500)
  if (ctgui_job_running(process)) return()

  outcome <- ctgui_job_collect(process)
  julia_check(NULL)
  unlink(shiny::isolate(julia_check_log()))
  julia_check_log(NULL)
  status <- ctgui_julia_remember(ctgui_julia_status_from_check(
    if (identical(outcome$status, "value")) outcome$value else NULL
  ))
  julia_status(status)

  if (!isTRUE(status$available)) return()
  # The selector starts with Stan alone because that is all that is known to
  # work until the check comes back. Julia is added and preferred once it is
  # confirmed, unless the user has already chosen for themselves.
  chosen <- shiny::isolate(input$fit_backend)
  if (!is.null(chosen) && !identical(chosen, "stan")) return()
  shiny::updateSelectInput(
    session, "fit_backend",
    choices = ctgui_backend_choices(status),
    selected = ctgui_default_backend(status)
  )
})

output$fit_backend_status <- shiny::renderUI({
  status <- julia_status()
  shiny::tagList(
    shiny::tags$p(
      class = if (ctgui_julia_is_pending(status)) "help-note" else {
        if (isTRUE(status$available)) "help-note" else "help-note ctgui-explain-detail"
      },
      status$message
    ),
    shiny::tags$p(
      class = "help-note ctgui-explain-detail",
      "Both engines fit the same model specification. Julia is the engine being developed and covers more measurement types; Stan remains available for continuous and binary data, and comparing the two can help when a fit behaves oddly."
    )
  )
})

fit_backend <- function() {
  selected <- input$fit_backend
  if (is.null(selected) || !nzchar(selected)) {
    return(ctgui_default_backend(shiny::isolate(julia_status())))
  }
  selected
}

# A model whose measurement types Stan cannot fit is stopped here rather than
# left to fail inside ctFit, where the message names a backend the user never
# chose. If Julia is available the engine is simply switched, since the choice
# was made for them by the model; if it is not, the fit cannot happen at all
# and saying so now is better than failing later.
fit_backend_for_model <- function(spec) {
  requested <- fit_backend()
  if (!ctgui_measurement_requires_julia(spec$manifest_type)) {
    return(list(backend = requested, message = ""))
  }
  reason <- ctgui_measurement_backend_message(spec$manifest_names, spec$manifest_type)
  if (isTRUE(shiny::isolate(julia_status())$available)) {
    if (!identical(requested, "julia")) {
      shiny::updateSelectInput(session, "fit_backend", selected = "julia")
    }
    return(list(
      backend = "julia",
      message = if (identical(requested, "julia")) "" else paste("Switched to the Julia engine:", reason)
    ))
  }
  list(backend = NA_character_, message = paste(
    reason,
    "Julia is not available here, so this model cannot be fitted.",
    paste0(ctgui_julia_remedy(), ", or change those variables to continuous or binary.")
  ))
}

fit_call_args <- function(data) {
  spec <- current_spec()
  engine <- fit_backend_for_model(spec)
  if (is.na(engine$backend)) stop(engine$message, call. = FALSE)
  if (nzchar(engine$message)) shiny::showNotification(engine$message, type = "warning")
  args <- list(
    datalong = data,
    model = ctgui_to_ctsem_model(spec, silent = TRUE),
    optimize = input$fit_optimize,
    cores = input$fit_cores,
    backend = engine$backend,
    plot = FALSE
  )
  args$priors <- arg_field("fit_priors", "help_fit_priors")
  extra <- more_args("fit")
  if (isTRUE(input$fit_optimize)) {
    supplied_optimcontrol <- extra$optimcontrol
    extra$optimcontrol <- NULL
    args$optimcontrol <- ctgui_uncertainty_merge_optimcontrol(
      uncertainty_optimcontrol(), supplied_optimcontrol)
  }
  c(args, extra[!names(extra) %in% names(args)])
}

fit_succeeded <- function(fit) {
  name <- keep_fit(fit, shiny::isolate(pending_fit_spec()) %||% shiny::isolate(current_spec()))
  clear_diagnostics()
  fit_status_value(paste0(
    "Fit available as ", name, " (", ctgui_ctsem_fit_backend_name(fit), " backend)."
  ))
  uncertainty_status_value("Uncertainty was estimated as part of fitting.")
  record_output_code("fit", output_code_snippet("fit"))
  shiny::showNotification("Fit complete", type = "message")
  # Almost every diagnostic needs generated data, so producing it once here
  # saves the user discovering that one panel at a time and running it by hand.
  if (isTRUE(input$fit_generate_after)) start_generate_from_fit(fit)
}

# Generating from a fit is as slow as a fit and equally worth watching, so it
# uses the same background process and the same live log.
start_generate_from_fit <- function(fit) {
  if (is.null(fit)) return(invisible(FALSE))
  if (ctgui_job_running(shiny::isolate(gen_process()))) return(invisible(FALSE))
  if (!requireNamespace("callr", quietly = TRUE)) return(invisible(FALSE))

  args <- tryCatch(
    append_more_args(
      compact_args(list(
        fit = fit,
        nsamples = arg_field("fit_gen_samples", "help_fit_gen_nsamples"),
        fullposterior = isTRUE(input$fit_gen_fullposterior),
        cores = generate_from_fit_cores()
      )),
      "generate_from_fit"
    ),
    error = function(e) e
  )
  if (inherits(args, "error")) {
    diagnostics_status(conditionMessage(args))
    return(invisible(FALSE))
  }

  log_path <- ctgui_fit_log_path()
  process <- tryCatch(ctgui_generate_process_start(worker, args, log_path), error = function(e) e)
  if (inherits(process, "error")) {
    diagnostics_status(conditionMessage(process))
    return(invisible(FALSE))
  }
  # Jobs queue for the one background process, so a fit started meanwhile can
  # finish first; what comes back belongs only to the fit it was made from.
  process$source <- fit
  gen_log_path(log_path)
  gen_log_offset(0)
  gen_log_state(ctgui_fit_log_state("Generating samples from the fitted model..."))
  gen_messages(ctgui_fit_log_text(shiny::isolate(gen_log_state())))
  diagnostics_status("Generating samples from the fitted model...")
  gen_process(process)
  invisible(TRUE)
}

drain_gen_log <- function() {
  chunk <- ctgui_fit_log_read(
    shiny::isolate(gen_log_path()), shiny::isolate(gen_log_offset())
  )
  if (!nzchar(chunk$text)) return(invisible(FALSE))
  gen_log_offset(chunk$offset)
  gen_log_state(ctgui_fit_log_append(shiny::isolate(gen_log_state()), chunk$text))
  gen_messages(ctgui_fit_log_text(shiny::isolate(gen_log_state())))
  invisible(TRUE)
}

shiny::observe({
  process <- gen_process()
  if (is.null(process)) return()
  shiny::invalidateLater(500)
  drain_gen_log()
  if (ctgui_job_running(process)) return()
  drain_gen_log()

  outcome <- ctgui_job_collect(process)
  gen_process(NULL)
  unlink(shiny::isolate(gen_log_path()))
  gen_log_path(NULL)

  if (identical(outcome$status, "cancelled")) {
    diagnostics_status("Generation stopped.")
  } else if (identical(outcome$status, "error")) {
    diagnostics_status(paste("Generation failed:", conditionMessage(outcome$error)))
    gen_log_state(ctgui_fit_log_append(
      shiny::isolate(gen_log_state()),
      paste0("\n", conditionMessage(outcome$error), "\n")
    ))
    gen_messages(ctgui_fit_log_text(shiny::isolate(gen_log_state())))
  } else if (!identical(shiny::isolate(active_fit()), process$source)) {
    diagnostics_status("Generated data discarded: the fit it came from is no longer the active one.")
  } else {
    replace_active_fit(outcome$value)
    generated_fit(ctgui_ctsem_fit_generated(outcome$value))
    diagnostics_status("Fit-generated data available.")
    record_output_code("generate_from_fit", output_code_snippet("generate_from_fit"))
  }
})

shiny::observeEvent(input$cancel_generate, {
  process <- gen_process()
  if (!ctgui_job_running(process)) {
    shiny::showNotification("No generation is running.", type = "warning")
    return()
  }
  diagnostics_status("Stopping generation...")
  ctgui_job_cancel(process)
})

output$generate_log <- shiny::renderText(gen_messages())

# Reading a fit in words ------------------------------------------------------

# A separate panel, so nothing here interposes itself between the user and the
# estimates. It computes only when opened, because outputs on hidden tabs are
# suspended.
output$fit_reading <- shiny::renderUI({
  fit <- active_fit()
  if (is.null(fit)) {
    return(shiny::div(class = "help-note", "Fit a model to read it in words."))
  }
  spec <- current_spec()
  notes <- tryCatch(
    ctgui_interpret_fit(fit, current_data(), spec$id, spec$time),
    error = function(e) NULL
  )
  if (!length(notes)) {
    return(shiny::div(
      class = "help-note",
      "There is nothing to read from this fit. That happens when the drift matrix could not be recovered from it."
    ))
  }
  shiny::div(
    class = "reading-list",
    lapply(notes, function(note) {
      shiny::div(
        class = if (identical(note$kind, "caution")) "reading-note reading-caution" else "reading-note",
        note$text
      )
    })
  )
})

fit_finished <- function() {
  fit_busy(FALSE)
  session$sendCustomMessage(
    "ctgui-fit-finished",
    list(beep = isTRUE(input$fit_completion_beep))
  )
}

fit_in_background <- function() {
  isTRUE(input$fit_async %||% TRUE) && requireNamespace("callr", quietly = TRUE)
}

# Warnings are pulled out of the fit's output into the panel that exists for
# them. Left only in the message stream, the box a user checks stays empty
# while the thing they need is buried in a few hundred lines.
publish_fit_log <- function() {
  state <- shiny::isolate(fit_log_state())
  fit_messages(ctgui_fit_log_text(state))
  warnings <- ctgui_fit_log_warnings(state$lines)
  fit_warnings(if (length(warnings)) paste(warnings, collapse = "\n") else "No warnings.")
}

# Read whatever the child has written since the last read and render it. The
# incomplete tail of a line is carried inside the state, so a progress line
# caught mid-rewrite is shown once, finished, rather than once per poll.
drain_fit_log <- function() {
  chunk <- ctgui_fit_log_read(
    shiny::isolate(fit_log_path()), shiny::isolate(fit_log_offset())
  )
  if (!nzchar(chunk$text)) return(invisible(FALSE))
  fit_log_offset(chunk$offset)
  fit_log_state(ctgui_fit_log_append(shiny::isolate(fit_log_state()), chunk$text))
  publish_fit_log()
  invisible(TRUE)
}

shiny::observeEvent(input$run_fit, {
  if (isTRUE(fit_busy())) return()
  data <- current_data()
  if (is.null(data)) {
    shiny::showNotification("Load or generate data before fitting", type = "error")
    fit_finished()
    return()
  }

  current_fit(NULL)
  pending_fit_spec(current_spec())
  clear_uncertainty_state()
  fit_busy(TRUE)
  fit_status_value("Fitting...")
  fit_warnings("No warnings.")

  args <- tryCatch(fit_call_args(data), error = function(e) e)
  if (inherits(args, "error")) {
    fit_status_value("Fit failed.")
    fit_messages(conditionMessage(args))
    shiny::showNotification(conditionMessage(args), type = "error")
    fit_finished()
    return()
  }

  if (!fit_in_background()) {
    # The in-session path is kept as a fallback. It blocks the interface for
    # the whole fit and cannot be stopped, which is why it is not the default,
    # but it needs no extra package and no second process.
    fit_messages("Fitting in this session. The interface will not respond until it finishes.")
    result <- NULL
    shiny::withProgress(message = "Fitting ctsem model", value = 0.1, {
      shiny::incProgress(0.2, detail = "Calling ctFit")
      result <- capture_conditions(
        ctgui_ctsem_call("ctFit", .args = args),
        progress_callback = function(lines) fit_messages(paste(lines, collapse = "\n"))
      )
      shiny::incProgress(0.7, detail = "Fit call returned")
    })
    if (inherits(result$value, "error")) {
      fit_status_value("Fit failed.")
      fit_messages(paste(c(result$messages, conditionMessage(result$value)), collapse = "\n"))
      fit_warnings(if (length(result$warnings)) paste(result$warnings, collapse = "\n") else "No warnings.")
      shiny::showNotification(conditionMessage(result$value), type = "error")
    } else {
      fit_messages(if (length(result$messages)) paste(result$messages, collapse = "\n") else "Fit complete.")
      fit_warnings(if (length(result$warnings)) paste(result$warnings, collapse = "\n") else "No warnings.")
      fit_succeeded(result$value)
    }
    fit_finished()
    return()
  }

  log_path <- ctgui_fit_log_path()
  waiting <- !is.null(worker$job)
  process <- tryCatch(ctgui_fit_process_start(worker, args, log_path), error = function(e) e)
  if (inherits(process, "error")) {
    fit_status_value("Fit failed.")
    fit_messages(conditionMessage(process))
    shiny::showNotification(conditionMessage(process), type = "error")
    fit_finished()
    return()
  }

  fit_log_path(log_path)
  fit_log_offset(0)
  # Julia builds and precompiles its engine the first time a given version is
  # used, which can take several minutes and prints almost nothing while it
  # happens. Without a word here the fit looks stalled at "Activating project".
  opening <- if (identical(args$backend, "julia")) {
    paste(
      "Starting a background fit with the Julia engine.",
      "The first fit after a ctsem or Julia update also precompiles the engine,",
      "which can take several minutes and prints little while it runs.",
      "Each new model shape is also compiled once, the first time this session",
      "fits it; generating from the fit, and later fits of the same shape, reuse it."
    )
  } else {
    "Starting a background fit..."
  }
  if (waiting) {
    opening <- c(opening, "Waiting for the background process to finish its current job.")
  }
  fit_log_state(ctgui_fit_log_state(opening))
  publish_fit_log()
  fit_process(process)
})

# supervise = TRUE covers the session dying unexpectedly. Closing a browser tab
# is not that, so a fit nobody is waiting for is stopped here rather than left
# to finish into a session that no longer exists, and the process goes with it.
session$onSessionEnded(function() {
  ctgui_worker_close(worker)
  unlink(c(
    shiny::isolate(fit_log_path()),
    shiny::isolate(gen_log_path()),
    shiny::isolate(julia_check_log())
  ))
})

# Polling is what turns the child's log file into a live message window. It
# runs only while a fit is running, so an idle session does no work.
shiny::observe({
  process <- fit_process()
  if (is.null(process)) return()
  shiny::invalidateLater(500)

  drain_fit_log()

  if (ctgui_job_running(process)) return()

  # The job has finished. Read whatever it wrote between the last poll and
  # finishing before reporting, so the reason for a failure is not lost.
  drain_fit_log()

  outcome <- ctgui_job_collect(process)
  fit_process(NULL)
  unlink(shiny::isolate(fit_log_path()))
  fit_log_path(NULL)

  if (identical(outcome$status, "cancelled")) {
    fit_status_value("Fit stopped.")
    shiny::showNotification("Fit stopped.", type = "warning")
  } else if (identical(outcome$status, "error")) {
    fit_status_value("Fit failed.")
    shiny::showNotification(conditionMessage(outcome$error), type = "error")
    fit_log_state(ctgui_fit_log_append(
      shiny::isolate(fit_log_state()),
      paste0("\n", conditionMessage(outcome$error), "\n")
    ))
    publish_fit_log()
  } else {
    fit_succeeded(outcome$value)
  }
  # The conditions themselves, rather than what the log reading picked out.
  if (length(outcome$warnings)) fit_warnings(paste(outcome$warnings, collapse = "\n"))
  fit_finished()
})

shiny::observeEvent(input$cancel_fit, {
  process <- fit_process()
  if (!ctgui_job_running(process)) {
    shiny::showNotification("No fit is running.", type = "warning")
    return()
  }
  fit_status_value("Stopping the fit...")
  ctgui_job_cancel(process)
})

selected_registry_fit <- function() {
  name <- input$active_fit_name
  if (is.null(name) || !nzchar(name) || !name %in% names(fit_registry())) return(NULL)
  name
}

shiny::observeEvent(input$rename_fit, {
  name <- selected_registry_fit()
  if (is.null(name)) {
    shiny::showNotification("No fit to rename", type = "error")
    return()
  }
  shiny::showModal(shiny::modalDialog(
    title = paste("Rename", name),
    shiny::textInput("rename_fit_name", "New name", value = name),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("confirm_rename_fit", "Rename", class = "btn-primary")
    )
  ))
})

shiny::observeEvent(input$confirm_rename_fit, {
  old <- selected_registry_fit()
  new <- trimws(input$rename_fit_name %||% "")
  if (is.null(old)) return()
  if (!nzchar(new)) {
    shiny::showNotification("Enter a fit name", type = "error")
    return()
  }
  if (!identical(new, old) && new %in% names(fit_registry())) {
    shiny::showNotification(paste("A fit named", new, "already exists"), type = "error")
    return()
  }
  registry <- fit_registry()
  names(registry)[names(registry) == old] <- new
  fit_registry(registry)
  specs <- fit_specs()
  names(specs)[names(specs) == old] <- new
  fit_specs(specs)
  update_fit_choices(selected = new)
  shiny::removeModal()
})

shiny::observeEvent(input$remove_fit, {
  name <- selected_registry_fit()
  if (is.null(name)) {
    shiny::showNotification("No fit to remove", type = "error")
    return()
  }
  shiny::showModal(shiny::modalDialog(
    title = paste("Remove", name),
    "The fit is dropped from this session. Save it as an RDS file first to keep it.",
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton("confirm_remove_fit", "Remove", class = "btn-danger")
    )
  ))
})

shiny::observeEvent(input$confirm_remove_fit, {
  name <- selected_registry_fit()
  shiny::removeModal()
  if (is.null(name)) return()
  registry <- fit_registry()
  removed <- registry[[name]]
  registry[[name]] <- NULL
  fit_registry(registry)
  specs <- fit_specs()
  specs[[name]] <- NULL
  fit_specs(specs)
  if (identical(current_fit(), removed)) {
    current_fit(NULL)
    clear_uncertainty_state()
    clear_diagnostics()
    fit_status_value(paste("Removed", name))
  }
  update_fit_choices(selected = if (length(registry)) names(registry)[length(registry)] else character())
})

shiny::observeEvent(input$active_fit_name, {
  registry <- fit_registry()
  selected <- input$active_fit_name
  if (!is.null(selected) && selected %in% names(registry)) {
    # Selecting the fit already in use, as keep_fit() and a rename do, keeps
    # its uncertainty and diagnostics.
    if (identical(current_fit(), registry[[selected]])) return()
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
    result <- run_on_fit("ctOptimUncertainty", function() list(
      fit = fit,
      uncertainty = input$fit_uncertainty_method,
      draws = ctgui_uncertainty_default_draws(input$fit_uncertainty_method),
      finishsamples = input$fit_uncertainty_samples,
      cores = input$fit_cores,
      control = uncertainty_control()
    ))
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
  if (ctgui_job_running(gen_process())) {
    shiny::showNotification("Generation is already running.", type = "warning")
    return()
  }
  # In the background, with a live log, like generation after a fit; in this
  # session only where there is no background process to be had.
  if (requireNamespace("callr", quietly = TRUE)) {
    start_generate_from_fit(fit)
    return()
  }
  diagnostics_status("Generating data from fit...")
  out <- NULL
  shiny::withProgress(message = "Generating from fit", value = 0.2, {
    out <- capture_conditions({
      args <- compact_args(list(
        fit = fit,
        nsamples = arg_field("fit_gen_samples", "help_fit_gen_nsamples"),
        fullposterior = input$fit_gen_fullposterior,
        cores = generate_from_fit_cores()
      ))
      args <- append_more_args(args, "generate_from_fit")
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
    out <- run_on_fit("ctFitCovCheck", function() {
      args <- list(
        fit = fit,
        cor = input$cov_cor,
        plot = FALSE,
        cores = 1
      )
      if (!is.null(lags)) args$lags <- lags
      append_more_args(args, "cov_check")
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
  call_args <- kalman_call_args()
  diagnostics_status("Running prediction plots with ctPredict...")
  out <- NULL
  shiny::withProgress(message = "Running ctPredict", value = 0.2, {
    out <- run_on_fit("ctPredict", function() {
      append_more_args(c(list(fit = fit, plot = FALSE), call_args), "kalman")
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
    out <- run_on_fit("ctPostPredPlots", function() append_more_args(list(fit = fit), "postpred"))
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
  call_args <- acf_call_args()
  residual_acf_log("Running ctACFresiduals...")
  out <- NULL
  shiny::withProgress(message = "Running residual ACF", value = 0.2, {
    out <- run_on_fit("ctACFresiduals", function() {
      append_more_args(c(list(fit = fit), call_args, list(plot = FALSE)), "residual_acf")
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
  call_args <- dynamics_call_args()
  dynamics_log("Running ctDiscretePars...")
  out <- NULL
  shiny::withProgress(message = "Plotting dynamics", value = 0.2, {
    out <- run_on_fit("ctDiscretePars", function() {
      append_more_args(c(list(fit = fit), call_args, list(plot = TRUE, cores = 1)), "dynamics")
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
  call_args <- tipred_call_args()
  tipred_effects_log("Running ctPredictTIP...")
  out <- NULL
  shiny::withProgress(message = "Running ctPredictTIP", value = 0.2, {
    out <- run_on_fit("ctPredictTIP", function() append_more_args(c(list(sf = fit), call_args), "tipred"))
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
  plot_result <- try(do.call(plot,
    append_more_args(c(list(out), kalman_plot_args()), "kalman", "plot")), silent = TRUE)
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
