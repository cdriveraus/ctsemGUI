# More arguments ---------------------------------------------------------------

# Each panel's calls into ctsem, and the arguments the GUI supplies itself.
# Every other argument of the function is offered under "More arguments", read
# from the installed ctsem, so an argument ctsem adds appears without a GUI
# change and one it drops disappears. The arguments a panel already shows are
# the ones its help catalog entries name. A panel can call more than one
# function: prediction plots call ctPredict, then plot its result.
ctgui_more_args_panels <- function() list(
  fit = list(call = list(topic = "ctFit",
    gui = c("datalong", "model", "fit", "plot", "backend", "cores", "ctstanmodel"))),
  generate_from_fit = list(call = list(topic = "ctGenerateFromFit", gui = c("fit", "cores"))),
  cov_check = list(call = list(topic = "ctFitCovCheck", gui = c("fit", "plot", "cores"))),
  kalman = list(
    call = list(topic = "ctPredict", gui = c("fit", "plot")),
    plot = list(topic = "plot.ctKalmanDF", gui = c("x", "plot"))
  ),
  postpred = list(call = list(topic = "ctPostPredPlots", gui = c("fit", "plot", "wait"))),
  # ctACFresiduals passes these on to ctACF, having set the data and its
  # id and time columns from the fit's residuals.
  residual_acf = list(call = list(topic = "ctACF", gui = c("dat", "idcol", "timecol", "plot"))),
  dynamics = list(call = list(topic = "ctDiscretePars", gui = c("fit", "plot", "cores"))),
  tipred = list(call = list(topic = "ctPredictTIP", gui = c("sf", "plot")))
)

ctgui_more_args_key <- function(panel, role, param) {
  paste(panel, role, gsub("[^A-Za-z0-9_]", "_", param), sep = "_")
}
ctgui_more_args_input_id <- function(panel, role, param) {
  paste0("more_", ctgui_more_args_key(panel, role, param))
}
ctgui_more_args_help_id <- function(panel, role, param) {
  paste0("help_more_", ctgui_more_args_key(panel, role, param))
}

# An argument's own help text, without the "name: " the reader prefixes.
ctgui_arg_help_body <- function(topic, param) {
  text <- tryCatch(ctgui_ctsem_help_text(topic, param), error = function(e) "")
  if (grepl("^(No ctsem help|No argument help|Could not render)", text)) return("")
  trimws(sub(paste0("^\\s*", gsub("([.])", "\\\\\\1", param), ":\\s*"), "", text))
}

# ctsem marks a deprecated argument in its help, "Deprecated. Use ...", which
# is the one place that says so for every function alike.
ctgui_arg_deprecated <- function(topic, param) {
  grepl("^Deprecated\\b", ctgui_arg_help_body(topic, param))
}

# The arguments a panel's role offers under More arguments.
ctgui_more_args_list <- function(panel, role, base_catalog) {
  call <- ctgui_more_args_panels()[[panel]][[role]]
  fun <- ctgui_ctsem_function(call$topic)
  if (is.null(fun)) return(character())
  shown <- unlist(lapply(base_catalog, function(help) {
    if (identical(help$topic, call$topic)) help$param
  }), use.names = FALSE)
  args <- setdiff(names(formals(fun)), c("...", call$gui, shown))
  args[!vapply(args, ctgui_arg_deprecated, logical(1L), topic = call$topic)]
}

# Help catalog entries for every More argument, so each has the same "?" as a
# main control, showing its ctsem help.
ctgui_more_args_help <- function(base_catalog) {
  entries <- list()
  for (panel in names(ctgui_more_args_panels())) {
    roles <- ctgui_more_args_panels()[[panel]]
    for (role in names(roles)) {
      topic <- roles[[role]]$topic
      for (param in ctgui_more_args_list(panel, role, base_catalog)) {
        body <- ctgui_arg_help_body(topic, param)
        tooltip <- if (nzchar(body)) {
          if (nchar(body) > 240L) paste0(substr(body, 1L, 237L), "...") else body
        } else paste0(topic, " argument: ", param)
        entries[[ctgui_more_args_help_id(panel, role, param)]] <- list(
          topic = topic, param = param, tooltip = tooltip, more = TRUE,
          panel = panel, role = role)
      }
    }
  }
  entries
}

# The entries of one panel, in the function's own argument order.
ctgui_more_args_entries <- function(help_catalog, panel) {
  Filter(function(help) isTRUE(help$more) && identical(help$panel, panel), help_catalog)
}

# The More arguments section of a panel: one field per argument, built as the
# main controls are, so each shows ctsem's default and passes nothing until
# changed. `function_help` names the catalog entry for the whole function.
ctgui_more_args_ui <- function(panel, help_catalog, function_help = NULL) {
  entries <- ctgui_more_args_entries(help_catalog, panel)
  if (!length(entries)) return(NULL)
  roles <- names(ctgui_more_args_panels()[[panel]])
  field <- function(help_id) {
    help <- help_catalog[[help_id]]
    id <- ctgui_more_args_input_id(panel, help$role, help$param)
    label <- ctgui_arg_label(help_catalog, help$param, help_id)
    arg <- ctgui_ctsem_arg(help$topic, help$param)
    input <- if (length(arg$choices) || isTRUE(arg$logical)) {
      ctgui_arg_select_input(id, label, help_catalog, help_id)
    } else {
      ctgui_arg_text_input(id, label, help_catalog, help_id)
    }
    shiny::div(class = "ctgui-more-arg",
      `data-search` = tolower(paste(help$param, help$tooltip)), input)
  }
  groups <- lapply(roles, function(role) {
    ids <- names(entries)[vapply(entries, function(help) identical(help$role, role), logical(1L))]
    if (!length(ids)) return(NULL)
    shiny::tagList(
      if (length(roles) > 1L) shiny::tags$h5(class = "ctgui-more-heading", entries[[ids[1L]]]$topic),
      shiny::div(class = "control-grid", lapply(ids, field))
    )
  })
  shiny::tags$details(
    class = "ctgui-disclosure ctgui-more-args",
    shiny::tags$summary(
      paste0("More arguments (", length(entries), ")"),
      if (!is.null(function_help) && !is.null(help_catalog[[function_help]]))
        ctgui_help_link(help_catalog, function_help)
    ),
    shiny::tags$p(class = "help-note",
      "Read from the installed ctsem. A field left at its default is not passed; ",
      "other entries are read as R code, so text needs quotes only where it could be read as a name."),
    if (length(entries) > 8L) shiny::tags$input(
      type = "search", class = "form-control ctgui-more-filter",
      placeholder = "Filter arguments", `aria-label` = "Filter arguments"),
    groups
  )
}

# The values a panel's role will pass: NULL fields, those left blank or at
# ctsem's default, are dropped.
ctgui_more_args_values <- function(input, help_catalog, panel, role = "call") {
  entries <- Filter(function(help) identical(help$role, role),
    ctgui_more_args_entries(help_catalog, panel))
  values <- lapply(entries, function(help) {
    ctgui_arg_value(input[[ctgui_more_args_input_id(panel, role, help$param)]],
      ctgui_ctsem_arg(help$topic, help$param))
  })
  names(values) <- vapply(entries, function(help) help$param, character(1L))
  values[!vapply(values, is.null, logical(1L))]
}
