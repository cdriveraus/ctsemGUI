# Fit, diagnostics, and plot services ---------------------------------------

# Keep condition capture independent of Shiny so actions can be characterized
# without starting an application.  Callers may pass a function or an
# expression.
#
# Messages are rendered the way the background fit's log is, by
# `ctgui_fit_log_append()`. ctsem reports progress by rewriting one line with a
# carriage return and no newline -- ctACFresiduals sends "\r Y1 Y2  45%" once
# per percent -- so taking each message as a line of its own turned one
# progress counter into a hundred lines.
ctgui_run_result <- function(action, progress_callback = NULL) {
  if (!is.function(action)) {
    expression <- substitute(action)
    caller <- parent.frame()
    action <- function() eval(expression, caller)
  }
  log <- ctgui_fit_log_state()
  warnings <- character()
  shown <- function() {
    lines <- ctgui_fit_log_lines(log)
    lines[nzchar(trimws(lines))]
  }
  value <- withCallingHandlers(
    tryCatch(action(), error = function(error) error),
    message = function(message) {
      log <<- ctgui_fit_log_append(log, conditionMessage(message))
      if (!is.null(progress_callback)) progress_callback(shown())
      invokeRestart("muffleMessage")
    },
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = shown(), warnings = unique(warnings))
}

ctgui_ctsem_run <- function(name, args = list(), progress_callback = NULL) {
  if (!is.list(args)) stop("args must be a list", call. = FALSE)
  ctgui_run_result(function() ctgui_ctsem_call(name, .args = args), progress_callback)
}

ctgui_result_text <- function(result, success, failure = NULL) {
  if (inherits(result$value, "error")) {
    return(paste(c(failure %||% "Action failed.", result$messages,
      conditionMessage(result$value), result$warnings), collapse = "\n"))
  }
  paste(c(success, result$messages, result$warnings), collapse = "\n")
}

# A ctsem plotting helper can return one plot, an unnamed list, a nested list,
# a recorded base plot, or a plotting function.  Normalize those variants once
# before server code assigns dynamic Shiny outputs.
ctgui_plot_collection <- function(x, prefix = character()) {
  is_plot <- inherits(x, "ggplot") || inherits(x, "recordedplot") || is.function(x)
  if (is_plot) {
    label <- paste(prefix[nzchar(prefix)], collapse = " / ")
    if (!nzchar(label)) label <- "Plot"
    return(stats::setNames(list(x), label))
  }
  if (is.null(x) || !is.list(x) || !length(x)) return(list())
  labels <- names(x)
  if (is.null(labels)) labels <- rep("", length(x))
  out <- list()
  for (index in seq_along(x)) {
    label <- labels[[index]]
    if (is.null(label) || !nzchar(label)) label <- paste("Plot", index)
    out <- c(out, ctgui_plot_collection(x[[index]], c(prefix, label)))
  }
  if (anyDuplicated(names(out))) names(out) <- make.unique(names(out), sep = " #")
  out
}

ctgui_draw_plot <- function(plot) {
  if (is.function(plot)) plot <- plot()
  if (inherits(plot, "recordedplot")) {
    grDevices::replayPlot(plot)
  } else if (!is.null(plot)) {
    print(plot)
  }
  invisible(plot)
}

ctgui_fit_comparison_stats <- function(fit) {
  statistics <- ctgui_ctsem_fit_statistics(fit)
  loglik <- statistics$loglik
  logposterior <- statistics$logposterior
  npars <- statistics$npars
  nobs <- statistics$nobs
  aic <- if (!is.na(loglik) && !is.na(npars)) 2 * npars - 2 * loglik else NA_real_
  bic <- if (!is.na(loglik) && !is.na(npars) && !is.na(nobs) && nobs > 0) log(nobs) * npars - 2 * loglik else NA_real_
  list(loglik = loglik, logposterior = logposterior, npars = npars, nobs = nobs,
    aic = aic, bic = bic,
    note = if (is.na(loglik)) "Likelihood unavailable in this fit object" else if (is.na(bic)) "BIC unavailable because observation count was not found" else "")
}
