# Fit, diagnostics, and plot services ---------------------------------------

# Keep condition capture independent of Shiny so actions can be characterized
# without starting an application.  Callers may pass a function or an
# expression; the latter preserves the historical `capture_conditions()` API.
ctgui_run_result <- function(action, progress_callback = NULL) {
  if (!is.function(action)) {
    expression <- substitute(action)
    action <- function() eval(expression, parent.frame())
  }
  messages <- character()
  warnings <- character()
  append_message <- function(condition) {
    line <- trimws(conditionMessage(condition))
    messages <<- c(messages, line)
    if (!is.null(progress_callback)) progress_callback(messages)
  }
  value <- withCallingHandlers(
    tryCatch(action(), error = function(error) error),
    message = function(message) {
      append_message(message)
      invokeRestart("muffleMessage")
    },
    warning = function(warning) {
      warnings <<- c(warnings, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = unique(messages), warnings = unique(warnings))
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
  numeric_scalar <- function(x) {
    out <- suppressWarnings(as.numeric(x))
    if (!length(out) || all(is.na(out))) return(NA_real_)
    out[1L]
  }
  loglik <- tryCatch(numeric_scalar(fit$stanfit$transformedparsfull$ll), error = function(e) NA_real_)
  logposterior <- tryCatch(numeric_scalar(fit$stanfit$optimfit$value), error = function(e) NA_real_)
  npars <- tryCatch(length(fit$stanfit$rawest), error = function(e) NA_integer_)
  nobs <- tryCatch(length(fit$stanfit$transformedparsfull$llrow[1, ]), error = function(e) NA_integer_)
  if (is.na(loglik)) {
    summary_fit <- tryCatch(summary(fit), error = function(e) NULL)
    if (!is.null(summary_fit)) {
      loglik <- numeric_scalar(summary_fit$loglik)
      logposterior <- numeric_scalar(summary_fit$logposterior)
      npars <- suppressWarnings(as.integer(numeric_scalar(summary_fit$npars)))
    }
  }
  aic <- if (!is.na(loglik) && !is.na(npars)) 2 * npars - 2 * loglik else NA_real_
  bic <- if (!is.na(loglik) && !is.na(npars) && !is.na(nobs) && nobs > 0) log(nobs) * npars - 2 * loglik else NA_real_
  list(loglik = loglik, logposterior = logposterior, npars = npars, nobs = nobs,
    aic = aic, bic = bic,
    note = if (is.na(loglik)) "Likelihood unavailable in this fit object" else if (is.na(bic)) "BIC unavailable because observation count was not found" else "")
}
