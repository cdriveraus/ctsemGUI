# ctsem compatibility boundary ----------------------------------------------

ctgui_ctsem_capabilities <- function() {
  required <- c("ctModel", "ctModelMatrices")
  optional <- c("ctFit", "ctModelLatex", "ctGenerate", "ctGenerateFromFit", "ctOptimUncertainty",
    "ctSummaryMatrices", "ctFitCovCheck", "ctFitCovCheckPlot", "ctPredict",
    "ctPredictTIP", "ctPostPredPlots", "ctACFresiduals", "ctDiscretePars", "plotctACF")
  installed <- requireNamespace("ctsem", quietly = TRUE)
  exports <- if (installed) getNamespaceExports("ctsem") else character()
  list(installed = installed,
    version = if (installed) as.character(utils::packageVersion("ctsem")) else NA_character_,
    required = stats::setNames(required %in% exports, required),
    optional = stats::setNames(optional %in% exports, optional))
}

ctgui_has_ctsem <- function() {
  caps <- ctgui_ctsem_capabilities()
  isTRUE(caps$installed) && all(caps$required)
}

ctgui_ctsem_call <- function(name, ..., .args = NULL) {
  caps <- ctgui_ctsem_capabilities()
  available <- isTRUE(caps$installed) && name %in% getNamespaceExports("ctsem")
  if (!available) stop("The loaded ctsem version does not provide ", name, call. = FALSE)
  args <- c(list(...), .args %||% list())
  do.call(getExportedValue("ctsem", name), args)
}

ctgui_ctsem_new_model <- function(args) ctgui_ctsem_call("ctModel", .args = args)

ctgui_ctsem_matrices <- function(model) ctgui_ctsem_call("ctModelMatrices", model)

ctgui_ctsem_object_path <- function(object, path, default = NULL) {
  if (!length(path)) return(if (is.null(object)) default else object)
  value <- object
  for (name in path) {
    if (is.null(value)) return(default)
    value <- tryCatch({
      if (is.environment(value)) {
        if (!exists(name, envir = value, inherits = FALSE)) return(default)
        get(name, envir = value, inherits = FALSE)
      } else if (is.list(value) || is.pairlist(value)) {
        if (!name %in% names(value)) return(default)
        value[[name]]
      } else if (isS4(value) && name %in% methods::slotNames(value)) {
        methods::slot(value, name)
      } else {
        return(default)
      }
    }, error = function(error) default)
  }
  if (is.null(value)) default else value
}

ctgui_ctsem_fit_value <- function(fit, paths, default = NULL) {
  if (is.character(paths) && is.null(dim(paths))) paths <- list(paths)
  for (path in paths) {
    value <- ctgui_ctsem_object_path(fit, path, default = NULL)
    if (!is.null(value)) return(value)
  }
  default
}

ctgui_ctsem_fit_field <- function(fit, field, default = NULL) {
  ctgui_ctsem_fit_value(fit, list(field), default)
}

ctgui_ctsem_is_fit <- function(x) {
  if (is.null(x)) return(FALSE)
  if (inherits(x, c("ctStanFit", "ctFit"))) return(TRUE)
  !is.null(ctgui_ctsem_fit_value(x, list(
    c("stanfit"), c("model"), c("ctstanmodel"), c("generated")
  )))
}

ctgui_ctsem_fit_model <- function(fit, default = NULL) {
  ctgui_ctsem_fit_value(fit, list(
    c("model"), c("ctstanmodel"), c("stanmodel"),
    c("stanfit", "model"), c("stanfit", "ctstanmodel")
  ), default)
}

ctgui_ctsem_fit_generated <- function(fit, default = NULL) {
  ctgui_ctsem_fit_value(fit, list(
    c("generated"), c("stanfit", "generated"), c("generateddata")
  ), default)
}

ctgui_ctsem_fit_uncertainty <- function(fit, default = NULL) {
  ctgui_ctsem_fit_value(fit, list(
    c("stanfit", "uncertainty"), c("uncertainty")
  ), default)
}

ctgui_ctsem_fit_rawposterior <- function(fit, default = NULL) {
  ctgui_ctsem_fit_value(fit, list(
    c("stanfit", "rawposterior"), c("rawposterior")
  ), default)
}

ctgui_ctsem_fit_backend <- function(fit, default = NULL) {
  ctgui_ctsem_fit_value(fit, list(
    c("stanfit", "stanfit"), c("stanfit")
  ), default)
}

ctgui_ctsem_fit_is_sampled <- function(fit) {
  backend <- ctgui_ctsem_fit_backend(fit)
  sim <- ctgui_ctsem_object_path(backend, "sim", default = NULL)
  length(sim) > 0L
}

ctgui_ctsem_fit_missing_components <- function(fit,
    required = c("stanfit", "stanmodel", "standata")) {
  required[!vapply(required, function(name) {
    !is.null(ctgui_ctsem_fit_value(fit, list(
      c(name), c("stanfit", name)
    )))
  }, logical(1L))]
}

ctgui_ctsem_numeric_scalar <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  if (!length(out) || all(is.na(out))) return(NA_real_)
  out[1L]
}

ctgui_ctsem_fit_statistics <- function(fit) {
  loglik <- ctgui_ctsem_numeric_scalar(ctgui_ctsem_fit_value(fit, list(
    c("stanfit", "transformedparsfull", "ll"),
    c("transformedparsfull", "ll"),
    c("fitStatistics", "loglik"),
    c("loglik")
  ), NA_real_))
  logposterior <- ctgui_ctsem_numeric_scalar(ctgui_ctsem_fit_value(fit, list(
    c("stanfit", "optimfit", "value"),
    c("optimfit", "value"),
    c("fitStatistics", "logposterior"),
    c("logposterior")
  ), NA_real_))
  rawest <- ctgui_ctsem_fit_value(fit, list(
    c("stanfit", "rawest"), c("rawest")
  ))
  npars <- if (is.null(rawest)) {
    suppressWarnings(as.integer(ctgui_ctsem_numeric_scalar(ctgui_ctsem_fit_value(
      fit, list(c("fitStatistics", "npars"), c("npars")), NA_real_))))
  } else {
    as.integer(length(rawest))
  }
  llrow <- ctgui_ctsem_fit_value(fit, list(
    c("stanfit", "transformedparsfull", "llrow"),
    c("transformedparsfull", "llrow")
  ))
  nobs <- if (!is.null(llrow) && length(dim(llrow)) >= 2L) {
    as.integer(ncol(llrow))
  } else {
    suppressWarnings(as.integer(ctgui_ctsem_numeric_scalar(ctgui_ctsem_fit_value(
      fit, list(c("fitStatistics", "nobs"), c("nobs")), NA_real_))))
  }

  if (is.na(loglik)) {
    summary_fit <- tryCatch(summary(fit), error = function(error) NULL)
    if (!is.null(summary_fit)) {
      loglik <- ctgui_ctsem_numeric_scalar(ctgui_ctsem_object_path(summary_fit, "loglik"))
      logposterior <- ctgui_ctsem_numeric_scalar(
        ctgui_ctsem_object_path(summary_fit, "logposterior"))
      summary_npars <- suppressWarnings(as.integer(ctgui_ctsem_numeric_scalar(
        ctgui_ctsem_object_path(summary_fit, "npars"))))
      summary_nobs <- suppressWarnings(as.integer(ctgui_ctsem_numeric_scalar(
        ctgui_ctsem_object_path(summary_fit, "nobs"))))
      if (is.na(npars)) npars <- summary_npars
      if (is.na(nobs)) nobs <- summary_nobs
    }
  }
  list(loglik = loglik, logposterior = logposterior, npars = npars, nobs = nobs)
}

ctgui_ctsem_dormant_t0var <- function(spec, build) {
  t0var <- spec$matrices[["T0VAR"]]
  saved <- if (is.null(t0var)) NULL else matrix(as.character(t0var),
    nrow = nrow(t0var), ncol = ncol(t0var), dimnames = dimnames(t0var))
  model <- build()
  if (!is.null(saved)) spec$matrices[["T0VAR"]] <- saved
  list(spec = spec, model = model)
}
