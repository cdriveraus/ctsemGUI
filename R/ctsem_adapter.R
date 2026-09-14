# ctsem compatibility boundary ----------------------------------------------

ctgui_ctsem_capabilities <- function() {
  required <- c("ctModel", "ctModelMatrices")
  optional <- c("ctFit", "ctModelLatex", "ctGenerate", "ctGenerateFromFit", "ctOptimUncertainty",
    "ctSummaryMatrices", "ctFitCovCheck", "ctFitCovCheckPlot", "ctPredict",
    "ctPredictTIP", "ctPostPredPlots", "ctACFresiduals", "ctDiscretePars", "plotctACF",
    "ctJuliaStatus", "ctJuliaInstall")
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

# What the loaded ctModel() actually takes. Asked of the installed ctsem
# rather than listed here, because the signature differs across the versions
# this GUI supports and both directions have bitten: ctsem 3.12 reports a
# RAWPOPVAR matrix that 3.11 has no argument for, and 3.11 has no argument
# for the ordinal and censored measurement 3.12 added. NULL when ctsem is not
# installed, which callers read as "unknown" rather than "none".
ctgui_ctmodel_formals <- function() {
  if (!ctgui_has_ctsem()) return(NULL)
  tryCatch(names(formals(getExportedValue("ctsem", "ctModel"))),
    error = function(e) NULL)
}

# The installed ctsem's version, for the measurement types whose availability
# cannot be read off a signature. NULL when ctsem is not installed.
ctgui_ctsem_version <- function() {
  if (!requireNamespace("ctsem", quietly = TRUE)) return(NULL)
  tryCatch(utils::packageVersion("ctsem"), error = function(e) NULL)
}

# A `<TI>_effect` column of ctsem's `pars` says four different things, and
# which of them it can say depends on the version. ctsem 3.11 held a logical.
# ctsem 3.12 holds a character spec, because "free" and "fixed at 1" are
# different statements and both have to be sayable:
#
#   FALSE / 'FALSE' / ''   no effect
#   TRUE  / 'TRUE'         a free effect, named automatically
#   '4.3'                  fixed at that value
#   'myeffect'             a free effect with a name, so another parameter can
#                          be constrained to the same one
#
# as.logical() reads the last two as NA, which every caller then treats as "no
# effect" -- so a model using either would load into the GUI with the effect
# quietly gone. Asking whether an effect is active covers both versions.
ctgui_tipred_effect_active <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  spec <- trimws(as.character(unlist(x)))
  spec[is.na(spec)] <- "FALSE"
  active <- !spec %in% c("FALSE", "F", "", "NA", "0")
  numeric <- suppressWarnings(as.numeric(spec))
  active & !(is.finite(numeric) & numeric == 0)
}

# Whether the effect is one this GUI can write back unchanged. Its label
# grammar lists predictor names and nothing else, so an effect that is fixed to
# a value or carries a name survives loading but would be re-emitted as an
# ordinary free one.
ctgui_tipred_effect_is_plain <- function(x) {
  if (is.logical(x)) return(rep(TRUE, length(x)))
  spec <- trimws(as.character(unlist(x)))
  spec[is.na(spec)] <- "FALSE"
  toupper(spec) %in% c("TRUE", "FALSE", "F", "T", "", "NA")
}

# Whether a manifest intercept reaches the link for a non-Gaussian variable.
# ctsem 3.12 forms the linear predictor as MANIFESTMEANS + LAMBDA * state and
# hands that to the link, so MANIFESTMEANS sets the level directly and the
# warning about fixed CINT entries went with it. On 3.11 the link applies to
# the latent alone and ctModel() still warns.
ctgui_ctsem_manifest_means_in_link <- function() {
  version <- ctgui_ctsem_version()
  if (is.null(version)) return(TRUE)
  version >= package_version("3.12.0")
}

ctgui_ctsem_call <- function(name, ..., .args = NULL) {
  caps <- ctgui_ctsem_capabilities()
  available <- isTRUE(caps$installed) && name %in% getNamespaceExports("ctsem")
  if (!available) stop("The loaded ctsem version does not provide ", name, call. = FALSE)
  args <- c(list(...), .args %||% list())
  # Model construction is an internal GUI operation; ctModel's console
  # diagnostics belong in the app's status surfaces, not the R console.
  if (identical(name, "ctModel")) {
    args$silent <- TRUE
    return(suppressMessages(do.call(getExportedValue("ctsem", name), args)))
  }
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
    c("modelbase"), c("model"), c("ctstanmodelbase"), c("ctstanmodel"),
    c("stanmodel"), c("stanfit", "modelbase"), c("stanfit", "model"),
    c("stanfit", "ctstanmodelbase"), c("stanfit", "ctstanmodel")
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

# The engine's own name for itself, for display. A Julia fit records it
# directly; a Stan fit is identified by its class.
ctgui_ctsem_fit_backend_name <- function(fit) {
  engine <- ctgui_ctsem_fit_engine(fit)
  if (is.na(engine)) return("unknown")
  recorded <- ctgui_ctsem_fit_value(fit, list(c("backend")))
  if (is.character(recorded) && length(recorded) == 1L && nzchar(recorded)) return(recorded)
  engine
}

# Which engine produced this fit. ctsem can fit through Stan or through Julia,
# and the two produce differently shaped objects, so anything that reaches into
# a fit has to know which one it has rather than assuming Stan.
ctgui_ctsem_fit_engine <- function(fit) {
  if (is.null(fit)) return(NA_character_)
  if (inherits(fit, "ctJuliaFit")) return("julia")
  if (inherits(fit, "ctStanFit")) return("stan")
  NA_character_
}

ctgui_ctsem_fit_is_sampled <- function(fit) {
  backend <- ctgui_ctsem_fit_backend(fit)
  sim <- ctgui_ctsem_object_path(backend, "sim", default = NULL)
  length(sim) > 0L
}

# stanfit, stanmodel and standata exist only on a Stan fit. Asking a Julia fit
# for them reports it as broken when it is merely a different shape.
ctgui_ctsem_fit_required_components <- function(fit) {
  if (identical(ctgui_ctsem_fit_engine(fit), "julia")) return(character())
  c("stanfit", "stanmodel", "standata")
}

ctgui_ctsem_fit_missing_components <- function(fit,
    required = ctgui_ctsem_fit_required_components(fit)) {
  if (!length(required)) return(character())
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
  if (is.na(nobs) && identical(ctgui_ctsem_fit_engine(fit), "julia")) {
    # A Julia fit keeps no llrow, but it does carry the data it was fitted to.
    # Its row count matches what the Stan path reports for the same fit, which
    # is what makes BIC comparable across the two engines.
    fitted_data <- ctgui_ctsem_fit_value(fit, list(c("data")))
    if (is.data.frame(fitted_data)) nobs <- as.integer(nrow(fitted_data))
  }

  if (is.na(loglik)) {
    summary_fit <- tryCatch(summary(fit), error = function(error) NULL)
    if (!is.null(summary_fit)) {
      summary_loglik <- ctgui_ctsem_numeric_scalar(
        ctgui_ctsem_object_path(summary_fit, "loglik"))
      summary_logposterior <- ctgui_ctsem_numeric_scalar(
        ctgui_ctsem_object_path(summary_fit, "logposterior"))
      summary_npars <- suppressWarnings(as.integer(ctgui_ctsem_numeric_scalar(
        ctgui_ctsem_object_path(summary_fit, "npars"))))
      summary_nobs <- suppressWarnings(as.integer(ctgui_ctsem_numeric_scalar(
        ctgui_ctsem_object_path(summary_fit, "nobs"))))
      if (is.na(loglik)) loglik <- summary_loglik
      if (is.na(logposterior)) logposterior <- summary_logposterior
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

ctgui_julia_cache <- new.env(parent = emptyenv())

# Whether ctsem can fit through Julia here.
#
# Answering starts Julia, which costs seconds and may build the engine, so the
# application asks in a background process as soon as it loads and carries on
# without the answer. By the time anyone reaches the Fit panel it has arrived,
# and asking early also warms the engine cache before the first fit needs it.
# The answer is remembered so a second session in the same R process is free.
ctgui_julia_status <- function(refresh = FALSE) {
  if (!isTRUE(refresh) && !is.null(ctgui_julia_cache$status)) return(ctgui_julia_cache$status)
  ctgui_julia_remember(ctgui_julia_status_uncached())
}

ctgui_julia_known <- function() ctgui_julia_cache$status

ctgui_julia_remember <- function(status) {
  ctgui_julia_cache$status <- status
  status
}

# The answer is not in yet. Distinguished from a negative answer so the panel
# can say it is still looking rather than that Julia is unavailable.
ctgui_julia_pending <- function() {
  list(
    available = NA,
    pending = TRUE,
    message = "Looking for the Julia engine..."
  )
}

ctgui_julia_is_pending <- function(status) {
  isTRUE(status$pending) || (length(status$available) == 1L && is.na(status$available))
}

# Runs in the child. Only ctsem is needed, so the worker does not depend on
# ctsemGUI being loadable there.
ctgui_julia_check_worker <- function(args) {
  if (!requireNamespace("ctsem", quietly = TRUE)) return(NULL)
  if (!"ctJuliaStatus" %in% getNamespaceExports("ctsem")) return(NULL)
  # Looked up rather than called as ctsem::ctJuliaStatus, because a ctsem
  # without the Julia backend is a supported configuration and R CMD check
  # reads a :: call as a hard requirement on the installed version.
  tryCatch(getExportedValue("ctsem", "ctJuliaStatus")(), error = function(e) NULL)
}

# Turn whatever the child reported into the shape the panel expects. A child
# that failed or returned nothing means Julia is not usable here, which is a
# real answer rather than a reason to keep waiting.
ctgui_julia_status_from_check <- function(status) {
  if (is.null(status) || !is.list(status) || !isTRUE(status$available)) {
    return(list(
      available = FALSE,
      message = "Julia is not set up. Run ctsem::ctJuliaInstall() to use it."
    ))
  }
  list(
    available = TRUE,
    version = status$julia %||% "",
    threads = status$threads %||% NA_integer_,
    message = paste0("Julia ", status$julia %||% "", " is available.")
  )
}

ctgui_julia_status_uncached <- function() {
  if (!ctgui_has_ctsem()) return(list(available = FALSE, message = "ctsem is not installed."))
  if (!isTRUE(ctgui_ctsem_capabilities()$optional[["ctJuliaStatus"]])) {
    return(list(available = FALSE, message = "This ctsem version has no Julia backend."))
  }
  status <- tryCatch(ctgui_ctsem_call("ctJuliaStatus"), error = function(e) e)
  if (inherits(status, "error")) {
    return(list(available = FALSE, message = paste("Julia is unavailable:", conditionMessage(status))))
  }
  if (!isTRUE(status$available)) {
    return(list(
      available = FALSE,
      message = "Julia is not set up. Run ctsem::ctJuliaInstall() to use it."
    ))
  }
  list(
    available = TRUE,
    version = status$julia %||% "",
    threads = status$threads %||% NA_integer_,
    message = paste0("Julia ", status$julia %||% "", " is available.")
  )
}

ctgui_backend_choices <- function(julia = ctgui_julia_status()) {
  if (isTRUE(julia$available)) c("Julia" = "julia", "Stan" = "stan") else c("Stan" = "stan")
}

ctgui_default_backend <- function(julia = ctgui_julia_status()) {
  if (isTRUE(julia$available)) "julia" else "stan"
}
