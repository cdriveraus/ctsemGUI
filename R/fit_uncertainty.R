ctgui_uncertainty_method_choices <- function() {
  c(
    "Hessian approximation" = "hessian",
    "Local quadratic surrogate" = "surrogate",
    "Importance sampling" = "is",
    "Score bootstrap" = "bootstrap",
    "Full subject bootstrap" = "fullbootstrap",
    "Sandwich covariance" = "sandwich",
    "OPG covariance" = "opg"
  )
}

ctgui_uncertainty_draw_choices <- function(method = "hessian") {
  method <- match.arg(method, unname(ctgui_uncertainty_method_choices()))
  if (identical(method, "is")) return(c("Importance sampling" = "imis"))
  if (method %in% c("bootstrap", "fullbootstrap")) {
    return(c("Automatic" = "auto", "Empirical draws" = "empirical",
      "Normal approximation" = "normal", "Importance sampling" = "imis"))
  }
  c("Automatic" = "auto", "Normal approximation" = "normal",
    "Importance sampling" = "imis")
}

ctgui_uncertainty_control <- function(ridge = 1e-8, hessian_step = 1e-3,
  surrogate_npoints = NA, surrogate_scale = .5, surrogate_profile = TRUE,
  surrogate_profile_target_drop = NA, surrogate_profile_max_step = 64,
  imis_max_iter = 50, imis_scale_init = 1.1, imis_tail_scale = 1.1,
  is_ess = 100, is_itersize = 1000, bootstrap_fit_cores = 1,
  bootstrap_tol = 1e-5) {
  optional_number <- function(x) {
    x <- suppressWarnings(as.numeric(x)[1])
    if (is.na(x)) NULL else x
  }
  list(
    ridge = as.numeric(ridge)[1],
    hessianStep = as.numeric(hessian_step)[1],
    surrogateNpoints = optional_number(surrogate_npoints),
    surrogateScale = as.numeric(surrogate_scale)[1],
    surrogateProfile = isTRUE(surrogate_profile),
    surrogateProfileTargetDrop = optional_number(surrogate_profile_target_drop),
    surrogateProfileMaxStep = as.numeric(surrogate_profile_max_step)[1],
    imisMaxIter = as.integer(imis_max_iter)[1],
    imisScaleInit = as.numeric(imis_scale_init)[1],
    imisTailScale = as.numeric(imis_tail_scale)[1],
    isESS = as.numeric(is_ess)[1],
    isitersize = as.integer(is_itersize)[1],
    bootstrapFitCores = as.integer(bootstrap_fit_cores)[1],
    bootstrapTol = as.numeric(bootstrap_tol)[1]
  )
}

ctgui_uncertainty_optimcontrol <- function(method = "hessian", draws = "auto",
  finishsamples = 1000, control = list()) {
  method <- match.arg(method, unname(ctgui_uncertainty_method_choices()))
  allowed_draws <- unname(ctgui_uncertainty_draw_choices(method))
  if (!draws %in% allowed_draws) draws <- allowed_draws[1L]
  list(
    uncertainty = method,
    uncertaintyDraws = draws,
    finishsamples = as.integer(finishsamples)[1],
    uncertaintyControl = control
  )
}

ctgui_uncertainty_merge_optimcontrol <- function(gui_control, supplied = NULL) {
  if (is.null(supplied)) return(gui_control)
  if (!is.list(supplied)) stop("optimcontrol in Extra ctFit arguments must be a list", call. = FALSE)
  utils::modifyList(gui_control, supplied)
}

ctgui_optim_uncertainty_eligibility <- function(fit) {
  if (is.null(fit)) return(list(ok = FALSE, message = "No fit is available."))
  if (!inherits(fit, "ctStanFit")) {
    return(list(ok = FALSE, message = "Uncertainty recomputation requires an optimized ctStanFit object."))
  }
  stanfit <- tryCatch(fit$stanfit$stanfit, error = function(e) NULL)
  sim <- tryCatch(stanfit@sim, error = function(e) NULL)
  if (length(sim) > 0L) {
    return(list(ok = FALSE, message = "This is a sampled fit. ctOptimUncertainty applies only to optimized fits."))
  }
  required <- c("stanfit", "stanmodel", "standata")
  missing <- required[!vapply(required, function(name) !is.null(fit[[name]]), logical(1))]
  if (length(missing)) {
    return(list(ok = FALSE, message = paste("Fit is missing", paste(missing, collapse = ", "), "required for optimized uncertainty.")))
  }
  list(ok = TRUE, message = "Optimized-fit uncertainty can be recomputed.")
}

ctgui_uncertainty_summary <- function(fit) {
  uncertainty <- tryCatch(fit$stanfit$uncertainty, error = function(e) NULL)
  if (is.null(uncertainty)) return("No optimized uncertainty information is stored in this fit.")
  settings <- uncertainty$settings %||% list()
  lines <- c(
    paste("Method:", uncertainty$method %||% settings$method %||% "unknown"),
    paste("Draws:", uncertainty$draws %||% settings$draws %||% "unknown")
  )
  nsamples <- tryCatch(nrow(fit$stanfit$rawposterior), error = function(e) NA_integer_)
  if (!is.na(nsamples)) lines <- c(lines, paste("Approximate draws:", nsamples))
  importance <- uncertainty$details$importance_sampling
  if (!is.null(importance)) {
    lines <- c(lines, paste("Importance-sampling ESS:", format(importance$ess, digits = 5)),
      paste("Importance-sampling df used:", format(importance$df_used, digits = 5)))
  }
  fullbootstrap <- uncertainty$details$fullbootstrap
  if (!is.null(fullbootstrap)) {
    lines <- c(lines, paste("Full-bootstrap refits retained:", length(fullbootstrap$sampledSubjects %||% list())),
      paste("Full-bootstrap workers:", fullbootstrap$outerCores %||% "unknown"))
  }
  surrogate <- uncertainty$details$surrogate
  if (!is.null(surrogate)) {
    lines <- c(lines, paste("Surrogate points used:", surrogate$diagnostics$nUsed %||% surrogate$nfinite %||% "unknown"),
      paste("Surrogate profile directions adjusted:", surrogate$profile$nAdjusted %||% 0L))
  }
  covariance <- uncertainty$details$covariance %||% uncertainty$details$proposal_covariance
  if (!is.null(covariance)) {
    lines <- c(lines, paste("Covariance construction:", covariance$method %||% "unknown"))
    repairs <- c(
      if (isTRUE(covariance$usedNearPD)) "nearPD",
      if (isTRUE(covariance$infoRidgeApplied) || isTRUE(covariance$covRidgeApplied)) "ridge",
      if (isTRUE(covariance$usedGinv)) "MASS::ginv"
    )
    lines <- c(lines, if (length(repairs)) paste("Numerical repair used:", paste(repairs, collapse = ", ")) else "Numerical repair used: none")
  }
  paste(lines, collapse = "\n")
}
