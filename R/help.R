ctgui_help_catalog <- function() {
  list(
    help_gui_time_model = list(title = "Time model", text = "Choose continuous time for irregular intervals or dynamics between observations; choose discrete time when the observation step is the time unit."),
    help_gui_logdtsd = list(title = "Generated logdtsd", text = "The standard deviation of log time intervals in generated data. Use 0 for equal intervals."),
    help_gui_generation_defaults = list(title = "Generation preview values", text = "Replaces free labels with simple numeric values so ctGenerate can preview data shape. These values are not for inference."),
    help_fit_optimize = list(topic = "ctFit", param = "optimize", tooltip = "Whether to optimize or use full Bayesian sampling via Stan (generally very slow!)."),
    help_fit_priors = list(topic = "ctFit", param = "priors", tooltip = "Whether ctFit should use its prior distributions during fitting."),
    help_fit_cores = list(topic = "ctFit", param = "cores", tooltip = "Number of processor cores ctFit may use; the detected host total is shown beside this control."),
    help_ctFit = list(topic = "ctFit", tooltip = "Fit a continuous-time structural equation model."),
    help_ctOptimUncertainty = list(topic = "ctOptimUncertainty", tooltip = "Recompute uncertainty and approximate parameter draws for an optimized ctsem fit."),
    help_uncertainty_method = list(
      title = "Uncertainty method",
      topic = "ctOptimUncertainty",
      param = "uncertainty",
      tooltip = "How uncertainty is approximated for an optimized fit. Hessian is the usual fast default; importance sampling and bootstrap methods are slower."
    ),
    help_uncertainty_draws = list(topic = "ctOptimUncertainty", param = "draws", tooltip = "How approximate raw-parameter draws are constructed from the selected uncertainty method."),
    help_uncertainty_samples = list(topic = "ctOptimUncertainty", param = "finishsamples", tooltip = "Number of approximate draws, or number of full bootstrap refits, retained for uncertainty estimation."),
    help_uncertainty_control = list(
      title = "Advanced method controls",
      topic = "ctOptimUncertainty",
      param = "control",
      tooltip = "Optional method-specific tuning settings. Leave these at their defaults unless you have a reason to adjust the uncertainty calculation."
    ),
    help_ctGenerateFromFit = list(topic = "ctGenerateFromFit", tooltip = "Generate data from a fitted ctsem model."),
    help_fit_gen_nsamples = list(topic = "ctGenerateFromFit", param = "nsamples", tooltip = "Number of draws or generated datasets to return."),
    help_fit_gen_cores = list(topic = "ctGenerateFromFit", param = "cores", tooltip = "Number of processor cores used for generation. It initially follows the fit core selection; changing this value uses the value you enter."),
    help_fit_gen_fullposterior = list(topic = "ctGenerateFromFit", param = "fullposterior", tooltip = "Use posterior draws rather than only point estimates when generating data."),
    help_ctFitCovCheck = list(topic = "ctFitCovCheck", tooltip = "Compare model-implied and observed covariance structure."),
    help_cov_lags = list(topic = "ctFitCovCheck", param = "lags", tooltip = "Optional sequence of lag values, for example 0:10 for lags 0 through 10. Leave blank to use ctsem's default."),
    help_cov_cor = list(topic = "ctFitCovCheck", param = "cor", tooltip = "Compare correlations rather than covariances."),
    help_ctPredict = list(topic = "ctPredict", tooltip = "Obtain model predictions and smoothed latent-state estimates."),
    help_kalman_subjects = list(topic = "ctPredict", param = "subjects", tooltip = "Subjects for which predictions are returned."),
    help_kalman_timerange = list(topic = "ctPredict", param = "timerange", tooltip = "Time range over which predictions are calculated."),
    help_kalman_timestep = list(topic = "ctPredict", param = "timestep", tooltip = "Time increment used for the prediction grid."),
    help_kalman_removeObs = list(topic = "ctPredict", param = "removeObs", tooltip = "Whether observed values are removed before computing predictions."),
    help_kalmanvec = list(topic = "plot.ctKalmanDF", param = "kalmanvec", tooltip = "Prediction series to include in the Kalman plot."),
    help_errorvec = list(topic = "plot.ctKalmanDF", param = "errorvec", tooltip = "Error or interval series to include in the Kalman plot."),
    help_ctPostPredPlots = list(topic = "ctPostPredPlots", tooltip = "Plot posterior-predictive comparisons between observed and generated data."),
    help_ctACFresiduals = list(topic = "ctACFresiduals", tooltip = "Assess residual autocorrelation left unexplained by the model."),
    # ctACFresiduals forwards these controls through `...` to ctACF, whose
    # Rd file documents the actual arguments.
    help_acf_varnames = list(topic = "ctACF", param = "varnames", tooltip = "Variables whose residual autocorrelation is calculated."),
    help_acf_nboot = list(topic = "ctACF", param = "nboot", tooltip = "Number of bootstrap samples used for residual autocorrelation intervals."),
    help_ctDiscretePars = list(topic = "ctDiscretePars", tooltip = "Convert continuous-time parameters to discrete-time quantities at chosen times."),
    help_dynamic_subjects = list(topic = "ctDiscretePars", param = "subjects", tooltip = "Subjects, or population mean, for which discrete parameters are calculated."),
    help_dynamic_times = list(topic = "ctDiscretePars", param = "times", tooltip = "Times at which discrete-time parameters are evaluated."),
    help_dynamic_nsamples = list(topic = "ctDiscretePars", param = "nsamples", tooltip = "Number of posterior samples used for uncertainty summaries."),
    help_dynamic_observational = list(topic = "ctDiscretePars", param = "observational", tooltip = "Whether effects are calculated from observational rather than intervention-style dynamics."),
    help_ctPredictTIP = list(topic = "ctPredictTIP", tooltip = "Predict trajectories and dynamics at selected time-independent-predictor values."),
    help_tipred_tipreds = list(topic = "ctPredictTIP", param = "tipreds", tooltip = "Time-independent predictors whose values are varied."),
    help_tipred_subject = list(topic = "ctPredictTIP", param = "subject", tooltip = "Subject whose parameters are used for the prediction."),
    help_tipred_timestep = list(topic = "ctPredictTIP", param = "timestep", tooltip = "Time increment used to calculate predicted trajectories."),
    help_tipred_tipvalues = list(topic = "ctPredictTIP", param = "TIPvalues", tooltip = "Values assigned to the selected time-independent predictors."),
    help_matrix_random_effects = list(title = "RandomEffects", text = "Estimate subject-level variation in this free parameter. Available only for free parameter labels, not fixed numeric cells."),
    help_matrix_transform = list(title = "Transform", text = "Transforms generally do not need adjusting, but can support different priors or penalties and, in specific cases, added flexibility such as positive drift diagonals."),
    help_matrix_random_effects_scale = list(title = "RandomEffectsScale", text = "Scale for the standard deviation of this parameter's RandomEffects distribution. The default is 1."),
    help_matrix_time_independent_predictors = list(title = "Time Independent Predictors", text = "Subject-level predictors that moderate this free parameter. They must be named in Model > Specification.")
  )
}

ctgui_help_tooltip <- function(help) {
  help$tooltip %||% help$text %||% paste("Show help for", help$title %||% help$topic)
}

# The lias entries of one Rd page. A page documents every name it aliases,
# and those names are not derivable from its filename.
ctgui_rd_aliases <- function(rd) {
  tags <- vapply(rd, function(part) attr(part, "Rd_tag") %||% "", character(1L))
  aliases <- rd[tags == "\\alias"]
  if (!length(aliases)) return(character())
  trimws(vapply(aliases, function(a) paste(unlist(a), collapse = ""), character(1L)))
}

# Which Rd page documents a topic. Not always the one named after it: ctsem
# documents several functions per page and keeps renamed ones as aliases, so
# ctFitCovCheck lives in ctFitCheckCov.Rd. Matching the filename alone reported
# "No ctsem help found" for a function that is perfectly well documented.
ctgui_ctsem_rd_file <- function(rd_db, topic) {
  named <- paste0(topic, ".Rd")
  if (named %in% names(rd_db)) return(named)
  for (file in names(rd_db)) {
    if (topic %in% ctgui_rd_aliases(rd_db[[file]])) return(file)
  }
  NULL
}

# The plain text under an Rd node, with the markup dropped.
ctgui_rd_node_text <- function(node) {
  if (is.character(node)) return(paste(node, collapse = ""))
  if (!is.list(node)) return("")
  paste(vapply(node, ctgui_rd_node_text, character(1L)), collapse = "")
}

# One argument's description, read from the \arguments section of the parse
# tree rather than from rendered text. Exact, and it still works on a page
# Rd2txt will not render at all -- ctsem's own ctFit.Rd is one, which is how
# this came up.
ctgui_rd_argument_text <- function(rd, param) {
  tags <- vapply(rd, function(part) attr(part, "Rd_tag") %||% "", character(1L))
  for (section in rd[tags == "\\arguments"]) {
    item_tags <- vapply(section, function(part) attr(part, "Rd_tag") %||% "",
      character(1L))
    for (item in section[item_tags == "\\item"]) {
      if (length(item) < 2L) next
      names_given <- trimws(strsplit(ctgui_rd_node_text(item[[1L]]), ",")[[1L]])
      if (!param %in% names_given) next
      body <- trimws(gsub("[[:space:]]+", " ", ctgui_rd_node_text(item[[2L]])))
      if (nzchar(body)) return(paste0(param, ": ", body))
    }
  }
  NULL
}

ctgui_ctsem_help_text <- function(topic, param = NULL) {
  rd_db <- tryCatch(tools::Rd_db("ctsem"), error = function(e) e)
  if (inherits(rd_db, "error")) return(paste("No ctsem help found for", topic))
  topic_file <- ctgui_ctsem_rd_file(rd_db, topic)
  if (is.null(topic_file)) return(paste("No ctsem help found for", topic))
  if (!is.null(param)) {
    from_tree <- ctgui_rd_argument_text(rd_db[[topic_file]], param)
    if (!is.null(from_tree)) return(from_tree)
  }
  rendered <- tryCatch(
    utils::capture.output(tools::Rd2txt(
      rd_db[[topic_file]], options = list(underline_titles = FALSE)
    )),
    error = function(e) e
  )
  # A page that will not render is not the same as a page without this
  # argument, and saying the latter sends the reader looking in the wrong place.
  if (inherits(rendered, "error")) {
    return(paste0("Could not render the ctsem help for ", topic, ": ",
      conditionMessage(rendered)))
  }
  text <- rendered
  backspace <- rawToChar(as.raw(8))
  text <- gsub("\\033\\[[0-9;]*m", "", text, perl = TRUE)
  for (i in seq_len(4L)) text <- gsub(paste0(".?", backspace), "", text)
  text <- gsub("\\r", "", text, fixed = TRUE)
  text <- text[!grepl("^\\s*([_=\\-]\\s*){3,}\\s*$", text)]
  text <- gsub("\\s+$", "", text)
  if (is.null(param)) return(paste(text, collapse = "\n"))
  escaped_param <- gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", param)
  start <- grep(paste0("^\\s*", escaped_param, ":"), text)
  if (!length(start)) return(paste("No argument help found for", param, "in", topic))
  next_arg <- grep("^\\s*[[:alnum:]_.]+:", text)
  next_arg <- next_arg[next_arg > start[1L]]
  end <- if (length(next_arg)) next_arg[1L] - 1L else min(length(text), start[1L] + 8L)
  paste(text[start[1L]:end], collapse = "\n")
}
