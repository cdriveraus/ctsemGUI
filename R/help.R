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

ctgui_ctsem_help_text <- function(topic, param = NULL) {
  rd_db <- tryCatch(tools::Rd_db("ctsem"), error = function(e) e)
  if (inherits(rd_db, "error")) return(paste("No ctsem help found for", topic))
  topic_file <- paste0(topic, ".Rd")
  if (!topic_file %in% names(rd_db)) return(paste("No ctsem help found for", topic))
  text <- tryCatch(
    utils::capture.output(tools::Rd2txt(
      rd_db[[topic_file]], options = list(underline_titles = FALSE)
    )),
    error = function(e) paste("Could not load help:", conditionMessage(e))
  )
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
