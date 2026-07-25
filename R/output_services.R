# Reproducible output code services -----------------------------------------

# These helpers deliberately accept ordinary lists rather than Shiny inputs.
# The server is responsible only for translating controls into an options list;
# code assembly and formatting remain deterministic and directly testable.

ctgui_code_symbol <- function(x) structure(as.character(x)[1L], class = "ctgui_code_symbol")

ctgui_code_value <- function(x) {
  if (inherits(x, "ctgui_code_symbol")) return(unclass(x))
  if (is.matrix(x)) return(paste(ctgui_deparse(x), collapse = "\n"))
  paste(deparse(x, width.cutoff = 500L), collapse = "\n")
}

ctgui_code_option <- function(options, name, default = NULL) {
  if (!is.list(options)) stop("options must be a list", call. = FALSE)
  if (!name %in% names(options) || is.null(options[[name]])) default else options[[name]]
}

ctgui_code_arg_lines <- function(args, indent = "  ", final_comma = FALSE) {
  if (!length(args)) return(character())
  if (is.null(names(args)) || any(!nzchar(names(args)))) {
    stop("Code arguments must be a named list", call. = FALSE)
  }
  lines <- vapply(names(args), function(name) {
    paste0(indent, name, " = ", ctgui_code_value(args[[name]]))
  }, character(1L))
  if (length(lines) > 1L || isTRUE(final_comma)) {
    comma <- seq_along(lines) < length(lines) | isTRUE(final_comma)
    lines[comma] <- paste0(lines[comma], ",")
  }
  lines
}

ctgui_code_list_lines <- function(name, values, indent = "  ", final_comma = FALSE) {
  values <- values[!vapply(values, is.null, logical(1L))]
  inner <- ctgui_code_arg_lines(values, paste0(indent, "  "))
  end <- paste0(indent, ")")
  if (isTRUE(final_comma)) end <- paste0(end, ",")
  c(paste0(indent, name, " = list("), inner, end)
}

ctgui_code_optional_expression <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L) return(x)
  if (!nzchar(trimws(x))) return(NULL)
  parsed <- tryCatch(eval(parse(text = x), envir = baseenv()), error = function(error) error)
  if (inherits(parsed, "error")) x else parsed
}

ctgui_output_data_source <- function(type = c("none", "generated", "r_object", "csv", "session"),
    name = NULL, generation = list()) {
  type <- match.arg(type)
  if (!is.list(generation)) stop("generation must be a list", call. = FALSE)
  structure(list(type = type, name = name, generation = generation),
    class = "ctgui_output_data_source")
}

ctgui_output_data_code <- function(source = ctgui_output_data_source()) {
  if (is.null(source)) source <- ctgui_output_data_source()
  if (!is.list(source)) stop("source must be a ctgui output data source", call. = FALSE)
  type <- source$type %||% "none"
  name <- source$name %||% ""
  generation <- source$generation %||% list()

  switch(type,
    generated = {
      args <- list(
        ctmodelobj = ctgui_code_symbol("model"),
        n.subjects = ctgui_code_option(generation, "n.subjects", 100),
        burnin = ctgui_code_option(generation, "burnin", 0),
        dtmean = ctgui_code_option(generation, "dtmean", 1),
        logdtsd = ctgui_code_option(generation, "logdtsd", 0),
        Tpoints = ctgui_code_option(generation, "Tpoints", 10),
        wide = ctgui_code_option(generation, "wide", FALSE)
      )
      lines <- ctgui_code_arg_lines(args)
      c(
        "# The GUI preview substituted numeric defaults for free parameters.",
        "# ctgui_generate_data() is internal and is intentionally not a public workflow API.",
        "# Before running this public ctsem equivalent, give every free model parameter",
        "# a numeric value appropriate for the intended simulation.",
        "data <- ctsem::ctGenerate(",
        lines,
        ")"
      )
    },
    r_object = {
      if (!grepl("^[.A-Za-z][.A-Za-z0-9_]*$", name)) {
        stop("R data source name must be a syntactically valid object name", call. = FALSE)
      }
      paste0("data <- ", name)
    },
    csv = c(
      paste0("# Imported CSV was named ", ctgui_code_value(name), " in the Shiny session."),
      "data <- utils::read.csv(\"path/to/data.csv\", stringsAsFactors = FALSE)"
    ),
    session = "# Active data exists only in the Shiny session; assign it here as `data` before fitting.",
    none = "# No data is currently active.",
    stop("Unknown output data source type: ", type, call. = FALSE)
  )
}

ctgui_output_base_code <- function(spec, source = ctgui_output_data_source()) {
  ctgui_check_spec(spec)
  c(
    "# Model specification",
    "# Explanations are shown in the GUI.",
    ctgui_export_code(spec),
    "",
    "# Data",
    ctgui_output_data_code(source)
  )
}

ctgui_output_fit_code <- function(options = list()) {
  optimize <- isTRUE(ctgui_code_option(options, "optimize", TRUE))
  args <- list(
    datalong = ctgui_code_symbol("data"),
    model = ctgui_code_symbol("model"),
    optimize = optimize,
    priors = isTRUE(ctgui_code_option(options, "priors", TRUE)),
    cores = as.integer(ctgui_code_option(options, "cores", 1L))
  )
  extra <- ctgui_code_option(options, "extra_args", list())
  if (!is.list(extra)) stop("extra_args must be a named list", call. = FALSE)
  if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra))))) {
    stop("extra_args must be a named list", call. = FALSE)
  }
  extra <- extra[!names(extra) %in% c(names(args), "optimcontrol", "plot")]
  arg_lines <- ctgui_code_arg_lines(args, final_comma = TRUE)

  if (optimize) {
    control <- ctgui_code_option(options, "uncertainty_control", list())
    optimcontrol <- list(
      uncertainty = ctgui_code_option(options, "uncertainty", "hessian"),
      uncertaintyDraws = ctgui_code_option(options, "uncertainty_draws", "auto"),
      finishsamples = as.integer(ctgui_code_option(options, "finishsamples", 1000L)),
      uncertaintyControl = control[!vapply(control, is.null, logical(1L))]
    )
    optim_lines <- ctgui_code_list_lines("optimcontrol", optimcontrol, final_comma = TRUE)
  } else {
    optim_lines <- character()
  }
  extra_lines <- ctgui_code_arg_lines(extra, final_comma = length(extra) > 0L)
  c(
    "# Fit",
    "fit <- ctsem::ctFit(",
    arg_lines,
    optim_lines,
    extra_lines,
    "  plot = FALSE",
    ")",
    "",
    "# Output",
    "summary(fit)",
    "ctsem::ctSummaryMatrices(fit)"
  )
}

ctgui_output_uncertainty_code <- function(options = list()) {
  control <- ctgui_code_option(options, "uncertainty_control", list())
  args <- list(
    fit = ctgui_code_symbol("fit"),
    uncertainty = ctgui_code_option(options, "uncertainty", "hessian"),
    draws = ctgui_code_option(options, "draws",
      ctgui_code_option(options, "uncertainty_draws", "auto")),
    finishsamples = as.integer(ctgui_code_option(options, "finishsamples", 1000L)),
    cores = as.integer(ctgui_code_option(options, "cores", 1L))
  )
  lines <- ctgui_code_arg_lines(args, final_comma = TRUE)
  c(
    "# Recompute optimized-fit uncertainty",
    "fit <- ctsem::ctOptimUncertainty(",
    lines,
    ctgui_code_list_lines("control",
      control[!vapply(control, is.null, logical(1L))]),
    ")"
  )
}

ctgui_output_diagnostic_code <- function(diagnostic = c(
    "generate_from_fit", "cov_check", "kalman", "postpred", "residual_acf",
    "dynamics", "tipred"), options = list()) {
  diagnostic <- match.arg(diagnostic)
  value <- function(name, default = NULL) ctgui_code_option(options, name, default)
  extra_args <- function(protected) {
    extra <- value("extra_args", list())
    if (!is.list(extra) || (length(extra) &&
        (is.null(names(extra)) || any(!nzchar(names(extra)))))) {
      stop("extra_args must be a named list", call. = FALSE)
    }
    extra[!names(extra) %in% protected]
  }

  switch(diagnostic,
    generate_from_fit = {
      args <- list(
        fit = ctgui_code_symbol("fit"),
        nsamples = value("nsamples", 1L),
        fullposterior = isTRUE(value("fullposterior", FALSE)),
        cores = as.integer(value("cores", 1L))
      )
      c(
        "# Generate from fit for diagnostics",
        "fit <- ctsem::ctGenerateFromFit(",
        ctgui_code_arg_lines(c(args, extra_args(names(args)))),
        ")"
      )
    },
    cov_check = {
      lags <- value("lags", quote(0:3))
      if (is.character(lags) && length(lags) == 1L) {
        parsed <- tryCatch(parse(text = lags)[[1L]], error = function(error) error)
        if (inherits(parsed, "error")) stop("lags must be valid R code", call. = FALSE)
        lags <- parsed
      }
      cor <- isTRUE(value("cor", TRUE))
      args <- list(
        fit = ctgui_code_symbol("fit"),
        cor = cor,
        lags = ctgui_code_symbol("cov_lags"),
        plot = FALSE,
        cores = as.integer(value("cores", 1L))
      )
      c(
        "# Covariance check",
        paste0("cov_lags <- ", ctgui_code_value(lags)),
        "cov_check <- ctsem::ctFitCovCheck(",
        ctgui_code_arg_lines(c(args, extra_args(names(args)))),
        ")",
        "cov_check_plots <- ctsem::ctFitCovCheckPlot(",
        "  cov_check,",
        "  maxlag = max(cov_lags),",
        paste0("  cor = ", ctgui_code_value(cor)),
        ")",
        "lapply(cov_check_plots, print)"
      )
    },
    kalman = {
      optional <- list(
        subjects = ctgui_code_optional_expression(value("subjects")),
        timerange = ctgui_code_optional_expression(value("timerange")),
        timestep = ctgui_code_optional_expression(value("timestep")),
        removeObs = ctgui_code_optional_expression(value("removeObs"))
      )
      optional <- optional[!vapply(optional, is.null, logical(1L))]
      args <- c(list(fit = ctgui_code_symbol("fit")), optional)
      args <- c(args, extra_args(c(names(args), "plot")), list(plot = FALSE))
      c(
        "# Prediction plots using ctPredict",
        "prediction <- ctsem::ctPredict(",
        ctgui_code_arg_lines(args),
        ")",
        "plot(",
        "  prediction,",
        paste0("  kalmanvec = ", ctgui_code_value(value("kalmanvec", c("y", "yprior"))), ","),
        paste0("  errorvec = ", ctgui_code_value(value("errorvec", "auto"))),
        ")"
      )
    },
    postpred = c(
      "# Posterior predictive checks",
      "postpred_plots <- ctsem::ctPostPredPlots(fit)",
      "lapply(postpred_plots, print)"
    ),
    residual_acf = {
      args <- list(
        fit = ctgui_code_symbol("fit"),
        varnames = value("varnames", "auto"),
        nboot = as.integer(value("nboot", 100L)),
        plot = FALSE
      )
      c(
        "# Residual autocorrelation",
        "residual_acf <- ctsem::ctACFresiduals(",
        ctgui_code_arg_lines(c(args, extra_args(names(args)))),
        ")",
        "print(ctsem::plotctACF(residual_acf))"
      )
    },
    dynamics = {
      optional <- list(
        subjects = ctgui_code_optional_expression(value("subjects")),
        times = ctgui_code_optional_expression(value("times")),
        nsamples = ctgui_code_optional_expression(value("nsamples"))
      )
      optional <- optional[!vapply(optional, is.null, logical(1L))]
      ylim <- ctgui_code_optional_expression(value("ylim"))
      args <- c(
        list(fit = ctgui_code_symbol("fit")),
        optional,
        list(
          observational = isTRUE(value("observational", FALSE)),
          plot = TRUE,
          cores = as.integer(value("cores", 1L))
        )
      )
      args <- c(args, extra_args(names(args)))
      c(
        "# Dynamics / impulse-response style plot",
        "dynamics <- ctsem::ctDiscretePars(",
        ctgui_code_arg_lines(args),
        ")",
        if (!is.null(ylim)) paste0(
          "# Apply y limits post hoc when supported: ylim = ", ctgui_code_value(ylim)),
        "print(dynamics)"
      )
    },
    tipred = {
      args <- list(
        sf = ctgui_code_symbol("fit"),
        tipreds = ctgui_code_optional_expression(value("tipreds", "all")),
        subject = ctgui_code_optional_expression(value("subject")),
        timestep = ctgui_code_optional_expression(value("timestep", "auto")),
        TIPvalues = ctgui_code_optional_expression(value("TIPvalues"))
      )
      args <- args[!vapply(args, is.null, logical(1L))]
      lines <- ctgui_code_arg_lines(args)
      c(
        "# TI predictor effects",
        "tip_plots <- ctsem::ctPredictTIP(",
        lines,
        ")",
        "# tip_plots$Process and tip_plots$Dynamics contain the returned plot groups."
      )
    }
  )
}

ctgui_output_snippet <- function(action, options = list(), spec = NULL) {
  if (identical(action, "fit")) return(ctgui_output_fit_code(options))
  if (identical(action, "uncertainty")) return(ctgui_output_uncertainty_code(options))
  if (action %in% c("generate_from_fit", "cov_check", "kalman", "postpred",
      "residual_acf", "dynamics", "tipred")) {
    return(ctgui_output_diagnostic_code(action, options))
  }
  switch(action,
    summary = c("# Fit summary", "summary(fit)"),
    summary_matrices = c("# Fit summary matrices", "ctsem::ctSummaryMatrices(fit)"),
    model_pars = c("# Model parameter table", "model$pars"),
    fit_comparison = c(
      "# Fit comparison",
      "# Save candidate fits in a named list, then use ctsem summaries to compare them.",
      "fits <- list(fit1 = fit)",
      "lapply(fits, summary)"
    ),
    raw_plot = c(
      "# Data visualisation",
      paste0("# Plot type: ", ctgui_code_value(ctgui_code_option(options,
        "plot_type", "Subject trajectories"))),
      paste0("# Time column: ", ctgui_code_value(ctgui_code_option(options,
        "time", spec$time %||% "time"))),
      paste0("# Plotted variables: ", ctgui_code_value(ctgui_code_option(options,
        "variables", spec$manifest_names[1L] %||% "manifest"))),
      paste0("# Subject ID column: ", ctgui_code_value(ctgui_code_option(options,
        "id", spec$id %||% "id"))),
      paste0("# Colour variable: ", ctgui_code_value(ctgui_code_option(options,
        "colour", "(plotted variable)"))),
      "# Use the Data > Visuals controls to reproduce the current GUI plot."
    ),
    model_visual = c(
      "# Model visualisation",
      paste0("# Visual type: ", ctgui_code_value(ctgui_code_option(options,
        "visual_type", "Temporal dynamics graph"))),
      "# The graph is derived from DRIFT, DIFFUSION, and LAMBDA."
    ),
    stop("Unknown output action: ", action, call. = FALSE)
  )
}

ctgui_output_workflow_code <- function(spec, source = ctgui_output_data_source(),
    snippets = list()) {
  if (!is.list(snippets)) stop("snippets must be a named list", call. = FALSE)
  lines <- ctgui_output_base_code(spec, source)
  if (!length(snippets)) {
    lines <- c(lines, "",
      "# Fit or run diagnostics in the GUI to add reproducible action code here.")
  } else {
    if (is.null(names(snippets)) || any(!nzchar(names(snippets)))) {
      stop("snippets must be named by action", call. = FALSE)
    }
    lines <- c(lines, "", "# Actions run in the GUI")
    for (snippet in snippets) lines <- c(lines, "", snippet)
  }
  paste(lines, collapse = "\n")
}
