#' Build guided ctsem GUI matrices
#'
#' @param spec A `ctsemgui_spec` object.
#' @param structure Matrix structure. One of `"linear_growth"`,
#'   `"dynamic_var"`, or `"dynamic_var_trend"`.
#' @param measurement Measurement model. One of `"single_indicator"`,
#'   `"marker"`, or `"fixed_loadings"`.
#' @param names Named list of model names. Common entries are
#'   `factor_names`, `manifest_names`, `id`, and `time`.
#' @param options Named list of builder options, including `n`, `type`,
#'   `trend_type`, `indicators_per_factor`, `free_noise_correlations`,
#'   `fixed_loadings`, and `trend_coupling`.
#'
#' @return `ctgui_build_matrices()` and
#'   `ctgui_build_measurement_matrices()` return updated `ctsemgui_spec`
#'   objects without changing the model names or data-role fields.
#'   `ctgui_build_model()` returns a `ctsemgui_spec`.
#'   `ctgui_structures()` and `ctgui_measurements()` return data frames.
#'   `ctgui_validate_data()` returns a validation data frame.
#'   `ctgui_graph_edges()` returns graph edge data for one model element.
ctgui_build_matrices <- function(spec,
    structure = c("dynamic_var", "linear_growth", "dynamic_var_trend"),
    options = list()) {
  ctgui_check_spec(spec)
  structure <- match.arg(structure)
  if (is.null(options)) options <- list()

  latent_names <- spec$latent_names
  matrices <- spec$matrices

  selected_latents <- function(value, field, default = latent_names) {
    out <- ctgui_as_names(value %||% default, field)
    missing <- setdiff(out, latent_names)
    if (length(missing)) stop(field, " contains unknown latent names: ", paste(missing, collapse = ", "), call. = FALSE)
    out
  }

  if (identical(structure, "dynamic_var")) {
    dyn <- selected_latents(options$dynamic_latents, "dynamic_latents")
    matrices$DRIFT <- matrix(0, length(latent_names), length(latent_names), dimnames = list(latent_names, latent_names))
    for (r in dyn) for (c in dyn) {
      matrices$DRIFT[r, c] <- if (identical(r, c)) paste0("auto_", r) else paste0("cross_", c, "_to_", r)
    }
    matrices$DIFFUSION <- ctgui_diffusion_matrix(latent_names, ctgui_free_noise_correlations(options), active_latents = dyn)
    matrices$CINT <- ctgui_fixed_matrix(0, latent_names, "CINT", ncol = 1L)
    matrices$T0MEANS <- ctgui_label_matrix("T0MEANS", latent_names, "mean", ncol = 1L)
    matrices$T0VAR <- ctgui_lower_label_matrix("t0var", latent_names)
  } else if (identical(structure, "linear_growth")) {
    level <- selected_latents(options$level_latents, "level_latents")
    slope <- selected_latents(options$slope_latents, "slope_latents")
    if (length(level) != length(slope)) stop("level_latents and slope_latents must have the same length", call. = FALSE)
    if (any(level == slope)) stop("level and slope latent pairs must be distinct", call. = FALSE)
    matrices$DRIFT <- matrix(0, length(latent_names), length(latent_names), dimnames = list(latent_names, latent_names))
    for (i in seq_along(level)) matrices$DRIFT[level[i], slope[i]] <- 1
    matrices$DIFFUSION <- matrix(0, length(latent_names), length(latent_names), dimnames = list(latent_names, latent_names))
    matrices$CINT <- ctgui_fixed_matrix(0, latent_names, "CINT", ncol = 1L)
    matrices$T0MEANS <- ctgui_label_matrix("T0MEANS", latent_names, "mean", ncol = 1L)
    matrices$T0VAR <- ctgui_lower_label_matrix("t0var", latent_names)
  } else if (identical(structure, "dynamic_var_trend")) {
    dyn <- selected_latents(options$dynamic_latents, "dynamic_latents")
    trend <- selected_latents(options$trend_latents, "trend_latents", default = character())
    if (length(trend) == 0L) stop("trend_latents must be selected for trend structures", call. = FALSE)
    if (length(dyn) != length(trend)) stop("dynamic_latents and trend_latents must have the same length", call. = FALSE)
    if (any(dyn == trend)) stop("dynamic and trend latent pairs must be distinct", call. = FALSE)
    trend_type <- match.arg(options$trend_type %||% "linear", c("linear", "exponential"))
    matrices$DRIFT <- matrix(0, length(latent_names), length(latent_names), dimnames = list(latent_names, latent_names))
    for (target in seq_along(dyn)) {
      for (source in seq_along(dyn)) {
        matrices$DRIFT[dyn[target], dyn[source]] <- if (target == source) {
          paste0("auto_", dyn[target])
        } else {
          paste0("cross_", dyn[source], "_to_", dyn[target])
        }
      }
      matrices$DRIFT[dyn[target], trend[target]] <- ctgui_trend_coupling_value(options$trend_coupling, dyn[target])
      matrices$DRIFT[trend[target], trend[target]] <- if (identical(trend_type, "linear")) 0 else paste0("trend_decay_", trend[target])
    }
    matrices$DIFFUSION <- ctgui_diffusion_matrix(latent_names, ctgui_free_noise_correlations(options), active_latents = dyn)
    matrices$CINT <- ctgui_fixed_matrix(0, latent_names, "CINT", ncol = 1L)
    matrices$T0MEANS <- ctgui_label_matrix("T0MEANS", latent_names, "mean", ncol = 1L)
    matrices$T0VAR <- ctgui_lower_label_matrix("t0var", latent_names)
  }

  spec$matrices <- ctgui_order_matrices(matrices)
  spec$builder <- list(
    structure = structure,
    dynamic_latents = options$dynamic_latents %||% NULL,
    level_latents = options$level_latents %||% NULL,
    slope_latents = options$slope_latents %||% NULL,
    trend_latents = options$trend_latents %||% NULL,
    trend_type = if (identical(structure, "dynamic_var_trend")) options$trend_type %||% "linear" else NULL,
    options = options
  )
  ctgui_sync_model_from_matrices(spec)
}

#' @rdname ctgui_build_matrices
ctgui_build_measurement_matrices <- function(spec,
    measurement = c("single_indicator", "marker", "fixed_loadings"),
    options = list()) {
  ctgui_check_spec(spec)
  measurement <- match.arg(measurement)
  if (is.null(options)) options <- list()

  latent_names <- spec$latent_names
  manifest_names <- spec$manifest_names
  factor_latents <- ctgui_as_names(options$factor_latents %||% latent_names, "factor_latents")
  missing_latents <- setdiff(factor_latents, latent_names)
  if (length(missing_latents)) stop("factor_latents contains unknown latent names: ", paste(missing_latents, collapse = ", "), call. = FALSE)

  manifest_blocks <- ctgui_spec_manifest_blocks(options$manifest_blocks, manifest_names, factor_latents)
  lambda <- matrix(0, length(manifest_names), length(latent_names), dimnames = list(manifest_names, latent_names))
  for (i in seq_along(factor_latents)) {
    block <- manifest_blocks[[i]]
    loadings <- ctgui_measurement_loadings(measurement, block, factor_latents[i], options, i)
    lambda[block, factor_latents[i]] <- loadings
  }

  trend_latents <- options$trend_latents %||% character()
  if (length(trend_latents)) {
    trend_latents <- ctgui_as_names(trend_latents, "trend_latents", allow_empty = TRUE)
    if (length(trend_latents) != length(factor_latents)) stop("trend_latents must match factor_latents length", call. = FALSE)
    missing_trends <- setdiff(trend_latents, latent_names)
    if (length(missing_trends)) stop("trend_latents contains unknown latent names: ", paste(missing_trends, collapse = ", "), call. = FALSE)
    for (i in seq_along(factor_latents)) lambda[manifest_blocks[[i]], trend_latents[i]] <- lambda[manifest_blocks[[i]], factor_latents[i]]
  }

  spec$matrices$LAMBDA <- lambda
  spec$matrices$MANIFESTMEANS <- ctgui_fixed_matrix(0, manifest_names, "MANIFESTMEANS", ncol = 1L)
  spec$matrices$MANIFESTVAR <- ctgui_diag_label_matrix("MANIFESTVAR", manifest_names)
  spec$matrices <- ctgui_order_matrices(spec$matrices)
  spec$measurement_builder <- list(
    measurement = measurement,
    factor_latents = factor_latents,
    trend_latents = trend_latents,
    manifest_blocks = manifest_blocks,
    options = options
  )
  ctgui_sync_model_from_matrices(spec)
}

ctgui_build_model <- function(structure = c("dynamic_var", "linear_growth", "dynamic_var_trend"),
    measurement = c("single_indicator", "marker", "fixed_loadings"),
    names = list(),
    options = list()) {

  structure <- match.arg(structure)
  measurement <- match.arg(measurement)
  if (is.null(names)) names <- list()
  if (is.null(options)) options <- list()

  n <- options$n
  if (is.null(n) || length(n) == 0L || is.na(n[1L])) {
    supplied_names <- names$factor_names %||% names$dynamic_names
    n <- if (length(supplied_names)) length(supplied_names) else 1L
  }
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L) stop("options$n must be a positive integer", call. = FALSE)

  factor_names <- ctgui_builder_names(names$factor_names %||% names$dynamic_names,
    prefix = options$factor_prefix %||% "eta", n = n, field = "factor_names")
  type <- match.arg(options$type %||% "ct", c("ct", "dt"))
  id <- names$id %||% options$id %||% "id"
  time <- names$time %||% options$time %||% "time"
  tdpred_names <- ctgui_as_names(names$tdpred_names %||% options$tdpred_names %||% character(),
    "tdpred_names", allow_empty = TRUE)
  tipred_names <- ctgui_as_names(names$tipred_names %||% options$tipred_names %||% character(),
    "tipred_names", allow_empty = TRUE)

  latent_names <- switch(structure,
    linear_growth = as.vector(rbind(
      paste0(factor_names, "_level"),
      paste0(factor_names, "_slope")
    )),
    dynamic_var = factor_names,
    dynamic_var_trend = as.vector(rbind(
      factor_names,
      paste0(factor_names, "_trend")
    ))
  )

  measurement_spec <- ctgui_build_measurement(
    measurement = measurement,
    factor_names = factor_names,
    latent_names = latent_names,
    manifest_names = names$manifest_names,
    options = options,
    structure = structure
  )

  matrices <- switch(structure,
    linear_growth = ctgui_linear_growth_matrices(
      factor_names = factor_names,
      latent_names = latent_names,
      measurement_spec = measurement_spec
    ),
    dynamic_var = ctgui_dynamic_var_matrices(
      factor_names = factor_names,
      latent_names = latent_names,
      measurement_spec = measurement_spec,
      options = options
    ),
    dynamic_var_trend = ctgui_dynamic_var_trend_matrices(
      factor_names = factor_names,
      latent_names = latent_names,
      measurement_spec = measurement_spec,
      options = options
    )
  )

  spec <- ctgui_spec(
    latent_names = latent_names,
    manifest_names = measurement_spec$manifest_names,
    type = type,
    id = id,
    time = time,
    Tpoints = options$Tpoints %||% NULL,
    manifest_type = options$manifest_type %||% rep(0, length(measurement_spec$manifest_names)),
    tdpred_names = tdpred_names,
    tipred_names = tipred_names,
    matrices = matrices,
    tipredDefault = isTRUE(options$tipredDefault %||% FALSE)
  )

  spec$builder <- list(
    structure = structure,
    measurement = measurement,
    factor_names = factor_names,
    latent_names = latent_names,
    manifest_blocks = measurement_spec$manifest_blocks,
    trend_type = if (identical(structure, "dynamic_var_trend")) options$trend_type %||% "linear" else NULL,
    options = options
  )
  spec
}

#' @rdname ctgui_build_model
ctgui_structures <- function() {
  data.frame(
    id = c("linear_growth", "dynamic_var", "dynamic_var_trend"),
    title = c("Linear growth", "Cross-lagged / VAR dynamics", "Cross-lagged / VAR with trend processes"),
    description = c(
      "n-dimensional latent growth structure with level/slope processes and correlated initial individual differences.",
      "n-dimensional dynamic system with auto-effects on the DRIFT diagonal and cross-effects off diagonal.",
      "n-dimensional dynamic system with paired trend processes that share the selected measurement model."
    ),
    stringsAsFactors = FALSE
  )
}

#' @rdname ctgui_build_model
ctgui_measurements <- function() {
  data.frame(
    id = c("single_indicator", "marker", "fixed_loadings"),
    title = c("Single-indicator identity", "Marker-loading factor model", "User-fixed loading factor model"),
    description = c(
      "One observed indicator per factor, loading fixed to 1.",
      "Multiple indicators per factor with the first loading fixed to 1 and remaining loadings free.",
      "Multiple indicators per factor with loadings supplied through options$fixed_loadings."
    ),
    stringsAsFactors = FALSE
  )
}

#' @rdname ctgui_build_model
#' @param spec A `ctsemgui_spec`.
#' @param data A data frame to check against the model specification.
ctgui_validate_data <- function(spec, data) {
  ctgui_check_spec(spec)
  messages <- list()
  add <- function(severity, field, message) {
    messages[[length(messages) + 1L]] <<- data.frame(
      severity = severity, field = field, message = message,
      stringsAsFactors = FALSE
    )
  }

  if (!is.data.frame(data)) {
    return(data.frame(severity = "error", field = "data", message = "data must be a data.frame",
      stringsAsFactors = FALSE))
  }

  required <- unique(c(spec$id, spec$time, spec$manifest_names, spec$tdpred_names, spec$tipred_names))
  missing <- setdiff(required, names(data))
  if (length(missing)) add("error", "columns", paste("Missing required columns:", paste(missing, collapse = ", ")))

  if (spec$id %in% names(data) && spec$time %in% names(data)) {
    key <- paste(data[[spec$id]], data[[spec$time]], sep = "\r")
    if (anyDuplicated(key)) add("warning", "id/time", "Some rows have duplicate id/time combinations")
    if (!is.numeric(data[[spec$time]])) {
      add("error", spec$time, "Time column should be numeric for ctsem fitting")
    } else {
      by_id <- split(data[[spec$time]], data[[spec$id]])
      unsorted <- vapply(by_id, function(x) any(diff(x) < 0, na.rm = TRUE), logical(1L))
      if (any(unsorted)) add("warning", spec$time, "Some subject time series are not sorted by time")
      gaps <- unlist(lapply(by_id, function(x) diff(sort(unique(x)))), use.names = FALSE)
      if (length(gaps) && any(gaps <= 0, na.rm = TRUE)) add("warning", spec$time, "Some subjects have non-positive time gaps")
      if (length(gaps) && stats::sd(gaps, na.rm = TRUE) > 0) add("info", spec$time, "Unequal time gaps detected; this is expected in continuous-time models but should be inspected")
    }
  }

  present_manifests <- intersect(spec$manifest_names, names(data))
  for (variable in present_manifests) {
    if (anyNA(data[[variable]])) add("info", variable, "Manifest missing values are allowed, but inspect missingness patterns")
    if (is.numeric(data[[variable]]) && spec$id %in% names(data)) {
      within_sd <- unlist(lapply(split(data[[variable]], data[[spec$id]]), stats::sd, na.rm = TRUE), use.names = FALSE)
      if (all(is.na(within_sd) | within_sd == 0)) add("warning", variable, "No within-person variation detected")
      means <- unlist(lapply(split(data[[variable]], data[[spec$id]]), mean, na.rm = TRUE), use.names = FALSE)
      if (length(stats::na.omit(means)) > 1L && stats::sd(means, na.rm = TRUE) == 0) add("info", variable, "No between-person variation detected")
    }
  }

  covariates <- intersect(c(spec$tdpred_names, spec$tipred_names), names(data))
  for (variable in covariates) {
    if (anyNA(data[[variable]])) add("error", variable, "Missing values in TD/TI predictors require explicit handling before fitting")
    if (is.numeric(data[[variable]])) {
      s <- stats::sd(data[[variable]], na.rm = TRUE)
      if (is.finite(s) && s > 10) add("info", variable, "Predictor scale is large; centering/scaling may improve estimation")
    }
  }

  if (!length(messages)) {
    return(data.frame(severity = "ok", field = "data", message = "No data readiness issues found",
      stringsAsFactors = FALSE))
  }
  do.call(rbind, messages)
}

#' @rdname ctgui_build_model
#' @param element Graph element: `"drift"`, `"diffusion"`, `"measurement"`, or
#'   `"trend"`.
ctgui_graph_edges <- function(spec, element = c("drift", "diffusion", "measurement", "trend")) {
  ctgui_check_spec(spec)
  element <- match.arg(element)
  out <- switch(element,
    drift = ctgui_edges_from_matrix(spec$matrices$DRIFT, directed = TRUE, element = "DRIFT"),
    diffusion = ctgui_diffusion_edges(spec),
    measurement = ctgui_edges_from_lambda(spec),
    trend = ctgui_trend_edges(spec)
  )
  if (is.null(out) || nrow(out) == 0L) {
    return(data.frame(from = character(), to = character(), value = character(),
      directed = logical(), element = character(), stringsAsFactors = FALSE))
  }
  out
}

ctgui_builder_names <- function(x, prefix, n, field) {
  if (is.null(x)) x <- paste0(prefix, seq_len(n))
  x <- ctgui_as_names(x, field)
  if (length(x) != n) stop(field, " must contain ", n, " names", call. = FALSE)
  x
}

ctgui_build_measurement <- function(measurement, factor_names, latent_names, manifest_names, options, structure) {
  n <- length(factor_names)
  indicators_per_factor <- as.integer(options$indicators_per_factor %||% if (measurement == "single_indicator") 1L else 2L)
  if (length(indicators_per_factor) != 1L || is.na(indicators_per_factor) || indicators_per_factor < 1L) {
    stop("options$indicators_per_factor must be a positive integer", call. = FALSE)
  }
  if (measurement == "single_indicator") indicators_per_factor <- 1L

  manifest_blocks <- ctgui_manifest_blocks(manifest_names, factor_names, indicators_per_factor)
  manifest_vector <- unlist(manifest_blocks, use.names = FALSE)
  lambda <- matrix(0, nrow = length(manifest_vector), ncol = length(latent_names),
    dimnames = list(manifest_vector, latent_names))

  for (i in seq_along(factor_names)) {
    block <- manifest_blocks[[i]]
    dynamic_col <- ctgui_measurement_dynamic_col(structure, i)
    trend_col <- ctgui_measurement_trend_col(structure, i, n)
    loadings <- ctgui_measurement_loadings(measurement, block, factor_names[i], options, block_index = i)
    lambda[block, dynamic_col] <- loadings
    if (!is.null(trend_col)) lambda[block, trend_col] <- loadings
  }

  list(
    LAMBDA = lambda,
    manifest_names = manifest_vector,
    manifest_blocks = manifest_blocks
  )
}

ctgui_manifest_blocks <- function(manifest_names, factor_names, indicators_per_factor) {
  if (is.null(manifest_names)) {
    return(lapply(factor_names, function(f) {
      if (indicators_per_factor == 1L) f else paste0(f, "_y", seq_len(indicators_per_factor))
    }))
  }
  if (is.list(manifest_names)) {
    if (length(manifest_names) != length(factor_names)) {
      stop("names$manifest_names list must have one element per factor", call. = FALSE)
    }
    return(lapply(seq_along(manifest_names), function(i) {
      ctgui_as_names(manifest_names[[i]], paste0("manifest_names[[", i, "]]"))
    }))
  }
  manifest_names <- ctgui_as_names(manifest_names, "manifest_names")
  expected <- length(factor_names) * indicators_per_factor
  if (length(manifest_names) != expected) {
    stop("manifest_names must contain ", expected, " names for this measurement model", call. = FALSE)
  }
  split(manifest_names, rep(seq_along(factor_names), each = indicators_per_factor))
}

ctgui_measurement_dynamic_col <- function(structure, i) {
  switch(structure,
    linear_growth = (2L * i) - 1L,
    dynamic_var = i,
    dynamic_var_trend = (2L * i) - 1L
  )
}

ctgui_measurement_trend_col <- function(structure, i, n) {
  if (identical(structure, "dynamic_var_trend")) return(2L * i)
  NULL
}

ctgui_measurement_loadings <- function(measurement, block, factor_name, options, block_index) {
  if (measurement == "single_indicator") return(1)
  if (measurement == "marker") {
    out <- rep(0, length(block))
    out[1L] <- 1
    if (length(out) > 1L) out[-1L] <- paste0("lambda_", block[-1L], "_", factor_name)
    return(out)
  }
  fixed <- options$fixed_loadings
  if (is.null(fixed)) stop("options$fixed_loadings is required for fixed_loadings measurement", call. = FALSE)
  if (is.list(fixed)) fixed <- fixed[[block_index]]
  if (is.matrix(fixed)) fixed <- fixed[seq_along(block), block_index]
  if (length(fixed) != length(block)) stop("fixed loadings must match the number of indicators per factor", call. = FALSE)
  fixed
}

ctgui_linear_growth_matrices <- function(factor_names, latent_names, measurement_spec) {
  nlatent <- length(latent_names)
  drift <- matrix(0, nlatent, nlatent, dimnames = list(latent_names, latent_names))
  for (i in seq_along(factor_names)) drift[(2L * i) - 1L, 2L * i] <- 1
  list(
    LAMBDA = measurement_spec$LAMBDA,
    T0VAR = ctgui_lower_label_matrix("t0var", latent_names),
    T0MEANS = ctgui_label_matrix("T0MEANS", latent_names, "mean", ncol = 1L),
    MANIFESTMEANS = ctgui_fixed_matrix(0, measurement_spec$manifest_names, "MANIFESTMEANS", ncol = 1L),
    MANIFESTVAR = ctgui_diag_label_matrix("MANIFESTVAR", measurement_spec$manifest_names),
    DRIFT = drift,
    CINT = ctgui_fixed_matrix(0, latent_names, "CINT", ncol = 1L),
    DIFFUSION = matrix(0, nlatent, nlatent, dimnames = list(latent_names, latent_names))
  )
}

ctgui_dynamic_var_matrices <- function(factor_names, latent_names, measurement_spec, options) {
  nlatent <- length(latent_names)
  drift <- matrix("", nlatent, nlatent, dimnames = list(latent_names, latent_names))
  for (r in seq_len(nlatent)) {
    for (c in seq_len(nlatent)) {
      drift[r, c] <- if (r == c) paste0("auto_", latent_names[r]) else paste0("cross_", latent_names[c], "_to_", latent_names[r])
    }
  }
  list(
    LAMBDA = measurement_spec$LAMBDA,
    T0VAR = ctgui_lower_label_matrix("t0var", latent_names),
    T0MEANS = ctgui_label_matrix("T0MEANS", latent_names, "mean", ncol = 1L),
    MANIFESTMEANS = ctgui_fixed_matrix(0, measurement_spec$manifest_names, "MANIFESTMEANS", ncol = 1L),
    MANIFESTVAR = ctgui_diag_label_matrix("MANIFESTVAR", measurement_spec$manifest_names),
    DRIFT = drift,
    CINT = ctgui_fixed_matrix(0, latent_names, "CINT", ncol = 1L),
    DIFFUSION = ctgui_diffusion_matrix(latent_names, ctgui_free_noise_correlations(options))
  )
}

ctgui_dynamic_var_trend_matrices <- function(factor_names, latent_names, measurement_spec, options) {
  n <- length(factor_names)
  nlatent <- length(latent_names)
  trend_type <- match.arg(options$trend_type %||% "linear", c("linear", "exponential"))
  drift <- matrix(0, nlatent, nlatent, dimnames = list(latent_names, latent_names))
  for (target in seq_len(n)) {
    target_row <- (2L * target) - 1L
    for (source in seq_len(n)) {
      source_col <- (2L * source) - 1L
      drift[target_row, source_col] <- if (target == source) {
        paste0("auto_", factor_names[target])
      } else {
        paste0("cross_", factor_names[source], "_to_", factor_names[target])
      }
    }
    trend_col <- 2L * target
    drift[target_row, trend_col] <- ctgui_trend_coupling_value(options$trend_coupling, factor_names[target])
    drift[trend_col, trend_col] <- if (trend_type == "linear") 0 else paste0("trend_decay_", factor_names[target])
  }
  list(
    LAMBDA = measurement_spec$LAMBDA,
    T0VAR = ctgui_lower_label_matrix("t0var", latent_names),
    T0MEANS = ctgui_label_matrix("T0MEANS", latent_names, "mean", ncol = 1L),
    MANIFESTMEANS = ctgui_fixed_matrix(0, measurement_spec$manifest_names, "MANIFESTMEANS", ncol = 1L),
    MANIFESTVAR = ctgui_diag_label_matrix("MANIFESTVAR", measurement_spec$manifest_names),
    DRIFT = drift,
    CINT = ctgui_fixed_matrix(0, latent_names, "CINT", ncol = 1L),
    DIFFUSION = ctgui_diffusion_matrix(latent_names, ctgui_free_noise_correlations(options))
  )
}

ctgui_free_noise_correlations <- function(options) {
  !identical(options$free_noise_correlations, FALSE)
}

ctgui_trend_coupling_value <- function(value, factor_name) {
  if (is.null(value) || identical(value, "fixed")) return(1)
  if (identical(value, "free")) return(paste0("trend_to_", factor_name))
  value
}

ctgui_spec_manifest_blocks <- function(blocks, manifest_names, factor_latents) {
  if (is.null(blocks)) {
    if (length(manifest_names) < length(factor_latents)) {
      stop("Need at least one manifest per selected factor latent", call. = FALSE)
    }
    return(as.list(manifest_names[seq_along(factor_latents)]))
  }
  if (is.character(blocks)) {
    blocks <- strsplit(blocks, ";", fixed = TRUE)[[1]]
    blocks <- lapply(blocks, function(x) ctgui_as_names(strsplit(x, ",", fixed = TRUE)[[1]], "manifest_blocks"))
  }
  if (!is.list(blocks) || length(blocks) != length(factor_latents)) {
    stop("manifest_blocks must have one block per selected factor latent", call. = FALSE)
  }
  blocks <- lapply(blocks, function(x) ctgui_as_names(x, "manifest_blocks"))
  missing <- setdiff(unique(unlist(blocks, use.names = FALSE)), manifest_names)
  if (length(missing)) stop("manifest_blocks contains unknown manifest names: ", paste(missing, collapse = ", "), call. = FALSE)
  blocks
}

ctgui_diffusion_matrix <- function(latent_names, free_correlations = FALSE, active_latents = latent_names) {
  mat <- matrix(0, length(latent_names), length(latent_names), dimnames = list(latent_names, latent_names))
  active_latents <- intersect(active_latents, latent_names)
  for (name in active_latents) mat[name, name] <- paste0("system_noise_", name)
  if (isTRUE(free_correlations) && length(active_latents) > 1L) {
    for (r in seq_along(active_latents)) {
      for (c in seq_len(r - 1L)) mat[active_latents[r], active_latents[c]] <- paste0("noise_cor_", active_latents[r], "_", active_latents[c])
    }
  }
  mat
}

ctgui_cell_active <- function(x) {
  if (length(x) == 0L || is.na(x)) return(FALSE)
  numeric <- suppressWarnings(as.numeric(x))
  if (!is.na(numeric)) return(numeric != 0)
  nzchar(trimws(as.character(x))) && trimws(as.character(x)) != "0"
}

ctgui_edges_from_matrix <- function(mat, directed, element) {
  if (is.null(mat) || !is.matrix(mat)) return(NULL)
  edges <- list()
  for (r in seq_len(nrow(mat))) {
    for (c in seq_len(ncol(mat))) {
      if (!ctgui_cell_active(mat[r, c])) next
      edges[[length(edges) + 1L]] <- data.frame(
        from = colnames(mat)[c] %||% as.character(c),
        to = rownames(mat)[r] %||% as.character(r),
        value = as.character(mat[r, c]),
        directed = directed,
        element = element,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(edges)) return(NULL)
  do.call(rbind, edges)
}

ctgui_diffusion_edges <- function(spec) {
  mat <- spec$matrices$DIFFUSION
  if (is.null(mat) || !is.matrix(mat)) return(NULL)
  edges <- list()
  for (i in seq_len(min(nrow(mat), ncol(mat)))) {
    if (!ctgui_cell_active(mat[i, i])) next
    edges[[length(edges) + 1L]] <- data.frame(
      from = rownames(mat)[i],
      to = colnames(mat)[i],
      value = as.character(mat[i, i]),
      directed = FALSE,
      element = "DIFFUSION noise",
      stringsAsFactors = FALSE
    )
  }
  if (nrow(mat) < 2L) {
    if (!length(edges)) return(NULL)
    return(do.call(rbind, edges))
  }
  for (r in seq_len(nrow(mat))) {
    for (c in seq_len(r - 1L)) {
      if (!ctgui_cell_active(mat[r, c])) next
      edges[[length(edges) + 1L]] <- data.frame(
        from = rownames(mat)[r],
        to = colnames(mat)[c],
        value = as.character(mat[r, c]),
        directed = FALSE,
        element = "DIFFUSION correlation",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(edges)) return(NULL)
  do.call(rbind, edges)
}

ctgui_edges_from_lambda <- function(spec) {
  mat <- spec$matrices$LAMBDA
  if (is.null(mat) || !is.matrix(mat)) return(NULL)
  edges <- list()
  for (r in seq_len(nrow(mat))) {
    for (c in seq_len(ncol(mat))) {
      if (!ctgui_cell_active(mat[r, c])) next
      edges[[length(edges) + 1L]] <- data.frame(
        from = colnames(mat)[c],
        to = rownames(mat)[r],
        value = as.character(mat[r, c]),
        directed = TRUE,
        element = "LAMBDA",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(edges)) return(NULL)
  do.call(rbind, edges)
}

ctgui_trend_edges <- function(spec) {
  if (is.null(spec$builder) || !identical(spec$builder$structure, "dynamic_var_trend")) return(NULL)
  drift_edges <- ctgui_edges_from_matrix(spec$matrices$DRIFT, directed = TRUE, element = "trend DRIFT")
  if (is.null(drift_edges)) return(NULL)
  trend_names <- spec$builder$trend_latents %||% grep("_trend$", spec$latent_names, value = TRUE)
  drift_edges[drift_edges$from %in% trend_names | drift_edges$to %in% trend_names, , drop = FALSE]
}
