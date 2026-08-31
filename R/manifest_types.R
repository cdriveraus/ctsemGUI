# Measurement models for manifest variables ------------------------------------

# ctsem models a manifest variable as continuous, binary, ordinal, a count, or
# censored. Three of those need something the model cannot infer from the data:
# an ordinal variable needs its number of categories before any data is seen,
# because the model has to know how many thresholds to estimate; a censored
# variable needs the limits of the instrument, which are known constants rather
# than parameters.
#
# Those extra arguments are the whole difficulty. A type chosen without them
# fails at ctModel(), well after the point where the user made the choice, so
# they are collected beside the type wherever the type can be set.

ctgui_manifest_type_catalog <- function() {
  list(
    list(
      value = 0L, id = "continuous", label = "Continuous",
      short = "A numeric measurement with Gaussian error.",
      detail = paste(
        "The default. The observed value is the latent process plus normally",
        "distributed measurement error."
      ),
      needs = "none", backends = c("julia", "stan")
    ),
    list(
      value = 1L, id = "binary", label = "Binary",
      short = "Two categories, coded 0 and 1.",
      detail = paste(
        "The latent process drives the probability of a 1. Coded 0/1 in the",
        "data; anything else is a category count, which is ordinal instead."
      ),
      needs = "none", backends = c("julia", "stan")
    ),
    list(
      value = 2L, id = "ordinal", label = "Ordinal",
      short = "Ordered categories, coded 1, 2, 3, ... Needs a category count.",
      detail = paste(
        "Ordered categories with unknown spacing, such as a Likert item. The",
        "thresholds between categories are estimated, so the model needs to",
        "know how many there are before it sees any data. Code the data as",
        "consecutive integers starting at 1."
      ),
      needs = "ncategories", backends = "julia"
    ),
    list(
      value = 3L, id = "count", label = "Count",
      short = "Non-negative whole numbers, modelled as Poisson.",
      detail = paste(
        "Counts of events, modelled as Poisson with a log link, so the latent",
        "process is the log rate. Use this rather than treating a count as",
        "continuous when the numbers are small and the floor at zero matters."
      ),
      needs = "none", backends = "julia"
    ),
    list(
      value = 4L, id = "censored", label = "Censored",
      short = "Continuous, but only observed between known limits.",
      detail = paste(
        "A continuous variable that the instrument cannot record beyond a floor",
        "or a ceiling: a scale that bottoms out at zero, a test where several",
        "people score full marks. Values at a limit contribute the probability",
        "of being at or beyond it rather than a density. The limits are",
        "properties of the instrument, not parameters, so you supply them."
      ),
      needs = "censor", backends = "julia"
    )
  )
}

ctgui_manifest_type_entry <- function(value) {
  value <- suppressWarnings(as.integer(value))
  entries <- ctgui_manifest_type_catalog()
  match <- Filter(function(entry) identical(entry$value, value), entries)
  if (length(match)) match[[1L]] else entries[[1L]]
}

ctgui_manifest_type_choices <- function() {
  entries <- ctgui_manifest_type_catalog()
  stats::setNames(
    vapply(entries, function(entry) entry$value, integer(1L)),
    vapply(entries, function(entry) entry$label, character(1L))
  )
}

ctgui_manifest_type_label <- function(value) ctgui_manifest_type_entry(value)$label

ctgui_manifest_type_needs <- function(value) ctgui_manifest_type_entry(value)$needs

ctgui_manifest_type_is_default <- function(value) {
  identical(suppressWarnings(as.integer(value)), 0L)
}

# Which engines can fit a model containing these variables. Non-Gaussian
# measurement is implemented in the julia backend only, with binary also
# available in stan; a model that needs julia and is sent to stan fails at fit
# time with a message about a backend the user never chose.
ctgui_measurement_backends <- function(manifest_type) {
  if (!length(manifest_type)) return(c("julia", "stan"))
  Reduce(intersect, lapply(manifest_type, function(value) {
    ctgui_manifest_type_entry(value)$backends
  }))
}

ctgui_measurement_requires_julia <- function(manifest_type) {
  !"stan" %in% ctgui_measurement_backends(manifest_type)
}

# Which variables are the reason, so the message can name them rather than
# leaving the user to work it out.
ctgui_measurement_julia_only <- function(manifest_names, manifest_type) {
  if (!length(manifest_type)) return(character())
  needs_julia <- vapply(manifest_type, function(value) {
    !"stan" %in% ctgui_manifest_type_entry(value)$backends
  }, logical(1L))
  manifest_names[seq_along(needs_julia)][needs_julia]
}

ctgui_measurement_backend_message <- function(manifest_names, manifest_type) {
  offenders <- ctgui_measurement_julia_only(manifest_names, manifest_type)
  if (!length(offenders)) return("")
  labels <- vapply(manifest_type[match(offenders, manifest_names)],
    ctgui_manifest_type_label, character(1L))
  paste0(
    paste(paste0(offenders, " (", tolower(labels), ")"), collapse = ", "),
    if (length(offenders) == 1L) " needs " else " need ",
    "the Julia engine. Stan has no measurement model for these types."
  )
}

# Every manifest carries a value in each of these vectors, whether or not its
# type uses it, so they stay aligned with manifest_names through every resize.
ctgui_measurement_defaults <- function(n) {
  list(
    manifest_type = rep(0L, n),
    ncategories = rep(0L, n),
    censormin = rep(-Inf, n),
    censormax = rep(Inf, n)
  )
}

ctgui_measurement_fill <- function(values, n, default) {
  out <- rep(default, n)
  if (!length(values)) return(out)
  taken <- suppressWarnings(as.numeric(values))
  taken <- taken[seq_len(min(length(taken), n))]
  out[seq_along(taken)] <- taken
  out[!is.finite(out) & !is.infinite(out)] <- default
  out
}

#' Normalise the measurement description of a set of manifest variables
#'
#' @param manifest_names Manifest variable names.
#' @param manifest_type,ncategories,censormin,censormax Parallel vectors.
#' @return A list of the four vectors, each the length of `manifest_names`.
#' @keywords internal
ctgui_normalize_measurement <- function(manifest_names, manifest_type = NULL,
    ncategories = NULL, censormin = NULL, censormax = NULL) {
  n <- length(manifest_names)
  defaults <- ctgui_measurement_defaults(n)
  if (!n) return(defaults)

  types <- suppressWarnings(as.integer(ctgui_measurement_fill(manifest_type, n, 0)))
  types[is.na(types)] <- 0L
  known <- vapply(ctgui_manifest_type_catalog(), function(entry) entry$value, integer(1L))
  types[!types %in% known] <- 0L

  categories <- suppressWarnings(as.integer(ctgui_measurement_fill(ncategories, n, 0)))
  categories[is.na(categories) | categories < 0L] <- 0L
  # A count belongs to the type that uses it; carrying one on a continuous
  # variable would be written into the model and quietly ignored there.
  categories[types != 2L] <- 0L

  lower <- ctgui_measurement_fill(censormin, n, -Inf)
  upper <- ctgui_measurement_fill(censormax, n, Inf)
  lower[types != 4L] <- -Inf
  upper[types != 4L] <- Inf

  list(
    manifest_type = types, ncategories = categories,
    censormin = lower, censormax = upper
  )
}

# Problems a specification has before it ever reaches ctsem, so they can be
# reported where the choice was made rather than as a failure to build a model.
ctgui_measurement_problems <- function(manifest_names, manifest_type = NULL,
    ncategories = NULL, censormin = NULL, censormax = NULL) {
  measurement <- ctgui_normalize_measurement(
    manifest_names, manifest_type, ncategories, censormin, censormax
  )
  problems <- list()
  add <- function(field, message, severity = "error") {
    problems[[length(problems) + 1L]] <<- list(
      field = field, message = message, severity = severity
    )
  }

  for (index in seq_along(manifest_names)) {
    name <- manifest_names[index]
    type <- measurement$manifest_type[index]
    needs <- ctgui_manifest_type_needs(type)

    if (identical(needs, "ncategories") && measurement$ncategories[index] < 3L) {
      add("manifesttype", paste0(
        name, " is ordinal, so it needs a category count of at least 3. ",
        "The model estimates one threshold fewer than there are categories, ",
        "and has to know how many before it sees any data. A two-category ",
        "variable is binary rather than ordinal."
      ))
    }
    if (identical(needs, "censor") &&
        !is.finite(measurement$censormin[index]) &&
        !is.finite(measurement$censormax[index])) {
      add("manifesttype", paste0(
        name, " is censored, so it needs a lower limit, an upper limit, or ",
        "both. A variable censored on one side only leaves the other blank."
      ))
    }
    if (identical(needs, "censor") &&
        is.finite(measurement$censormin[index]) &&
        is.finite(measurement$censormax[index]) &&
        measurement$censormin[index] >= measurement$censormax[index]) {
      add("manifesttype", paste0(
        name, " has a censoring floor at or above its ceiling, so no value ",
        "could ever be observed."
      ))
    }
  }
  problems
}

# Identification traps that ctsem warns about at model construction, raised
# here instead so they appear beside the choice that caused them and name the
# cell to change.
ctgui_measurement_identification_problems <- function(manifest_names, manifest_type,
    manifest_means = NULL, latent_intercepts = NULL) {
  problems <- list()
  add <- function(message) {
    problems[[length(problems) + 1L]] <<- list(
      field = "manifesttype", message = message, severity = "warning"
    )
  }
  types <- suppressWarnings(as.integer(manifest_type))
  if (!length(types)) return(problems)

  free <- function(value) {
    text <- trimws(as.character(value %||% ""))
    nzchar(text) && is.na(suppressWarnings(as.numeric(strsplit(text, "|", fixed = TRUE)[[1L]][1L])))
  }

  ordinal <- which(types == 2L)
  if (length(ordinal) && !is.null(manifest_means)) {
    offenders <- manifest_names[ordinal][vapply(ordinal, function(i) {
      i <= length(manifest_means) && free(manifest_means[i])
    }, logical(1L))]
    if (length(offenders)) {
      add(paste0(
        "The thresholds of ", paste(offenders, collapse = ", "),
        " and their MANIFESTMEANS both describe where the categories sit, so ",
        "they are not separately identified. Fix MANIFESTMEANS to 0 for these ",
        "variables and let the thresholds carry the location."
      ))
    }
  }

  non_gaussian <- which(types > 0L)
  if (length(non_gaussian) && !is.null(latent_intercepts) &&
      !any(vapply(latent_intercepts, free, logical(1L)))) {
    add(paste0(
      "This model has non-continuous variables and every CINT entry is fixed. ",
      "The level of a non-Gaussian variable is usually carried by the latent ",
      "process rather than by a manifest intercept, so free the relevant CINT ",
      "entries, or fix the corresponding MANIFESTMEANS to 0."
    ))
  }
  problems
}

# The arguments ctModel needs beyond manifesttype. They are supplied only when
# a type actually uses them, so a model of ordinary continuous variables is
# built with the same call it always was.
ctgui_measurement_model_args <- function(measurement) {
  args <- list(manifesttype = measurement$manifest_type)
  if (any(measurement$manifest_type == 2L)) {
    args$ncategories <- measurement$ncategories
  }
  if (any(measurement$manifest_type == 4L)) {
    args$censormin <- measurement$censormin
    args$censormax <- measurement$censormax
  }
  args
}

# A short description of one variable's measurement, for a label or a node.
ctgui_measurement_summary <- function(name, type, ncategories = 0L,
    censormin = -Inf, censormax = Inf) {
  entry <- ctgui_manifest_type_entry(type)
  if (identical(entry$needs, "ncategories") && ncategories >= 2L) {
    return(paste0(entry$label, ", ", ncategories, " categories"))
  }
  if (identical(entry$needs, "censor")) {
    bounds <- c(
      if (is.finite(censormin)) paste0("from ", format(censormin)),
      if (is.finite(censormax)) paste0("to ", format(censormax))
    )
    if (length(bounds)) return(paste0(entry$label, ", ", paste(bounds, collapse = " ")))
  }
  entry$label
}
