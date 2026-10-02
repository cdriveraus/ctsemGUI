# Reading a fit in words -------------------------------------------------------

# What a graphical interface can add over a script is not another table of
# numbers. It is saying what the numbers mean in the units the user collected
# their data in: how long a disturbance takes to fade, when one process's
# influence on another peaks, whether the dynamics being reported are anywhere
# near the intervals actually observed.
#
# Everything here is arithmetic on the fitted drift matrix, stated plainly and
# hedged where it should be. It is off by default and never replaces the
# estimates: it sits beside them, and every statement names the quantity it
# came from so it can be checked.

ctgui_interpretation_digits <- 2L

ctgui_round <- function(x, digits = ctgui_interpretation_digits) {
  formatC(round(x, digits), format = "f", digits = digits)
}

# The fitted drift matrix, with latent names restored. summary() reports
# parmatrices by position, so the names come from the model.
ctgui_fitted_drift <- function(fit) {
  if (is.null(fit)) return(NULL)
  summary_fit <- tryCatch(suppressWarnings(summary(fit)), error = function(e) NULL)
  parmatrices <- ctgui_ctsem_object_path(summary_fit, "parmatrices")
  if (is.null(parmatrices)) return(NULL)
  parmatrices <- as.data.frame(parmatrices)
  rows <- parmatrices[parmatrices$matrix == "DRIFT", , drop = FALSE]
  if (!nrow(rows)) return(NULL)

  model <- ctgui_ctsem_fit_model(fit)
  latents <- model$latentNames
  size <- max(suppressWarnings(as.integer(c(rows$row, rows$col))), na.rm = TRUE)
  if (!length(latents) || length(latents) < size) return(NULL)

  drift <- matrix(NA_real_, size, size, dimnames = list(latents[seq_len(size)], latents[seq_len(size)]))
  for (index in seq_len(nrow(rows))) {
    drift[as.integer(rows$row[index]), as.integer(rows$col[index])] <-
      suppressWarnings(as.numeric(rows$Mean[index]))
  }
  drift
}

# Which drift cells were estimated rather than fixed by the specification.
#
# A growth curve's auto-effects and an oscillator's position row are structural
# zeros the user chose, not findings. Reading them as estimates produces alarm
# about non-stationarity on models that are behaving exactly as specified.
ctgui_free_drift_cells <- function(fit) {
  model <- ctgui_ctsem_fit_model(fit)
  pars <- ctgui_ctsem_object_path(model, "pars")
  latents <- ctgui_ctsem_object_path(model, "latentNames")
  if (is.null(pars) || !length(latents)) return(NULL)
  pars <- as.data.frame(pars)
  rows <- pars[pars$matrix == "DRIFT", , drop = FALSE]
  if (!nrow(rows)) return(NULL)

  size <- length(latents)
  free <- matrix(FALSE, size, size, dimnames = list(latents, latents))
  for (index in seq_len(nrow(rows))) {
    r <- suppressWarnings(as.integer(rows$row[index]))
    c <- suppressWarnings(as.integer(rows$col[index]))
    if (is.na(r) || is.na(c) || r > size || c > size) next
    free[r, c] <- !is.na(rows$param[index])
  }
  free
}

ctgui_cell_is_free <- function(free, row, col) {
  is.null(free) || isTRUE(free[row, col])
}

# An auto-effect is a continuous-time rate. The time for a disturbance to fall
# to half its size is log(2) / -rate, which is a quantity in the user's own
# time units rather than a coefficient whose scale depends on the interval.
ctgui_half_life <- function(auto_effect) {
  if (!is.finite(auto_effect) || auto_effect >= 0) return(NA_real_)
  log(2) / -auto_effect
}

# The discrete-time drift at one interval.
#
# Matrix::expm rather than an eigendecomposition: a growth curve's drift is
# nilpotent and a defective matrix has no eigenbasis to decompose, so the
# eigen route fails on exactly the model shape most likely to be read this
# way. Matrix is a recommended package, so this costs no new dependency.
ctgui_discrete_drift <- function(drift, dt) {
  if (is.null(drift) || !is.finite(dt)) return(NULL)
  tryCatch(as.matrix(Matrix::expm(drift * dt)), error = function(e) NULL)
}

# Where a cross-effect's influence is largest, over a grid of intervals. A
# continuous-time model implies a different coefficient at every interval; the
# peak is the one number that describes that curve without pretending it is
# flat.
#
# This works from the point estimate, so it is fast and deterministic and can
# be recomputed as the panel is opened. The uncertainty around it lives in the
# summary and the dynamics plot, and the panel says so.
ctgui_cross_effect_peak <- function(drift, row, col, times = seq(0.05, 20, by = 0.05)) {
  values <- vapply(times, function(dt) {
    discrete <- ctgui_discrete_drift(drift, dt)
    if (is.null(discrete)) return(NA_real_)
    discrete[row, col]
  }, numeric(1L))
  if (all(is.na(values))) return(NULL)
  index <- which.max(abs(values))
  # A peak at the very end of the grid is a curve still climbing, not a peak.
  list(time = times[index], value = values[index], at_edge = index == length(times))
}

ctgui_observed_intervals <- function(data, id, time) {
  if (!is.data.frame(data) || !all(c(id, time) %in% names(data))) return(NULL)
  frame <- data[order(data[[id]], data[[time]]), c(id, time), drop = FALSE]
  gaps <- unlist(lapply(split(frame[[time]], frame[[id]]), function(times) {
    if (length(times) < 2L) return(numeric())
    diff(times)
  }), use.names = FALSE)
  gaps <- gaps[is.finite(gaps) & gaps > 0]
  if (!length(gaps)) return(NULL)
  list(
    median = stats::median(gaps),
    min = min(gaps),
    max = max(gaps),
    n = length(gaps)
  )
}

ctgui_interpretation_note <- function(text, kind = "reading") {
  list(text = text, kind = kind)
}

# Persistence: how long each process takes to forget a disturbance.
ctgui_interpret_persistence <- function(drift, intervals = NULL, free = NULL) {
  if (is.null(drift)) return(list())
  Filter(Negate(is.null), lapply(rownames(drift), function(name) {
    rate <- drift[name, name]
    if (!is.finite(rate)) return(NULL)
    # A fixed zero on the diagonal is a modelling choice -- a growth curve's
    # level, an oscillator's position -- not a process failing to decay.
    if (!ctgui_cell_is_free(free, name, name)) return(NULL)
    # Read alone, an auto-effect describes one process. With other processes
    # feeding back into it, neither its sign nor its half-life settles how the
    # system behaves, so the reading says which it is describing.
    coupled <- nrow(drift) > 1L
    if (rate >= 0) {
      return(ctgui_interpretation_note(paste0(
        "The auto-effect on ", name, " is ", ctgui_round(rate), ", which is not negative. ",
        "On its own that would mean a disturbance to ", name, " does not die away",
        if (coupled) {
          paste0("; whether the system as a whole settles also depends on the effects ",
            "between processes, which Diagnostics > Dynamics shows")
        },
        ". Check the interval around this estimate before reading much into it."
      ), kind = "caution"))
    }
    half <- ctgui_half_life(rate)
    text <- paste0(
      if (coupled) "On its auto-effect alone (" else "With an auto-effect of ",
      ctgui_round(rate), if (coupled) "), " else ", ",
      "a disturbance to ", name, " falls to half its size in about ",
      ctgui_round(half), " time units",
      if (coupled) "; feedback through the other processes can lengthen or shorten that" else "",
      "."
    )
    if (!is.null(intervals) && is.finite(intervals$median)) {
      text <- paste0(text, " Your median observation interval is ",
        ctgui_round(intervals$median), ".")
      if (half < intervals$median / 2) {
        text <- paste0(text,
          " That is well under half an interval, so much of this process's ",
          "movement may happen between observations, and the estimate then rests ",
          "more on the model than on what was measured.")
      }
    }
    ctgui_interpretation_note(text)
  }))
}

# Transmission: when one process's influence on another is at its strongest.
ctgui_interpret_cross_effects <- function(drift, intervals = NULL) {
  if (is.null(drift) || nrow(drift) < 2L) return(list())
  notes <- list()
  names_ <- rownames(drift)
  for (target in seq_along(names_)) for (source in seq_along(names_)) {
    if (target == source) next
    rate <- drift[target, source]
    if (!is.finite(rate) || isTRUE(all.equal(rate, 0))) next
    peak <- ctgui_cross_effect_peak(drift, target, source)
    if (is.null(peak)) next
    direction <- if (peak$value >= 0) "raises" else "lowers"

    # Lead with the effect at the interval the user actually observed, because
    # that is the number their data speak to. The peak describes the shape of
    # the curve and comes second.
    text <- paste0("In the fitted model, a unit change in ", names_[source], " ",
      direction, " ", names_[target])
    if (!is.null(intervals) && is.finite(intervals$median)) {
      at_median <- ctgui_discrete_drift(drift, intervals$median)
      if (!is.null(at_median)) {
        text <- paste0(text, " by ", ctgui_round(at_median[target, source]),
          " over your median interval of ", ctgui_round(intervals$median), ".")
      } else {
        text <- paste0(text, ".")
      }
    } else {
      text <- paste0(text, ".")
    }

    text <- if (isTRUE(peak$at_edge)) {
      paste0(text, " The effect is still growing at the longest interval examined (",
        ctgui_round(peak$time), "), so it peaks somewhere beyond that, which can ",
        "happen when one of the processes barely decays.")
    } else {
      paste0(text, " It is largest at an interval of about ", ctgui_round(peak$time),
        ", where it reaches ", ctgui_round(peak$value), ".")
    }

    # Only worth saying when the peak is well outside the observed range. Said
    # of every effect, as it was, it stops carrying information.
    if (!is.null(intervals) && is.finite(intervals$max) && peak$time > intervals$max * 1.5) {
      text <- paste0(text, " Your longest observed interval is ",
        ctgui_round(intervals$max), ", so that peak is an extrapolation.")
    }
    notes[[length(notes) + 1L]] <- ctgui_interpretation_note(text)
  }
  notes
}

# Whether the estimated dynamics are on a timescale the data can inform.
ctgui_interpret_timescale <- function(drift, intervals, free = NULL) {
  if (is.null(intervals)) return(list())
  notes <- list(ctgui_interpretation_note(paste0(
    "Observation intervals run from ", ctgui_round(intervals$min), " to ",
    ctgui_round(intervals$max), ", median ", ctgui_round(intervals$median),
    ", across ", intervals$n, " gaps. Effects quoted at intervals outside that ",
    "range are the model extrapolating."
  )))
  if (is.null(drift)) return(notes)

  autos <- diag(drift)
  if (!is.null(free)) autos <- autos[diag(free)]
  autos <- autos[is.finite(autos) & autos < 0]
  if (!length(autos)) return(notes)
  fastest <- ctgui_half_life(min(autos))
  if (is.finite(fastest) && fastest < intervals$median / 2) {
    notes[[length(notes) + 1L]] <- ctgui_interpretation_note(paste0(
      "On its auto-effect alone, the fastest process has a half-life of about ",
      ctgui_round(fastest), ", shorter than half your median interval. Sampling ",
      "much slower than a process moves can make its dynamics hard to identify: ",
      "check the intervals around these effects, and whether a simpler model ",
      "describes the data about as well."
    ), kind = "caution")
  }
  notes
}

#' Describe a fitted model in words
#'
#' @param fit A fitted ctsem model.
#' @param data The long data the fit was made on, used to report the observed
#'   interval range. Optional.
#' @param id,time Column names for subject and time in `data`.
#' @return A list of notes, each with `text` and a `kind` of `"reading"` or
#'   `"caution"`.
#' @keywords internal
ctgui_interpret_fit <- function(fit, data = NULL, id = "id", time = "time") {
  if (is.null(fit)) return(list())
  drift <- ctgui_fitted_drift(fit)
  free <- ctgui_free_drift_cells(fit)
  intervals <- ctgui_observed_intervals(data, id, time)
  if (is.null(drift) && is.null(intervals)) return(list())

  notes <- c(
    ctgui_interpret_persistence(drift, intervals, free),
    ctgui_interpret_cross_effects(drift, intervals),
    ctgui_interpret_timescale(drift, intervals, free)
  )
  Filter(Negate(is.null), notes)
}
