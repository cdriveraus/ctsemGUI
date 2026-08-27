ctgui_half_life <- getFromNamespace("ctgui_half_life", "ctsemGUI")
ctgui_discrete_drift <- getFromNamespace("ctgui_discrete_drift", "ctsemGUI")
ctgui_cross_effect_peak <- getFromNamespace("ctgui_cross_effect_peak", "ctsemGUI")
ctgui_observed_intervals <- getFromNamespace("ctgui_observed_intervals", "ctsemGUI")
ctgui_interpret_persistence <- getFromNamespace("ctgui_interpret_persistence", "ctsemGUI")
ctgui_interpret_cross_effects <- getFromNamespace("ctgui_interpret_cross_effects", "ctsemGUI")
ctgui_interpret_timescale <- getFromNamespace("ctgui_interpret_timescale", "ctsemGUI")
ctgui_interpret_fit <- getFromNamespace("ctgui_interpret_fit", "ctsemGUI")
ctgui_cell_is_free <- getFromNamespace("ctgui_cell_is_free", "ctsemGUI")
ctgui_warning_guidance <- getFromNamespace("ctgui_warning_guidance", "ctsemGUI")

coupled_drift <- function() {
  matrix(c(-0.4, 0.1, -0.35, -0.25), 2, 2,
    dimnames = list(c("stress", "sleep"), c("stress", "sleep")))
}

growth_drift <- function() {
  matrix(c(0, 0, 1, 0), 2, 2,
    dimnames = list(c("level", "slope"), c("level", "slope")))
}

all_free <- function(drift) {
  matrix(TRUE, nrow(drift), ncol(drift), dimnames = dimnames(drift))
}

texts <- function(notes) vapply(notes, function(note) note$text, character(1L))
kinds <- function(notes) vapply(notes, function(note) note$kind, character(1L))

test_that("half-life is the time to lose half a disturbance", {
  # log(2) / 0.5 is about 1.386.
  expect_equal(ctgui_half_life(-0.5), log(2) / 0.5)
  expect_equal(ctgui_half_life(-log(2)), 1)

  # A process that does not decay has no half-life to report.
  expect_true(is.na(ctgui_half_life(0)))
  expect_true(is.na(ctgui_half_life(0.3)))
  expect_true(is.na(ctgui_half_life(NA_real_)))
})

test_that("the discrete drift is right where it can be checked by hand", {
  # A growth curve's drift is nilpotent, so exp(A*dt) is exactly I + A*dt. An
  # eigendecomposition cannot do this one at all, which is why it is not used.
  discrete <- ctgui_discrete_drift(growth_drift(), 2)
  expect_equal(discrete[["level", "slope"]], 2)
  expect_equal(discrete[["level", "level"]], 1)
  expect_equal(discrete[["slope", "slope"]], 1)

  # A pure decay of rate r over dt is exp(r * dt).
  decay <- matrix(-0.5, 1, 1, dimnames = list("a", "a"))
  expect_equal(ctgui_discrete_drift(decay, 3)[[1L, 1L]], exp(-1.5))

  expect_null(ctgui_discrete_drift(NULL, 1))
  expect_null(ctgui_discrete_drift(coupled_drift(), NA_real_))
})

test_that("a cross-effect peak is found, and a rising curve is not called one", {
  peak <- ctgui_cross_effect_peak(coupled_drift(), "stress", "sleep")
  expect_gt(peak$time, 0)
  expect_false(peak$at_edge)
  # The peak must be at least as large as the effect at any other interval.
  expect_gte(abs(peak$value), abs(ctgui_discrete_drift(coupled_drift(), 1)[["stress", "sleep"]]))

  # A growth curve's level rises without bound, so there is no peak to report.
  rising <- ctgui_cross_effect_peak(growth_drift(), "level", "slope")
  expect_true(rising$at_edge)
})

test_that("observed intervals are measured within subjects, not across them", {
  data <- data.frame(
    id = c(1, 1, 1, 2, 2),
    time = c(0, 1, 3, 0, 10)
  )
  # Gaps are 1, 2 within subject 1 and 10 within subject 2. The jump from
  # subject 1's last row to subject 2's first is not an interval.
  intervals <- ctgui_observed_intervals(data, "id", "time")
  expect_equal(intervals$n, 3L)
  expect_equal(intervals$min, 1)
  expect_equal(intervals$max, 10)
  expect_equal(intervals$median, 2)

  expect_null(ctgui_observed_intervals(NULL, "id", "time"))
  expect_null(ctgui_observed_intervals(data, "missing", "time"))
})

test_that("a fixed zero on the diagonal is not read as a failure to decay", {
  # A growth curve's level and an oscillator's position are structural zeros
  # the user chose. Reading them as estimates raised alarm about
  # non-stationarity on models behaving exactly as specified.
  free <- all_free(growth_drift())
  diag(free) <- FALSE

  expect_length(ctgui_interpret_persistence(growth_drift(), NULL, free), 0L)

  # The same zero, if it had actually been estimated, is worth flagging.
  estimated <- ctgui_interpret_persistence(growth_drift(), NULL, all_free(growth_drift()))
  expect_true("caution" %in% kinds(estimated))
  expect_match(paste(texts(estimated), collapse = " "), "not negative", fixed = TRUE)
})

test_that("persistence is reported against the intervals actually observed", {
  intervals <- list(median = 1, min = 0.5, max = 2, n = 100L)
  notes <- ctgui_interpret_persistence(coupled_drift(), intervals, all_free(coupled_drift()))
  combined <- paste(texts(notes), collapse = " ")

  expect_match(combined, "half its size", fixed = TRUE)
  expect_match(combined, "median observation interval", fixed = TRUE)

  # A process far faster than the sampling rate is worth a word, because the
  # estimate is then leaning on the model rather than the data.
  fast <- matrix(-10, 1, 1, dimnames = list("a", "a"))
  quick <- ctgui_interpret_persistence(fast, list(median = 5, min = 4, max = 6, n = 10L), all_free(fast))
  expect_match(texts(quick)[[1L]], "between observations", fixed = TRUE)
})

test_that("a cross-effect leads with the interval the data can speak to", {
  intervals <- list(median = 1, min = 0.5, max = 2, n = 100L)
  notes <- ctgui_interpret_cross_effects(coupled_drift(), intervals)
  combined <- paste(texts(notes), collapse = " ")

  expect_match(combined, "over your median interval", fixed = TRUE)
  expect_match(combined, "largest at an interval", fixed = TRUE)

  # Said of every effect the extrapolation caveat stops carrying information,
  # so it appears only when the peak is well outside the observed range.
  wide <- ctgui_interpret_cross_effects(coupled_drift(), list(median = 1, min = 0.5, max = 50, n = 100L))
  expect_false(any(grepl("extrapolation", texts(wide), fixed = TRUE)))
})

test_that("a model with one process has no cross-effects to describe", {
  single <- matrix(-0.3, 1, 1, dimnames = list("a", "a"))
  expect_length(ctgui_interpret_cross_effects(single, NULL), 0L)
})

test_that("sampling far slower than the process is flagged once", {
  fast <- matrix(-5, 1, 1, dimnames = list("a", "a"))
  notes <- ctgui_interpret_timescale(fast, list(median = 10, min = 8, max = 12, n = 40L), all_free(fast))
  expect_true("caution" %in% kinds(notes))
  expect_match(paste(texts(notes), collapse = " "), "hard to identify", fixed = TRUE)

  # Structural zeros must not be mistaken for a very slow process here either.
  free <- all_free(growth_drift())
  diag(free) <- FALSE
  calm <- ctgui_interpret_timescale(growth_drift(), list(median = 1, min = 1, max = 1, n = 10L), free)
  expect_false("caution" %in% kinds(calm))
})

test_that("nothing is claimed when there is nothing to read", {
  expect_length(ctgui_interpret_fit(NULL), 0L)
  expect_true(ctgui_cell_is_free(NULL, "a", "b"))
})

test_that("warnings a user will actually meet are explained", {
  # This one fires on three of the six worked examples, including one that
  # recovers every parameter it was generated from. Without a word of context
  # a user cannot tell a benign one from a real problem.
  hessian <- ctgui_warning_guidance(paste(
    "Hessian covariance from Hessian required numerical repair:",
    "solve(-hessian) failed; information eigenvalues were floored at ridge=1e-08",
    "(minimum original eigenvalue=-7.027e-11)"
  ))
  expect_gte(length(hessian), 1L)
  titles <- vapply(hessian, function(entry) entry$title, character(1L))
  expect_true(any(grepl("Hessian", titles, fixed = TRUE)))
  # Guidance has to say what would distinguish the harmless case.
  expect_match(paste(vapply(hessian, function(e) e$text, character(1L)), collapse = " "),
    "refitting from", fixed = TRUE)

  t0var <- ctgui_warning_guidance(
    "Free T0VAR parameters as well as indvarying T0MEANS -- fixing T0VAR pars to diag matrix of 1e-6"
  )
  expect_gte(length(t0var), 1L)

  expect_length(ctgui_warning_guidance(character()), 0L)
  expect_length(ctgui_warning_guidance("No warnings."), 0L)
  expect_length(ctgui_warning_guidance("something entirely unrelated"), 0L)
})
