ctgui_blueprint <- getFromNamespace("ctgui_blueprint", "ctsemGUI")
ctgui_blueprint_apply <- getFromNamespace("ctgui_blueprint_apply", "ctsemGUI")
ctgui_blueprint_summary <- getFromNamespace("ctgui_blueprint_summary", "ctsemGUI")
ctgui_blueprint_structures <- getFromNamespace("ctgui_blueprint_structures", "ctsemGUI")

quiet_blueprint <- function(code) suppressWarnings(suppressMessages(force(code)))

empty_spec <- function() {
  quiet_blueprint(ctgui_spec(latent_names = character(), manifest_names = character()))
}

free_cells <- function(mat) sum(is.na(suppressWarnings(as.numeric(mat))))

test_that("every template builds a model ctsem accepts, from nothing", {
  # The point of the rebuilt templates is that they own their variables. A
  # template that needs the right latents to exist first is not a template.
  for (id in names(ctgui_blueprint_structures())) {
    blueprint <- ctgui_blueprint(id, processes = c("stress", "sleep"), indicators = 1L)
    spec <- quiet_blueprint(ctgui_blueprint_apply(empty_spec(), blueprint, "replace"))

    expect_true(length(spec$latent_names) > 0L, info = id)
    expect_true(length(spec$manifest_names) > 0L, info = id)
    expect_no_error(quiet_blueprint(ctgui_to_ctsem_model(spec)))

    validation <- quiet_blueprint(ctgui_validate(spec))
    problems <- validation[!tolower(validation$status) %in% c("ok", "pass"), , drop = FALSE]
    expect_equal(nrow(problems), 0L, info = id)
  }
})

test_that("each template has the dynamics that make it that template", {
  build <- function(id, ...) {
    quiet_blueprint(ctgui_blueprint_apply(
      empty_spec(), ctgui_blueprint(id, processes = c("stress", "sleep"), ...), "replace"
    ))
  }

  independent <- build("independent")$matrices$DRIFT
  expect_equal(independent["stress", "sleep"], "0")
  expect_equal(independent["sleep", "stress"], "0")
  expect_true(is.na(suppressWarnings(as.numeric(independent["stress", "stress"]))))

  coupled <- build("coupled")$matrices$DRIFT
  expect_equal(free_cells(coupled), 4L)

  trend <- build("coupled_trend")
  # A trend feeds its process and has no dynamics of its own...
  expect_equal(trend$matrices$DRIFT["stress", "stress_trend"], "1")
  expect_equal(trend$matrices$DRIFT["stress_trend", "stress_trend"], "0")
  expect_equal(free_cells(trend$matrices$DRIFT[c("stress_trend", "sleep_trend"), ]), 0L)
  # ...but rises at a rate of its own from zero. Without the rate it is a
  # constant input: a second random intercept, not a trend.
  expect_equal(free_cells(trend$matrices$CINT[c("stress_trend", "sleep_trend"), , drop = FALSE]), 2L)
  expect_equal(unname(trend$matrices$T0MEANS[c("stress_trend", "sleep_trend"), 1L]), c("0", "0"))
  expect_true(all(trend$matrices$T0VAR[c("stress_trend", "sleep_trend"), ] == "0"))
  expect_true(all(trend$matrices$T0VAR[, c("stress_trend", "sleep_trend")] == "0"))

  growth <- build("growth")
  expect_equal(growth$matrices$DRIFT["stress_level", "stress_slope"], "1")
  # A growth curve has no system noise; everything unexplained is measurement.
  expect_equal(free_cells(growth$matrices$DIFFUSION), 0L)
  expect_true(all(growth$matrices$DIFFUSION == "0"))
  # The level's subject-varying start is the intercept; a free manifest mean
  # beside it would be the same intercept twice.
  expect_equal(unname(growth$matrices$MANIFESTMEANS[, 1L]), c("0", "0"))
  growth_model <- quiet_blueprint(ctgui_to_ctsem_model(growth))
  growth_t0 <- growth_model$pars[growth_model$pars$matrix == "T0MEANS", ]
  expect_true(all(growth_t0$indvarying))

  oscillator <- build("oscillator")
  expect_equal(oscillator$matrices$DRIFT["stress", "stress_velocity"], "1")
  # Position pulls velocity back, which is what produces the cycles.
  expect_true(is.na(suppressWarnings(as.numeric(
    oscillator$matrices$DRIFT["stress_velocity", "stress"]
  ))))
})

test_that("system noise correlations are only free when asked for", {
  correlated <- quiet_blueprint(ctgui_blueprint_apply(
    empty_spec(), ctgui_blueprint("coupled", c("a", "b"), free_noise_correlations = TRUE), "replace"
  ))
  independent <- quiet_blueprint(ctgui_blueprint_apply(
    empty_spec(), ctgui_blueprint("coupled", c("a", "b"), free_noise_correlations = FALSE), "replace"
  ))

  expect_gt(free_cells(correlated$matrices$DIFFUSION), free_cells(independent$matrices$DIFFUSION))
  expect_equal(independent$matrices$DIFFUSION["b", "a"], "0")
})

test_that("extra indicators load on the measured latent, first fixed to scale it", {
  spec <- quiet_blueprint(ctgui_blueprint_apply(
    empty_spec(), ctgui_blueprint("growth", "stress", indicators = 3L), "replace"
  ))
  loading <- spec$matrices$LAMBDA

  expect_equal(rownames(loading), c("stress_1", "stress_2", "stress_3"))
  expect_equal(loading["stress_1", "stress_level"], "1")
  expect_true(is.na(suppressWarnings(as.numeric(loading["stress_2", "stress_level"]))))
  # Indicators measure the level, never the slope.
  expect_true(all(loading[, "stress_slope"] == "0"))
})

test_that("extending keeps the model already built", {
  base <- quiet_blueprint(ctgui_blueprint_apply(
    empty_spec(), ctgui_blueprint("coupled", c("stress", "sleep")), "replace"
  ))
  base <- quiet_blueprint(ctgui_set_matrix_value(base, "DRIFT", "stress", "sleep", label = "hand_edit"))

  extended <- quiet_blueprint(ctgui_blueprint_apply(
    base, ctgui_blueprint("oscillator", "mood", indicators = 2L), "extend"
  ))

  expect_equal(extended$matrices$DRIFT["stress", "sleep"], "hand_edit")
  expect_true(all(c("stress", "sleep", "mood", "mood_velocity") %in% extended$latent_names))
  expect_true(all(c("mood_1", "mood_2") %in% extended$manifest_names))
  expect_no_error(quiet_blueprint(ctgui_to_ctsem_model(extended)))
})

test_that("extending leaves connections to the existing model for the user to make", {
  base <- quiet_blueprint(ctgui_blueprint_apply(
    empty_spec(), ctgui_blueprint("coupled", "stress"), "replace"
  ))

  # A resize gives new cells free default labels. Silently freeing every
  # connection between the old and new processes would be a modelling decision
  # the template has no business making.
  separate <- quiet_blueprint(ctgui_blueprint_apply(
    base, ctgui_blueprint("coupled", "mood", connect_existing = FALSE), "extend"
  ))
  expect_equal(separate$matrices$DRIFT["stress", "mood"], "0")
  expect_equal(separate$matrices$DRIFT["mood", "stress"], "0")

  connected <- quiet_blueprint(ctgui_blueprint_apply(
    base, ctgui_blueprint("coupled", "mood", connect_existing = TRUE), "extend"
  ))
  expect_true(is.na(suppressWarnings(as.numeric(connected$matrices$DRIFT["stress", "mood"]))))
})

test_that("extending refuses to collide with names already in the model", {
  base <- quiet_blueprint(ctgui_blueprint_apply(
    empty_spec(), ctgui_blueprint("coupled", c("stress", "sleep")), "replace"
  ))

  expect_error(
    quiet_blueprint(ctgui_blueprint_apply(base, ctgui_blueprint("coupled", "stress"), "extend")),
    "already in the model"
  )
})

test_that("data roles and predictors survive building a fresh model", {
  spec <- quiet_blueprint(ctgui_spec(
    latent_names = "old", manifest_names = "yold",
    id = "subject", time = "day", tipred_names = "age", type = "dt"
  ))
  built <- quiet_blueprint(ctgui_blueprint_apply(
    spec, ctgui_blueprint("coupled", c("stress", "sleep")), "replace"
  ))

  # These describe the data and the study, not the dynamics being replaced.
  expect_equal(built$id, "subject")
  expect_equal(built$time, "day")
  expect_equal(built$tipred_names, "age")
  expect_equal(built$type, "dt")
  expect_false("old" %in% built$latent_names)
})

test_that("the summary says what will happen before it happens", {
  base <- quiet_blueprint(ctgui_blueprint_apply(
    empty_spec(), ctgui_blueprint("coupled", "stress"), "replace"
  ))

  replacing <- ctgui_blueprint_summary(base, ctgui_blueprint("growth", c("a", "b")), "replace")
  expect_match(replacing, "replaces the current model", fixed = TRUE)
  expect_match(replacing, "a_level", fixed = TRUE)

  extending <- ctgui_blueprint_summary(base, ctgui_blueprint("coupled", "mood"), "extend")
  expect_match(extending, "added to the current model", fixed = TRUE)
  expect_match(extending, "fixed to zero", fixed = TRUE)
})

test_that("a blueprint refuses input it cannot build from", {
  expect_error(ctgui_blueprint("not_a_shape"), "structure must be one of")
  expect_error(ctgui_blueprint("coupled", character()), "at least one name")
  expect_error(ctgui_blueprint("coupled", c("a", "a")), "must be unique")
  expect_error(ctgui_blueprint("coupled", "a", indicators = 0L), "at least 1")
})

test_that("every template is described at both explanation levels", {
  for (definition in ctgui_blueprint_structures()) {
    expect_true(nzchar(definition$title))
    expect_true(nzchar(definition$summary))
    expect_true(nzchar(definition$detail))
    expect_lt(nchar(definition$summary), nchar(definition$detail))
  }
})

test_that("an unmeasured latent that drives a measured one is not flagged", {
  # Velocities, slopes and trends are never observed directly; that is the
  # point of them. Warning about every one would train users to ignore the
  # validation table, which is the opposite of what it is for.
  for (id in c("coupled_trend", "growth", "oscillator")) {
    spec <- quiet_blueprint(ctgui_blueprint_apply(
      empty_spec(), ctgui_blueprint(id, c("stress", "sleep")), "replace"
    ))
    validation <- quiet_blueprint(ctgui_validate(spec))
    expect_equal(nrow(validation[validation$field == "LAMBDA", , drop = FALSE]), 0L, info = id)
  }
})

test_that("a latent that reaches nothing observed is still flagged, with a fix", {
  ctgui_respec_preserving <- getFromNamespace("ctgui_respec_preserving", "ctsemGUI")
  spec <- quiet_blueprint(ctgui_blueprint_apply(
    empty_spec(), ctgui_blueprint("coupled", c("stress", "sleep")), "replace"
  ))
  orphaned <- quiet_blueprint(ctgui_respec_preserving(
    spec, latent_names = c("stress", "sleep", "ghost"), manifest_names = spec$manifest_names
  ))
  orphaned$matrices$DRIFT[, "ghost"] <- 0
  orphaned$matrices$LAMBDA[, "ghost"] <- 0

  validation <- quiet_blueprint(ctgui_validate(orphaned))
  messages <- validation[validation$field == "LAMBDA", "message"]
  expect_length(messages, 1L)
  expect_match(messages[[1L]], "ghost", fixed = TRUE)
  expect_match(messages[[1L]], "does not influence any measured process", fixed = TRUE)
  # A check that only reports a problem leaves the reader stuck.
  expect_match(messages[[1L]], "Give it a manifest loading", fixed = TRUE)
})

test_that("a template measures each process with the variables chosen for it", {
  # Choosing data columns is the point: a template that only numbers its
  # indicators leaves every one to be renamed to match the data afterwards.
  blueprint <- ctgui_blueprint("coupled", c("stress", "sleep"),
    manifests = list(c("Y1", "Y2"), NULL))
  spec <- quiet_blueprint(ctgui_blueprint_apply(empty_spec(), blueprint, "replace"))
  expect_equal(spec$manifest_names, c("Y1", "Y2", "sleep_1"))
  expect_equal(unname(spec$matrices$LAMBDA["Y1", "stress"]), "1")
  expect_true(is.na(suppressWarnings(as.numeric(spec$matrices$LAMBDA["Y2", "stress"]))))
  expect_equal(unname(spec$matrices$LAMBDA["Y1", "sleep"]), "0")
  expect_equal(unname(spec$matrices$LAMBDA["sleep_1", "sleep"]), "1")

  summary <- ctgui_blueprint_summary(empty_spec(), blueprint, "replace")
  expect_match(summary, "2, 1 indicators respectively", fixed = TRUE)
  expect_match(summary, "Manifest variables: Y1, Y2, sleep_1", fixed = TRUE)
})

test_that("chosen manifests that cannot be built say why", {
  expect_error(ctgui_blueprint("coupled", c("a", "b"), manifests = list("x", "x")),
    "only one process: x", fixed = TRUE)
  # Latents and manifests share one namespace in ctsem.
  expect_error(ctgui_blueprint("coupled", c("a", "b"), manifests = list("b", NULL)),
    "cannot name both a process and a manifest variable", fixed = TRUE)
})

test_that("a template offers data columns, and keeps existing manifests when extending", {
  ctgui_build_manifest_choices <- getFromNamespace("ctgui_build_manifest_choices", "ctsemGUI")
  data <- data.frame(id = 1, time = 0, Y1 = 1, Y2 = 2)
  spec <- quiet_blueprint(ctgui_spec(latent_names = "eta", manifest_names = "Y1"))
  expect_equal(ctgui_build_manifest_choices(data, spec, "replace"), c("Y1", "Y2"))
  expect_equal(ctgui_build_manifest_choices(data, spec, "extend"), "Y2")
})
