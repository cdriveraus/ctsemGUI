ctgui_history_is_model_change <- getFromNamespace("ctgui_history_is_model_change", "ctsemGUI")
ctgui_history_new <- getFromNamespace("ctgui_history_new", "ctsemGUI")
ctgui_history_push <- getFromNamespace("ctgui_history_push", "ctsemGUI")
ctgui_history_undo <- getFromNamespace("ctgui_history_undo", "ctsemGUI")
ctgui_history_redo <- getFromNamespace("ctgui_history_redo", "ctsemGUI")
ctgui_history_go <- getFromNamespace("ctgui_history_go", "ctsemGUI")
ctgui_history_current <- getFromNamespace("ctgui_history_current", "ctsemGUI")
ctgui_history_can_undo <- getFromNamespace("ctgui_history_can_undo", "ctsemGUI")
ctgui_history_can_redo <- getFromNamespace("ctgui_history_can_redo", "ctsemGUI")
ctgui_history_log <- getFromNamespace("ctgui_history_log", "ctsemGUI")
ctgui_history_describe <- getFromNamespace("ctgui_history_describe", "ctsemGUI")
ctgui_history_limit <- getFromNamespace("ctgui_history_limit", "ctsemGUI")

quiet_history <- function(code) suppressWarnings(suppressMessages(force(code)))

start_spec <- function() {
  quiet_history(ctgui_spec(latent_names = c("a", "b"), manifest_names = c("ya", "yb")))
}

test_that("undo and redo walk back and forward through committed states", {
  spec <- start_spec()
  history <- ctgui_history_new(spec)
  expect_false(ctgui_history_can_undo(history))
  expect_false(ctgui_history_can_redo(history))

  edited <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", label = "cross"))
  history <- ctgui_history_push(history, edited, "matrix-cell")

  expect_true(ctgui_history_can_undo(history))
  expect_equal(ctgui_history_current(history)$matrices$DRIFT["a", "b"], "cross")

  history <- ctgui_history_undo(history)
  expect_false(identical(ctgui_history_current(history)$matrices$DRIFT["a", "b"], "cross"))
  expect_true(ctgui_history_can_redo(history))

  history <- ctgui_history_redo(history)
  expect_equal(ctgui_history_current(history)$matrices$DRIFT["a", "b"], "cross")
})

test_that("editing from an earlier step discards the steps after it", {
  spec <- start_spec()
  history <- ctgui_history_new(spec)
  first <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", label = "one"))
  history <- ctgui_history_push(history, first, "matrix-cell")
  second <- quiet_history(ctgui_set_matrix_value(first, "DRIFT", "b", "a", label = "two"))
  history <- ctgui_history_push(history, second, "matrix-cell")

  history <- ctgui_history_undo(history)
  expect_true(ctgui_history_can_redo(history))

  # Once you change something from an earlier state, the abandoned future is
  # no longer reachable from where you are; every editor behaves this way.
  branched <- quiet_history(ctgui_set_matrix_value(
    ctgui_history_current(history), "DIFFUSION", "b", "a", value = 0
  ))
  history <- ctgui_history_push(history, branched, "matrix-cell")
  expect_false(ctgui_history_can_redo(history))
})

test_that("go to step moves anywhere in range and refuses anywhere else", {
  spec <- start_spec()
  history <- ctgui_history_new(spec)
  for (label in c("one", "two", "three")) {
    spec <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", label = label))
    history <- ctgui_history_push(history, spec, "matrix-cell")
  }

  history <- ctgui_history_go(history, 2L)
  expect_equal(ctgui_history_current(history)$matrices$DRIFT["a", "b"], "one")

  for (bad in list(0L, 99L, NA_integer_, "x", NULL)) {
    unchanged <- ctgui_history_go(history, bad)
    expect_equal(unchanged$position, history$position)
  }
})

test_that("the log says what each step actually did", {
  # A list of timestamps says nothing. The point of the log is that a user who
  # has been clicking around can see which step to go back to.
  spec <- start_spec()

  freed <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", label = "cross"))
  expect_match(ctgui_history_describe(spec, freed, "matrix-cell"), "Freed DRIFT[a, b] to cross", fixed = TRUE)

  fixed <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", value = 0))
  expect_match(ctgui_history_describe(spec, fixed, "matrix-cell"), "Fixed DRIFT[a, b] to 0", fixed = TRUE)

  added <- quiet_history(ctgui_add_spec_variable(spec, "manifest", "yc", measuring = "c")$spec)
  description <- ctgui_history_describe(spec, added, "add-manifest")
  expect_match(description, "Added manifest variable yc", fixed = TRUE)
  expect_match(description, "Added latent process c", fixed = TRUE)

  retimed <- spec
  retimed$type <- "dt"
  expect_match(ctgui_history_describe(spec, retimed, "specification"), "Time model set to dt", fixed = TRUE)
})

test_that("a wholesale rebuild is named by its action, not inventoried", {
  empty <- quiet_history(ctgui_spec(latent_names = character(), manifest_names = character()))
  built <- quiet_history(ctgui_blueprint_apply(
    empty, ctgui_blueprint("coupled", c("stress", "sleep")), "replace"
  ))

  description <- ctgui_history_describe(empty, built, "blueprint")
  expect_match(description, "Built from a template", fixed = TRUE)
  expect_match(description, "stress", fixed = TRUE)

  expect_match(
    ctgui_history_describe(empty, built, "example"), "Opened a worked example", fixed = TRUE
  )
})

test_that("a change that changed nothing still describes itself", {
  spec <- start_spec()
  expect_match(ctgui_history_describe(spec, spec, "visual"), "visual editor", fixed = TRUE)
})

test_that("the log reads newest first and marks where you are", {
  spec <- start_spec()
  history <- ctgui_history_new(spec)
  edited <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", label = "cross"))
  history <- ctgui_history_push(history, edited, "matrix-cell")
  history <- ctgui_history_undo(history)

  log <- ctgui_history_log(history)
  expect_equal(log$step, c(2L, 1L))
  expect_equal(log$state[log$step == 1L], "current")
  expect_equal(log$state[log$step == 2L], "undone")
})

test_that("history is bounded so a long session cannot grow without limit", {
  spec <- start_spec()
  history <- ctgui_history_new(spec)
  for (index in seq_len(ctgui_history_limit + 10L)) {
    spec <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", label = paste0("v", index)))
    history <- ctgui_history_push(history, spec, "matrix-cell")
  }
  expect_lte(length(history$entries), ctgui_history_limit)
  expect_equal(ctgui_history_current(history)$matrices$DRIFT["a", "b"],
    paste0("v", ctgui_history_limit + 10L))
})

test_that("laying out the graph is not a step in the model's history", {
  # Node positions are saved with the specification, so a relayout counts as a
  # change to it. Recording one would put a step in the log that undo appears
  # not to act on.
  spec <- start_spec()
  moved <- spec
  moved$visual <- list(layout = list(node = list(x = 10, y = 20)))

  expect_false(ctgui_history_is_model_change(spec, moved))
  expect_false(ctgui_history_is_model_change(spec, spec))

  edited <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", label = "cross"))
  expect_true(ctgui_history_is_model_change(spec, edited))
})

test_that("parameter annotations are logged as the model changes they are", {
  spec <- quiet_history(ctgui_spec(
    latent_names = c("a", "b"), manifest_names = c("ya", "yb"),
    tipred_names = "age", tipredDefault = FALSE
  ))
  spec <- quiet_history(ctgui_set_matrix_value(spec, "DRIFT", "a", "b", label = "cross"))

  annotated <- quiet_history(ctgui_set_parameter_metadata(
    spec, "DRIFT", "a", "b", indvarying = TRUE, transform = "exp(param)"
  ))
  description <- ctgui_history_describe(spec, annotated, "matrix-cell")
  expect_match(description, "RandomEffects", fixed = TRUE)
  expect_match(description, "DRIFT[a, b]", fixed = TRUE)

  moderated <- quiet_history(ctgui_set_parameter_metadata(
    spec, "DRIFT", "a", "b", tipred_effects = "age"
  ))
  expect_match(
    ctgui_history_describe(spec, moderated, "matrix-cell"),
    "age moderation", fixed = TRUE
  )
})

test_that("an empty field is the same model whether it is NULL or absent", {
  # Specifications built by different paths carry empty fields either as a
  # named NULL or by omitting the name. Both mean "nothing here", and treating
  # them as different put a step in the log for an action the user never took.
  spec <- start_spec()
  as_null <- spec
  as_null["parameter_metadata"] <- list(NULL)
  absent <- spec
  absent$parameter_metadata <- NULL

  expect_false(ctgui_history_is_model_change(as_null, absent))
})

test_that("one user action produces one history step", {
  skip_if_not_installed("shiny")

  server <- ctgui_app_server(
    quiet_history(ctgui_spec(latent_names = character(), manifest_names = character())),
    ctgui_help_catalog()
  )
  suppressWarnings(shiny::testServer(server, {
    session$setInputs(
      build_structure = "coupled", build_processes = "stress, sleep",
      build_indicators = 1, build_noise_correlations = TRUE, build_mode = "replace"
    )
    session$setInputs(build_apply = 1)

    expect_equal(length(spec_history()$entries), 2L)
    expect_equal(current_spec()$latent_names, c("stress", "sleep"))

    session$setInputs(history_undo = 1)
    expect_equal(current_spec()$latent_names, character())

    session$setInputs(history_redo = 1)
    expect_equal(current_spec()$latent_names, c("stress", "sleep"))
  }))
})

test_that("the fit comparison says what differs between the models", {
  skip_if_not_installed("shiny")
  ctgui_blueprint_apply <- getFromNamespace("ctgui_blueprint_apply", "ctsemGUI")

  empty <- quiet_history(ctgui_spec(latent_names = character(), manifest_names = character()))
  server <- ctgui_app_server(empty, ctgui_help_catalog())

  suppressWarnings(shiny::testServer(server, {
    base <- quiet_history(ctgui_blueprint_apply(
      empty, ctgui_blueprint("coupled", c("stress", "sleep")), "replace"
    ))
    nocross <- quiet_history(ctgui_set_matrix_value(base, "DRIFT", "stress", "sleep", value = 0))

    stub <- structure(list(), class = c("ctStanFit", "ctFit"))
    fit_registry(list(baseline = stub, nocross = stub, twin = stub))
    fit_specs(list(baseline = base, nocross = nocross, twin = base))

    table <- output$fit_comparison
    # Comparing fit statistics alone tells you which number is larger, not what
    # the difference is buying.
    expect_match(table, "baseline", fixed = TRUE)
    expect_match(table, "Fixed DRIFT[stress, sleep] to 0", fixed = TRUE)
    expect_match(table, "same model as baseline", fixed = TRUE)
  }))
})
