ctgui_autosave_write <- getFromNamespace("ctgui_autosave_write", "ctsemGUI")
ctgui_autosave_read <- getFromNamespace("ctgui_autosave_read", "ctsemGUI")
ctgui_autosave_dir <- getFromNamespace("ctgui_autosave_dir", "ctsemGUI")
ctgui_autosave_path <- getFromNamespace("ctgui_autosave_path", "ctsemGUI")
ctgui_autosave_clear <- getFromNamespace("ctgui_autosave_clear", "ctsemGUI")
ctgui_autosave_describe <- getFromNamespace("ctgui_autosave_describe", "ctsemGUI")
ctgui_autosave_worth_offering <- getFromNamespace("ctgui_autosave_worth_offering", "ctsemGUI")
ctgui_autosave_from_other_session <- getFromNamespace("ctgui_autosave_from_other_session", "ctsemGUI")
ctgui_autosave_data_fits <- getFromNamespace("ctgui_autosave_data_fits", "ctsemGUI")
ctgui_history_new <- getFromNamespace("ctgui_history_new", "ctsemGUI")

quiet_autosave <- function(code) suppressWarnings(suppressMessages(force(code)))

autosave_spec <- function() {
  quiet_autosave(ctgui_spec(
    latent_names = c("stress", "sleep"),
    manifest_names = c("stress_1", "sleep_1")
  ))
}

temp_autosave <- function() tempfile(fileext = ".rds")

test_that("a model written out comes back the same", {
  path <- temp_autosave()
  on.exit(unlink(path), add = TRUE)

  spec <- quiet_autosave(ctgui_set_matrix_value(autosave_spec(), "DRIFT", "stress", "sleep", label = "cross"))
  expect_true(ctgui_autosave_write(spec, path = path))

  state <- ctgui_autosave_read(path)
  expect_equal(state$spec$latent_names, c("stress", "sleep"))
  # The edits are the part that cannot be reconstructed from anything else.
  expect_equal(state$spec$matrices$DRIFT[["stress", "sleep"]], "cross")
})

test_that("the history comes back with the model", {
  path <- temp_autosave()
  on.exit(unlink(path), add = TRUE)

  spec <- autosave_spec()
  history <- ctgui_history_new(spec)
  ctgui_autosave_write(spec, history = history, path = path)

  expect_equal(length(ctgui_autosave_read(path)$history$entries), 1L)
})

test_that("data is kept when it is small and named when it is not", {
  path <- temp_autosave()
  on.exit(unlink(path), add = TRUE)

  small <- data.frame(id = 1:10, time = 1:10, y = rnorm(10))
  ctgui_autosave_write(autosave_spec(), data = small, data_name = "my data", path = path)
  kept <- ctgui_autosave_read(path)
  expect_equal(nrow(kept$data), 10L)
  expect_false(isTRUE(kept$data_omitted))

  # Writing a very large data frame on every model edit would stall the
  # interface, so past a limit only its name is remembered.
  expect_true(ctgui_autosave_data_fits(small))
  expect_false(ctgui_autosave_data_fits(matrix(0, 3000, 1000)))
})

test_that("a corrupt or foreign autosave is ignored rather than restored", {
  path <- temp_autosave()
  on.exit(unlink(path), add = TRUE)

  writeLines("not an rds file", path)
  expect_null(ctgui_autosave_read(path))

  saveRDS(list(version = 999L, spec = autosave_spec()), path)
  expect_null(ctgui_autosave_read(path))

  saveRDS(list(version = 1L, spec = NULL), path)
  expect_null(ctgui_autosave_read(path))

  expect_null(ctgui_autosave_read(temp_autosave()))
})

test_that("an untouched starting model is not offered back", {
  # Offering it would make every session begin with a question about nothing.
  empty <- quiet_autosave(ctgui_spec(latent_names = character(), manifest_names = character()))
  expect_false(ctgui_autosave_worth_offering(list(spec = empty)))
  expect_false(ctgui_autosave_worth_offering(NULL))

  expect_true(ctgui_autosave_worth_offering(list(spec = autosave_spec())))
})

test_that("this session's own autosave is not offered back to it", {
  # It is what this session just wrote, so recovering it recovers nothing.
  expect_false(ctgui_autosave_from_other_session(list(session = 4242L), pid = 4242L))
  expect_true(ctgui_autosave_from_other_session(list(session = 4242L), pid = 1L))
  expect_false(ctgui_autosave_from_other_session(NULL))
})

test_that("the offer says what would come back", {
  state <- list(
    saved_at = as.POSIXct("2026-08-27 09:30:00", tz = "UTC"),
    spec = autosave_spec(),
    data = data.frame(a = 1),
    data_name = "ctsem::ctstantestdat"
  )
  described <- ctgui_autosave_describe(state)
  expect_match(described, "2 latent processes", fixed = TRUE)
  expect_match(described, "2 manifest variables", fixed = TRUE)
  expect_match(described, "ctsem::ctstantestdat", fixed = TRUE)

  # A user has to know they will need to reload the data before choosing.
  omitted <- ctgui_autosave_describe(list(
    saved_at = Sys.time(), spec = autosave_spec(),
    data = NULL, data_omitted = TRUE, data_name = "big frame"
  ))
  expect_match(omitted, "too large to keep", fixed = TRUE)

  none <- ctgui_autosave_describe(list(saved_at = Sys.time(), spec = autosave_spec()))
  expect_match(none, "No data was loaded", fixed = TRUE)
})

test_that("clearing removes the file", {
  path <- temp_autosave()
  ctgui_autosave_write(autosave_spec(), path = path)
  expect_true(file.exists(path))
  ctgui_autosave_clear(path)
  expect_false(file.exists(path))
})

test_that("an unwritable location fails quietly rather than breaking the session", {
  # Autosave is a safety net; it must never be the thing that stops a session
  # working.
  expect_false(ctgui_autosave_write(NULL))

  # A path whose parent is an existing file cannot be created as a directory.
  blocker <- tempfile()
  on.exit(unlink(blocker), add = TRUE)
  writeLines("in the way", blocker)
  expect_false(ctgui_autosave_write(
    autosave_spec(),
    path = file.path(blocker, "autosave.rds")
  ))
})

test_that("fits are deliberately not saved", {
  source <- paste(
    readLines(ctgui_test_source_path("R", "autosave.R"), warn = FALSE),
    collapse = "\n"
  )
  # They are large, slow to serialise, and recoverable by refitting.
  expect_false(grepl("current_fit", source, fixed = TRUE))
  expect_match(source, "Fits are deliberately not saved", fixed = TRUE)
})

test_that("an empty starting session does not overwrite a model left to recover", {
  # The autosave observer fires as a session initialises. Writing the empty
  # starting model at that moment destroyed the very thing the user was about
  # to be offered.
  source <- paste(
    readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(source, "ctgui_autosave_worth_offering(list(spec = spec))", fixed = TRUE)
  # And the state to recover is read before that observer can run at all.
  expect_match(source, "recoverable_state <- ctgui_autosave_read()", fixed = TRUE)
})

test_that("the tests do not use the cache directory a real session would", {
  # Autosave lives at one per-user path. Without the redirection in
  # setup-autosave-cache.R a test run clears a user's genuine recovered model,
  # and two concurrent runs restore each other's specifications -- which shows
  # up as a restored spec carrying the other run's model, not as anything that
  # looks like an isolation problem.
  expect_equal(
    normalizePath(ctgui_autosave_dir(), winslash = "/", mustWork = FALSE),
    normalizePath(
      file.path(tempdir(), "ctsemGUI-test-cache", "R", "ctsemGUI"),
      winslash = "/", mustWork = FALSE
    )
  )
  expect_equal(
    ctgui_autosave_path(),
    file.path(ctgui_autosave_dir(), "autosave.rds")
  )
})

test_that("a model survives a session ending and comes back in the next", {
  skip_if_not_installed("shiny")
  ctgui_blueprint_apply <- getFromNamespace("ctgui_blueprint_apply", "ctsemGUI")

  # setup-autosave-cache.R has pointed the cache root at this session's own
  # temporary directory, so the path the app server reaches for by default is
  # this run's alone. Any autosave found there is still put back afterwards:
  # the test above reports a missing redirection but cannot stop this one
  # running, and without it this clear lands on a real user's only copy of a
  # model they spent an hour on.
  path <- ctgui_autosave_path()
  original <- if (file.exists(path)) readRDS(path) else NULL
  on.exit({
    if (is.null(original)) ctgui_autosave_clear(path) else saveRDS(original, path)
  }, add = TRUE)
  ctgui_autosave_clear(path)

  empty <- quiet_autosave(ctgui_spec(latent_names = character(), manifest_names = character()))

  suppressWarnings(shiny::testServer(ctgui_app_server(empty, ctgui_help_catalog()), {
    built <- quiet_autosave(ctgui_blueprint_apply(
      empty, ctgui_blueprint("coupled", c("stress", "sleep")), "replace"
    ))
    edited <- quiet_autosave(ctgui_set_matrix_value(built, "DRIFT", "stress", "sleep", label = "by_hand"))
    commit_current_spec(edited, reason = "matrix-cell")
    session$flushReact()
  }))

  state <- ctgui_autosave_read(path)
  expect_false(is.null(state))
  expect_true(any(grepl("by_hand", state$spec$matrices$DRIFT)))

  # A crash leaves an autosave from a different R process; that is the case
  # worth offering back.
  state$session <- state$session + 1L
  saveRDS(state, path)

  suppressWarnings(shiny::testServer(ctgui_app_server(empty, ctgui_help_catalog()), {
    expect_length(current_spec()$latent_names, 0L)
    session$setInputs(autosave_restore = 1)
    expect_equal(current_spec()$latent_names, c("stress", "sleep"))
    expect_true(any(grepl("by_hand", current_spec()$matrices$DRIFT)))
    expect_gt(length(spec_history()$entries), 1L)
  }))
})
