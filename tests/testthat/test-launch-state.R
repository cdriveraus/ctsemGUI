scripted_model <- function(...) {
  args <- list(
    type = "ct", n.latent = 2, n.manifest = 2,
    latentNames = c("eta1", "eta2"), manifestNames = c("Y1", "Y2"),
    LAMBDA = diag(2)
  )
  overrides <- list(...)
  args[names(overrides)] <- overrides
  suppressWarnings(suppressMessages(do.call(ctsem::ctModel, args)))
}

test_that("an empty launch keeps the previous starting state", {
  state <- ctgui_launch_state()

  expect_s3_class(state$spec, "ctsemgui_spec")
  expect_equal(state$spec$latent_names, character())
  expect_null(state$data)
  expect_null(state$fit)
  expect_equal(state$data_name, "No data selected")
  expect_equal(state$tab, "Data")
  expect_null(state$status)
})

test_that("a scripted model opens as an editable specification", {
  skip_if_not_installed("ctsem")
  model <- scripted_model(
    DRIFT = matrix(c("drift11", 0, "drift21", "drift22"), 2, 2, byrow = TRUE),
    id = "participant", time = "occasion"
  )

  state <- suppressWarnings(suppressMessages(ctgui_launch_state(model)))

  expect_s3_class(state$spec, "ctsemgui_spec")
  expect_equal(state$spec$latent_names, c("eta1", "eta2"))
  expect_equal(state$spec$manifest_names, c("Y1", "Y2"))
  expect_equal(state$spec$id, "participant")
  expect_equal(state$spec$time, "occasion")
  expect_equal(as.character(state$spec$matrices$DRIFT["eta2", "eta1"]), "drift21")
  expect_equal(as.character(state$spec$matrices$DRIFT["eta1", "eta2"]), "0")
  # An opened model is what the session is about, so it is shown first.
  expect_equal(state$tab, "Model")
  expect_match(state$status, "continuous-time model", fixed = TRUE)
})

test_that("importing a model preserves its TI predictor moderation exactly", {
  skip_if_not_installed("ctsem")
  model <- scripted_model(
    n.TIpred = 2, TIpredNames = c("age", "grp"), tipredDefault = FALSE,
    DRIFT = matrix(c("drift11||FALSE||age", 0, "drift21", "drift22"), 2, 2, byrow = TRUE)
  )

  spec <- suppressWarnings(suppressMessages(ctgui_launch_spec(model)))
  expect_false(spec$tipredDefault)

  rebuilt <- suppressWarnings(suppressMessages(ctgui_to_ctsem_model(spec)))
  original <- model$pars[!is.na(model$pars$param), , drop = FALSE]
  restored <- rebuilt$pars[!is.na(rebuilt$pars$param), , drop = FALSE]
  expect_equal(as.character(restored$param), as.character(original$param))
  for (field in c("age_effect", "grp_effect", "indvarying")) {
    expect_equal(
      vapply(restored[[field]], isTRUE, logical(1L)),
      vapply(original[[field]], isTRUE, logical(1L)),
      info = field
    )
  }
})

test_that("a model moderated by every predictor keeps the ctsem default", {
  skip_if_not_installed("ctsem")
  model <- scripted_model(n.TIpred = 1, TIpredNames = "age", tipredDefault = TRUE)

  spec <- suppressWarnings(suppressMessages(ctgui_launch_spec(model)))
  expect_true(spec$tipredDefault)
})

test_that("tipredDefault inference reads the per-parameter effect flags", {
  pars <- data.frame(
    param = c("a", "b"), age_effect = c(TRUE, TRUE), grp_effect = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  expect_true(ctgui_tipred_default_from_pars(pars, c("age", "grp")))

  pars$grp_effect <- c(TRUE, FALSE)
  expect_false(ctgui_tipred_default_from_pars(pars, c("age", "grp")))

  # Fixed cells carry no effects and must not drag the inference to FALSE.
  fixed <- rbind(pars, data.frame(
    param = NA_character_, age_effect = FALSE, grp_effect = FALSE,
    stringsAsFactors = FALSE
  ))
  fixed$grp_effect[seq_len(2L)] <- TRUE
  expect_true(ctgui_tipred_default_from_pars(fixed, c("age", "grp")))

  # Without TI predictors the flag is unused; keep the ctModel() default.
  expect_true(ctgui_tipred_default_from_pars(pars, character()))
})

test_that("a specification saved by the GUI reopens unchanged", {
  skip_if_not_installed("ctsem")
  spec <- suppressWarnings(suppressMessages(
    ctgui_spec(latent_names = "eta", manifest_names = "y")
  ))

  reopened <- suppressWarnings(suppressMessages(ctgui_launch_spec(spec)))
  expect_equal(reopened$latent_names, spec$latent_names)
  expect_equal(reopened$matrices$DRIFT, spec$matrices$DRIFT)
})

test_that("a model is read from an rds path", {
  skip_if_not_installed("ctsem")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(scripted_model(), path)

  spec <- suppressWarnings(suppressMessages(ctgui_launch_spec(path)))
  expect_equal(spec$manifest_names, c("Y1", "Y2"))
  expect_error(ctgui_launch_spec(file.path(tempdir(), "absent.rds")), "File not found")
})

test_that("data is accepted as an object or a file", {
  frame <- data.frame(id = 1:2, time = c(0, 1), Y1 = c(0.1, 0.2))
  expect_equal(ctgui_launch_data(frame), frame)

  csv <- tempfile(fileext = ".csv")
  on.exit(unlink(csv), add = TRUE)
  utils::write.csv(frame, csv, row.names = FALSE)
  expect_equal(ctgui_launch_data(csv)$Y1, frame$Y1)

  expect_error(ctgui_launch_data(list(a = 1)), "long-format data.frame")
  expect_error(ctgui_launch_data(tempfile(fileext = ".txt")), "File not found")
})

test_that("supplied data is named by the R object it came from", {
  frame <- data.frame(id = 1L, time = 0)
  expect_equal(ctgui_launch_data_name(frame, "mydata"), "R data: mydata")
  expect_equal(ctgui_launch_data_name(frame, NULL), "Supplied at launch")
  expect_equal(ctgui_launch_data_name(NULL), "No data selected")
})

test_that("columns the specification needs but the data lacks are reported", {
  skip_if_not_installed("ctsem")
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = "eta", manifest_names = c("Y1", "Y2"), id = "id", time = "time"
  )))
  data <- data.frame(id = 1L, time = 0, Y1 = 0.5)

  expect_equal(ctgui_launch_data_gaps(spec, data), "Y2")
  expect_equal(ctgui_launch_data_gaps(spec, NULL), character())
  expect_match(
    ctgui_launch_status(spec, NULL, data, "Y2"),
    "not in the supplied data: Y2", fixed = TRUE
  )
})

test_that("a non-fit is rejected before the app is built", {
  expect_error(ctgui_launch_fit(list(a = 1)), "fitted ctsem model")
  expect_null(ctgui_launch_fit(NULL))
})

test_that("a fit supplies both the opened model and the active fit", {
  skip_if_not_installed("ctsem")
  model <- scripted_model()
  fit <- structure(list(stanfit = list(), ctstanmodelbase = model), class = "ctStanFit")

  state <- suppressWarnings(suppressMessages(ctgui_launch_state(fit = fit)))
  expect_identical(state$fit, fit)
  expect_equal(state$spec$latent_names, c("eta1", "eta2"))
  expect_match(state$status, "fitted model was supplied", fixed = TRUE)
  expect_equal(state$fit_status, "Using the fit supplied at launch.")
})

test_that("the app opens on the supplied model, data, and fit", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ctsem")
  model <- scripted_model()
  data <- data.frame(id = c(1L, 1L), time = c(0, 1), Y1 = c(0.1, 0.2), Y2 = c(0.3, 0.4))
  state <- suppressWarnings(suppressMessages(ctgui_launch_state(model, data = data)))

  expect_s3_class(
    ctgui_app_ui(state$spec, ctgui_help_catalog(),
      list(visual_asset_url = function(file) file), initial_tab = state$tab),
    "shiny.tag.list"
  )
  suppressWarnings(shiny::testServer(
    ctgui_app_server(state$spec, ctgui_help_catalog(), initial_state = state), {
      expect_match(output$data_status, "2 rows x 4 columns", fixed = TRUE)
      expect_match(output$code_output, 'manifestNames = c("Y1", "Y2")', fixed = TRUE)
    }
  ))
})
