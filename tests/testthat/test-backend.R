ctgui_ctsem_fit_engine <- getFromNamespace("ctgui_ctsem_fit_engine", "ctsemGUI")
ctgui_ctsem_fit_backend_name <- getFromNamespace("ctgui_ctsem_fit_backend_name", "ctsemGUI")
ctgui_ctsem_fit_required_components <- getFromNamespace("ctgui_ctsem_fit_required_components", "ctsemGUI")
ctgui_ctsem_fit_missing_components <- getFromNamespace("ctgui_ctsem_fit_missing_components", "ctsemGUI")
ctgui_ctsem_fit_statistics <- getFromNamespace("ctgui_ctsem_fit_statistics", "ctsemGUI")
ctgui_optim_uncertainty_eligibility <- getFromNamespace("ctgui_optim_uncertainty_eligibility", "ctsemGUI")
ctgui_backend_choices <- getFromNamespace("ctgui_backend_choices", "ctsemGUI")
ctgui_default_backend <- getFromNamespace("ctgui_default_backend", "ctsemGUI")
ctgui_julia_pending <- getFromNamespace("ctgui_julia_pending", "ctsemGUI")
ctgui_julia_is_pending <- getFromNamespace("ctgui_julia_is_pending", "ctsemGUI")
ctgui_julia_status_from_check <- getFromNamespace("ctgui_julia_status_from_check", "ctsemGUI")

# A Julia fit as ctsem 3.12 returns it: no stanfit, stanmodel or standata, the
# engine recorded on the object, and the fitted data carried alongside.
julia_fit_stub <- function(rows = 720L) {
  structure(
    list(
      backend = "julia",
      engine = "f5f4738b78f7",
      data = data.frame(id = rep(1L, rows), time = seq_len(rows)),
      estimate = list(),
      transformedpars = list()
    ),
    class = c("ctJuliaFit", "ctFit")
  )
}

stan_fit_stub <- function() {
  structure(
    list(stanfit = list(), stanmodel = list(), standata = list()),
    class = c("ctStanFit", "ctFit")
  )
}

test_that("the engine that produced a fit is recognised", {
  expect_equal(ctgui_ctsem_fit_engine(julia_fit_stub()), "julia")
  expect_equal(ctgui_ctsem_fit_engine(stan_fit_stub()), "stan")
  expect_true(is.na(ctgui_ctsem_fit_engine(NULL)))
  expect_true(is.na(ctgui_ctsem_fit_engine(list(a = 1))))
})

test_that("a Julia fit is not judged against Stan's components", {
  # stanfit, stanmodel and standata exist only on a Stan fit. Requiring them
  # reported a working Julia fit as broken.
  expect_equal(ctgui_ctsem_fit_required_components(julia_fit_stub()), character())
  expect_equal(ctgui_ctsem_fit_missing_components(julia_fit_stub()), character())

  expect_equal(ctgui_ctsem_fit_missing_components(stan_fit_stub()), character())
  incomplete <- stan_fit_stub()
  incomplete$standata <- NULL
  expect_equal(ctgui_ctsem_fit_missing_components(incomplete), "standata")
})

test_that("uncertainty recomputation is offered for a Julia fit", {
  # ctOptimUncertainty works on a Julia fit. Requiring a ctStanFit refused a
  # feature that was available all along.
  eligible <- ctgui_optim_uncertainty_eligibility(julia_fit_stub())
  expect_true(eligible$ok)

  expect_false(ctgui_optim_uncertainty_eligibility(NULL)$ok)
  unknown <- ctgui_optim_uncertainty_eligibility(list(a = 1))
  expect_false(unknown$ok)
  expect_match(unknown$message, "produced by ctFit", fixed = TRUE)
})

test_that("observation count is recovered for a Julia fit so BIC is comparable", {
  # A Julia fit keeps no llrow, but carries the data it was fitted to. Its row
  # count matches what the Stan path reports for the same fit.
  stats <- ctgui_ctsem_fit_statistics(julia_fit_stub(rows = 720L))
  expect_equal(stats$nobs, 720L)

  no_data <- julia_fit_stub()
  no_data$data <- NULL
  expect_true(is.na(ctgui_ctsem_fit_statistics(no_data)$nobs))
})

test_that("the backend is named for display", {
  expect_equal(ctgui_ctsem_fit_backend_name(julia_fit_stub()), "julia")
  expect_equal(ctgui_ctsem_fit_backend_name(stan_fit_stub()), "stan")
  expect_equal(ctgui_ctsem_fit_backend_name(NULL), "unknown")
})

test_that("Julia is offered and preferred only when it is actually available", {
  available <- list(available = TRUE, julia = "1.12.5")
  expect_equal(unname(ctgui_backend_choices(available)), c("julia", "stan"))
  expect_equal(ctgui_default_backend(available), "julia")

  absent <- list(available = FALSE, message = "not set up")
  expect_equal(unname(ctgui_backend_choices(absent)), "stan")
  expect_equal(ctgui_default_backend(absent), "stan")
})

test_that("the Julia check is only made once", {
  # Asking starts Julia and costs seconds; a reactive read would pay it over
  # and over.
  cache <- getFromNamespace("ctgui_julia_cache", "ctsemGUI")
  old <- cache$status
  on.exit({
    if (is.null(old)) rm("status", envir = cache) else cache$status <- old
  }, add = TRUE)

  cache$status <- list(available = TRUE, message = "cached answer")
  expect_equal(getFromNamespace("ctgui_julia_status", "ctsemGUI")()$message, "cached answer")
  expect_equal(getFromNamespace("ctgui_julia_known", "ctsemGUI")()$message, "cached answer")
})

test_that("an unanswered check is not the same as a negative answer", {
  # Until the background check reports, the panel has to say it is still
  # looking rather than that Julia is unavailable.
  pending <- ctgui_julia_pending()
  expect_true(ctgui_julia_is_pending(pending))
  expect_true(is.na(pending$available))

  # Nothing is offered or preferred on the strength of an answer not yet given.
  expect_equal(unname(ctgui_backend_choices(pending)), "stan")
  expect_equal(ctgui_default_backend(pending), "stan")

  expect_false(ctgui_julia_is_pending(list(available = TRUE)))
  expect_false(ctgui_julia_is_pending(list(available = FALSE)))
})

test_that("whatever the check reports becomes a definite answer", {
  positive <- ctgui_julia_status_from_check(list(available = TRUE, julia = "1.12.5", threads = 1L))
  expect_true(positive$available)
  expect_match(positive$message, "1.12.5", fixed = TRUE)

  # A child that crashed, returned nothing, or reported unavailable all mean
  # Julia is not usable here, which is an answer rather than a reason to wait.
  for (reported in list(NULL, list(available = FALSE), "nonsense")) {
    negative <- ctgui_julia_status_from_check(reported)
    expect_false(negative$available)
    expect_false(ctgui_julia_is_pending(negative))
  }
})

test_that("the check worker needs only ctsem", {
  # It runs in a child started with package = FALSE.
  worker <- getFromNamespace("ctgui_julia_check_worker", "ctsemGUI")
  body_text <- paste(deparse(body(worker)), collapse = " ")
  expect_match(body_text, "ctsem", fixed = TRUE)
  expect_match(body_text, "ctJuliaStatus", fixed = TRUE)
  expect_false(grepl("ctgui_", body_text, fixed = TRUE))
})

test_that("the check runs in the background and never blocks the panel", {
  server_lines <- readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE)
  ui_lines <- readLines(ctgui_test_source_path("R", "app_ui.R"), warn = FALSE)
  server <- paste(server_lines, collapse = " ")

  # Started as the session loads, not when the Fit panel is opened.
  expect_match(server, "ctgui_julia_check_worker", fixed = TRUE)
  expect_match(server, "updateSelectInput(", fixed = TRUE)

  # The selector is static UI, so a late answer updates it rather than
  # re-rendering it and discarding a choice the user already made.
  expect_true(any(grepl('"fit_backend"', ui_lines, fixed = TRUE)))
  expect_true(any(grepl("shiny::selectInput(", ui_lines, fixed = TRUE)))
  expect_false(grepl("output$fit_backend_controls", server, fixed = TRUE))
})
