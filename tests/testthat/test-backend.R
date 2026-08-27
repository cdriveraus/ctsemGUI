ctgui_ctsem_fit_engine <- getFromNamespace("ctgui_ctsem_fit_engine", "ctsemGUI")
ctgui_ctsem_fit_backend_name <- getFromNamespace("ctgui_ctsem_fit_backend_name", "ctsemGUI")
ctgui_ctsem_fit_required_components <- getFromNamespace("ctgui_ctsem_fit_required_components", "ctsemGUI")
ctgui_ctsem_fit_missing_components <- getFromNamespace("ctgui_ctsem_fit_missing_components", "ctsemGUI")
ctgui_ctsem_fit_statistics <- getFromNamespace("ctgui_ctsem_fit_statistics", "ctsemGUI")
ctgui_optim_uncertainty_eligibility <- getFromNamespace("ctgui_optim_uncertainty_eligibility", "ctsemGUI")
ctgui_backend_choices <- getFromNamespace("ctgui_backend_choices", "ctsemGUI")
ctgui_default_backend <- getFromNamespace("ctgui_default_backend", "ctsemGUI")

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
  # Asking starts Julia and costs a few seconds; a reactive read would pay it
  # over and over.
  cache <- getFromNamespace("ctgui_julia_cache", "ctsemGUI")
  old <- cache$status
  on.exit({
    if (is.null(old)) rm("status", envir = cache) else cache$status <- old
  }, add = TRUE)

  cache$status <- list(available = TRUE, message = "cached answer")
  expect_equal(getFromNamespace("ctgui_julia_status", "ctsemGUI")()$message, "cached answer")
})
