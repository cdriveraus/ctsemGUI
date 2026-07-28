expect_parseable_code <- function(lines) {
  code <- paste(lines, collapse = "\n")
  expect_error(parse(text = code), NA)
  invisible(code)
}

test_that("data-source code covers every GUI source without private API calls", {
  generated <- ctgui_output_data_code(ctgui_output_data_source(
    "generated",
    generation = list(n.subjects = 4L, Tpoints = 7L, burnin = 2L,
      dtmean = .5, logdtsd = .1, wide = FALSE)
  ))
  expect_parseable_code(generated)
  expect_match(paste(generated, collapse = "\n"), "ctsem::ctGenerate", fixed = TRUE)
  expect_false(any(grepl("ctsemGUI::ctgui_generate_data", generated, fixed = TRUE)))
  expect_true(any(grepl("intentionally not a public workflow API", generated, fixed = TRUE)))

  r_object <- ctgui_output_data_code(ctgui_output_data_source("r_object", "panel_data"))
  expect_identical(r_object, "data <- panel_data")
  expect_error(ctgui_output_data_code(ctgui_output_data_source("r_object", "not valid")))

  csv <- ctgui_output_data_code(ctgui_output_data_source("csv", "observations.csv"))
  expect_parseable_code(csv)
  expect_match(paste(csv, collapse = "\n"), "utils::read.csv", fixed = TRUE)
  rds <- ctgui_output_data_code(ctgui_output_data_source("rds", "observations.rds"))
  expect_parseable_code(rds)
  expect_match(paste(rds, collapse = "\n"), "readRDS", fixed = TRUE)
  expect_match(ctgui_output_data_code(ctgui_output_data_source("session")), "Shiny session")
  expect_match(ctgui_output_data_code(ctgui_output_data_source("none")), "No data")
})

test_that("fit and uncertainty code reflects controls and remains valid R", {
  control <- list(ridge = 1e-8, surrogateNpoints = NULL, imisMaxIter = 40L)
  fit <- ctgui_output_fit_code(list(
    optimize = TRUE, priors = FALSE, cores = 3L,
    uncertainty = "is", uncertainty_draws = "imis", finishsamples = 250L,
    uncertainty_control = control,
    extra_args = list(verbose = 0L)
  ))
  fit_code <- expect_parseable_code(fit)
  expect_match(fit_code, "model = model", fixed = TRUE)
  expect_match(fit_code, "uncertainty = \"is\"", fixed = TRUE)
  expect_match(fit_code, "verbose = 0L", fixed = TRUE)
  expect_false(grepl("surrogateNpoints", fit_code, fixed = TRUE))

  no_optimize <- expect_parseable_code(ctgui_output_fit_code(list(optimize = FALSE)))
  expect_false(grepl("optimcontrol", no_optimize, fixed = TRUE))

  uncertainty <- expect_parseable_code(ctgui_output_uncertainty_code(list(
    uncertainty = "sandwich", draws = "normal", finishsamples = 125L,
    cores = 2L, uncertainty_control = control
  )))
  expect_match(uncertainty, "ctsem::ctOptimUncertainty", fixed = TRUE)
  expect_match(uncertainty, "control = list", fixed = TRUE)
  expect_true("control" %in% names(formals(ctsem::ctOptimUncertainty)))
})

test_that("every diagnostic emits parseable current ctsem code", {
  options <- list(
    generate_from_fit = list(nsamples = 2L, fullposterior = TRUE, cores = 2L),
    cov_check = list(lags = "c(0, .5, 1)", cor = FALSE, cores = 2L),
    kalman = list(subjects = "1:2", timerange = "c(0, 5)",
      timestep = .1, removeObs = FALSE, kalmanvec = c("y", "ysmooth"),
      errorvec = "auto"),
    postpred = list(),
    residual_acf = list(varnames = c("x", "y"), nboot = 25L),
    dynamics = list(subjects = 1:2, times = c(0, 1), nsamples = 10L,
      observational = TRUE, cores = 2L, ylim = "c(-1, 1)"),
    tipred = list(tipreds = "all", subject = 1L, timestep = "auto",
      TIPvalues = "c(-1, 0, 1)")
  )
  expected_calls <- c(
    generate_from_fit = "ctsem::ctGenerateFromFit",
    cov_check = "ctsem::ctFitCovCheck",
    kalman = "ctsem::ctPredict",
    postpred = "ctsem::ctPostPredPlots",
    residual_acf = "ctsem::ctACFresiduals",
    dynamics = "ctsem::ctDiscretePars",
    tipred = "ctsem::ctPredictTIP"
  )
  for (diagnostic in names(options)) {
    code <- expect_parseable_code(ctgui_output_diagnostic_code(
      diagnostic, options[[diagnostic]]))
    expect_match(code, expected_calls[[diagnostic]], fixed = TRUE,
      info = diagnostic)
  }
})

test_that("blank covariance lags preserve ctsem defaults in exported code", {
  code <- expect_parseable_code(ctgui_output_diagnostic_code(
    "cov_check", list(lags = NULL, cor = TRUE, cores = 1L)
  ))
  expect_false(grepl("cov_lags", code, fixed = TRUE))
  expect_false(grepl("maxlag", code, fixed = TRUE))
})

test_that("complete workflow code combines model, source, and recorded actions", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  code <- ctgui_output_workflow_code(
    spec,
    ctgui_output_data_source("r_object", "panel_data"),
    snippets = list(
      fit = ctgui_output_snippet("fit", list(optimize = FALSE)),
      summary = ctgui_output_snippet("summary")
    )
  )
  expect_error(parse(text = code), NA)
  expect_match(code, "ctsem::ctModel", fixed = TRUE)
  expect_match(code, "data <- panel_data", fixed = TRUE)
  expect_match(code, "ctsem::ctFit", fixed = TRUE)
})

test_that("fit accessors hide supported ctsem object-shape variants", {
  nested <- list(
    model = list(pars = data.frame()),
    generated = data.frame(y = 1),
    stanfit = list(
      rawest = 1:3,
      rawposterior = matrix(1, 4, 3),
      transformedparsfull = list(ll = -10, llrow = matrix(0, 1, 5)),
      optimfit = list(value = -12),
      uncertainty = list(method = "hessian"),
      stanfit = list(sim = list())
    ),
    stanmodel = "compiled",
    standata = list()
  )
  flat <- list(
    ctstanmodel = list(pars = data.frame()),
    stanfit = list(sim = list(draw = 1)),
    transformedparsfull = list(ll = -20, llrow = matrix(0, 2, 7)),
    rawest = 1:2,
    optimfit = list(value = -22),
    generateddata = data.frame(y = 2)
  )

  expect_true(ctgui_ctsem_is_fit(nested))
  expect_identical(ctgui_ctsem_fit_model(nested), nested$model)
  expect_identical(ctgui_ctsem_fit_generated(nested), nested$generated)
  expect_equal(ctgui_ctsem_fit_statistics(nested),
    list(loglik = -10, logposterior = -12, npars = 3L, nobs = 5L))
  expect_false(ctgui_ctsem_fit_is_sampled(nested))
  expect_length(ctgui_ctsem_fit_missing_components(nested), 0L)

  expect_identical(ctgui_ctsem_fit_model(flat), flat$ctstanmodel)
  expect_identical(ctgui_ctsem_fit_generated(flat), flat$generateddata)
  expect_equal(ctgui_ctsem_fit_statistics(flat),
    list(loglik = -20, logposterior = -22, npars = 2L, nobs = 7L))
  expect_true(ctgui_ctsem_fit_is_sampled(flat))
  expect_false(ctgui_ctsem_is_fit(list(unrelated = TRUE)))
  base_model <- list(latentNames = "eta", manifestNames = "y")
  expect_identical(ctgui_ctsem_fit_model(list(modelbase = base_model)), base_model)
})

test_that("version-sensitive fit paths occur only inside the adapter", {
  service_files <- c("fit_diagnostics_services.R", "fit_uncertainty.R",
    "output_services.R")
  source_dir <- dirname(ctgui_test_source_path("R", service_files[[1L]]))
  source_text <- unlist(lapply(file.path(source_dir, service_files), readLines,
    warn = FALSE), use.names = FALSE)
  expect_false(any(grepl("\\$(stanfit|model|generated)\\b", source_text,
    perl = TRUE)))
})
