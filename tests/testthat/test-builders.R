test_that("model builders work across supported dimensions", {
  for (structure in ctgui_structures()$id) {
    for (n in c(1L, 2L, 4L)) {
      spec <- ctgui_build_model(
        structure = structure,
        measurement = "single_indicator",
        options = list(n = n)
      )
      expect_s3_class(spec, "ctsemgui_spec")
      expect_false(any(ctgui_validate(spec)$severity == "error"))
    }
  }
})

test_that("matrix builders update existing specs without changing names", {
  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2", "eta3", "eta4"),
    manifest_names = c("Y1", "Y2"),
    id = "Subject",
    time = "Time",
    tdpred_names = "event",
    tipred_names = "group"
  )
  updated <- ctgui_build_matrices(spec, "dynamic_var", options = list(dynamic_latents = c("eta1", "eta2")))

  expect_equal(updated$latent_names, spec$latent_names)
  expect_equal(updated$manifest_names, spec$manifest_names)
  expect_equal(updated$id, "Subject")
  expect_equal(updated$time, "Time")
  expect_equal(updated$tdpred_names, "event")
  expect_equal(updated$tipred_names, "group")
  expect_equal(ctgui_matrix(updated, "DRIFT")["eta1", "eta2"], "cross_eta2_to_eta1")
})

test_that("trend matrix builder requires explicit existing trend pairs", {
  spec <- ctgui_spec(latent_names = c("dyn1", "dyn2", "tr1", "tr2"), manifest_names = c("Y1", "Y2"))

  expect_error(
    ctgui_build_matrices(spec, "dynamic_var_trend", options = list(dynamic_latents = c("dyn1", "dyn2"))),
    "trend_latents"
  )

  updated <- ctgui_build_matrices(spec, "dynamic_var_trend",
    options = list(dynamic_latents = c("dyn1", "dyn2"), trend_latents = c("tr1", "tr2")))
  expect_equal(updated$latent_names, spec$latent_names)
  expect_equal(ctgui_matrix(updated, "DRIFT")["dyn1", "tr1"], "1")
})

test_that("measurement matrix builder reuses loadings for explicit trend pairs", {
  spec <- ctgui_spec(
    latent_names = c("dyn1", "tr1", "dyn2", "tr2"),
    manifest_names = c("Y1a", "Y1b", "Y2a", "Y2b")
  )
  updated <- ctgui_build_measurement_matrices(spec, "marker",
    options = list(
      factor_latents = c("dyn1", "dyn2"),
      trend_latents = c("tr1", "tr2"),
      manifest_blocks = "Y1a,Y1b;Y2a,Y2b"
    ))
  lambda <- ctgui_matrix(updated, "LAMBDA")

  expect_equal(lambda["Y1a", "dyn1"], lambda["Y1a", "tr1"])
  expect_equal(lambda["Y1b", "dyn1"], lambda["Y1b", "tr1"])
  expect_equal(lambda["Y2a", "dyn2"], lambda["Y2a", "tr2"])
  expect_equal(lambda["Y2b", "dyn2"], lambda["Y2b", "tr2"])
})

test_that("measurement options compose with each structural family", {
  for (structure in ctgui_structures()$id) {
    single <- ctgui_build_model(structure, "single_indicator", options = list(n = 2))
    marker <- ctgui_build_model(structure, "marker", options = list(n = 2, indicators_per_factor = 2))
    fixed <- ctgui_build_model(structure, "fixed_loadings",
      options = list(n = 2, indicators_per_factor = 2, fixed_loadings = list(c(1, .8), c(1, .7))))

    expect_equal(length(single$manifest_names), 2)
    expect_equal(length(marker$manifest_names), 4)
    expect_equal(length(fixed$manifest_names), 4)
    expect_false(any(ctgui_validate(marker)$severity == "error"))
    expect_false(any(ctgui_validate(fixed)$severity == "error"))
  }
})

test_that("trend builders reuse measurement loadings for paired trend processes", {
  spec <- ctgui_build_model(
    structure = "dynamic_var_trend",
    measurement = "marker",
    names = list(factor_names = c("a", "b")),
    options = list(n = 2, indicators_per_factor = 2)
  )
  lambda <- ctgui_matrix(spec, "LAMBDA")

  expect_equal(lambda["a_y1", "a"], lambda["a_y1", "a_trend"])
  expect_equal(lambda["a_y2", "a"], lambda["a_y2", "a_trend"])
  expect_equal(lambda["b_y1", "b"], lambda["b_y1", "b_trend"])
  expect_equal(lambda["b_y2", "b"], lambda["b_y2", "b_trend"])
})

test_that("graph extraction separates drift, diffusion, measurement, and trend edges", {
  spec <- ctgui_build_model(
    structure = "dynamic_var_trend",
    measurement = "single_indicator",
    names = list(factor_names = c("a", "b")),
    options = list(n = 2, free_noise_correlations = TRUE)
  )

  drift <- ctgui_graph_edges(spec, "drift")
  diffusion <- ctgui_graph_edges(spec, "diffusion")
  measurement <- ctgui_graph_edges(spec, "measurement")
  trend <- ctgui_graph_edges(spec, "trend")

  expect_true(any(drift$directed))
  expect_true(nrow(diffusion) > 0)
  expect_false(any(diffusion$directed))
  expect_true(all(measurement$element == "LAMBDA"))
  expect_true(any(grepl("_trend", trend$from) | grepl("_trend", trend$to)))
})

test_that("data validation reports missing columns and duplicate id/time rows", {
  spec <- ctgui_build_model(
    structure = "dynamic_var",
    measurement = "single_indicator",
    names = list(factor_names = "affect", id = "Subject", time = "Time"),
    options = list(n = 1)
  )
  dat <- data.frame(Subject = c(1, 1), Time = c(0, 0), stringsAsFactors = FALSE)
  validation <- ctgui_validate_data(spec, dat)

  expect_true(any(validation$severity == "error"))
  expect_true(any(validation$field == "columns"))
  expect_true(any(validation$field == "id/time"))
})

test_that("workflow code uses modern ctFit model argument", {
  spec <- ctgui_build_model("dynamic_var", "single_indicator", options = list(n = 1))
  code <- ctgui_export_code(spec)

  expect_match(code, "ctsem::ctModel", fixed = TRUE)
})
