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
  expect_true(all(measurement$directed))
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

test_that("data validation accepts named matrices", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y", id = "id", time = "time")
  data <- cbind(id = c(1, 1, 2, 2), time = c(0, 1, 0, 1), y = c(2, 3, 4, 6))
  validation <- ctgui_validate_data(spec, data)

  expect_false(any(validation$severity == "error"))
})

test_that("workflow code uses modern ctFit model argument", {
  spec <- ctgui_build_model("dynamic_var", "single_indicator", options = list(n = 1))
  code <- ctgui_export_code(spec)

  expect_match(code, "ctsem::ctModel", fixed = TRUE)
})

test_that("guided and complete builders share every structure and measurement template", {
  factor_names <- c("alpha", "beta")
  structures <- ctgui_structures()$id
  measurements <- ctgui_measurements()$id

  for (structure in structures) for (measurement in measurements) {
    options <- list(
      n = 2L,
      indicators_per_factor = if (identical(measurement, "single_indicator")) 1L else 2L,
      free_noise_correlations = TRUE,
      trend_type = "exponential",
      trend_coupling = "free"
    )
    if (identical(measurement, "fixed_loadings")) {
      options$fixed_loadings <- list(c(1, .8), c(1, .7))
    }
    template <- ctgui_build_model(
      structure = structure, measurement = measurement,
      names = list(factor_names = factor_names), options = options
    )
    guided <- ctgui_spec(
      latent_names = template$latent_names,
      manifest_names = template$manifest_names,
      tipredDefault = FALSE
    )
    structure_options <- switch(structure,
      dynamic_var = list(dynamic_latents = template$builder$dynamic_latents),
      linear_growth = list(
        level_latents = template$builder$level_latents,
        slope_latents = template$builder$slope_latents
      ),
      dynamic_var_trend = list(
        dynamic_latents = template$builder$dynamic_latents,
        trend_latents = template$builder$trend_latents,
        trend_type = "exponential", trend_coupling = "free"
      )
    )
    structure_options$free_noise_correlations <- TRUE
    guided <- ctgui_build_matrices(guided, structure, structure_options)

    factor_latents <- switch(structure,
      linear_growth = template$builder$level_latents,
      template$builder$dynamic_latents
    )
    measurement_options <- list(
      factor_latents = factor_latents,
      manifest_blocks = template$builder$manifest_blocks
    )
    if (identical(structure, "dynamic_var_trend")) {
      measurement_options$trend_latents <- template$builder$trend_latents
    }
    if (identical(measurement, "fixed_loadings")) {
      measurement_options$fixed_loadings <- options$fixed_loadings
    }
    guided <- ctgui_build_measurement_matrices(guided, measurement, measurement_options)

    expect_equal(guided$matrices, template$matrices,
      info = paste(structure, measurement))
  }
})

test_that("template labels and fixed defaults remain stable", {
  dynamic <- ctgui_build_model(
    "dynamic_var", "marker", names = list(factor_names = c("a", "b")),
    options = list(n = 2L, indicators_per_factor = 2L, free_noise_correlations = TRUE)
  )
  expect_equal(dynamic$matrices$DRIFT, matrix(c(
    "auto_a", "cross_a_to_b", "cross_b_to_a", "auto_b"
  ), 2L, 2L, dimnames = list(c("a", "b"), c("a", "b"))))
  expect_equal(dynamic$matrices$DIFFUSION, matrix(c(
    "system_noise_a", "noise_cor_b_a", 0, "system_noise_b"
  ), 2L, 2L, dimnames = list(c("a", "b"), c("a", "b"))))
  expect_equal(unname(dynamic$matrices$LAMBDA[c("a_y1", "a_y2"), "a"]),
    c("1", "lambda_a_y2_a"))
  expect_equal(dynamic$matrices$T0VAR["b", "a"], "t0var_b_a")
  expect_equal(dynamic$matrices$MANIFESTMEANS, matrix(0, 4L, 1L,
    dimnames = list(c("a_y1", "a_y2", "b_y1", "b_y2"), "MANIFESTMEANS")))

  growth <- ctgui_build_model("linear_growth", "single_indicator",
    names = list(factor_names = c("a", "b")), options = list(n = 2L))
  expect_equal(growth$matrices$DRIFT["a_level", "a_slope"], 1)
  expect_true(all(growth$matrices$DIFFUSION == 0))

  trend <- ctgui_build_model("dynamic_var_trend", "single_indicator",
    names = list(factor_names = c("a", "b")),
    options = list(n = 2L, trend_type = "exponential", trend_coupling = "free"))
  expect_equal(trend$matrices$DRIFT["a", "a_trend"], "trend_to_a")
  expect_equal(trend$matrices$DRIFT["a_trend", "a_trend"], "trend_decay_a")
  expect_equal(unname(trend$matrices$LAMBDA["a", c("a", "a_trend")]), c(1, 1))
})

test_that("builder catalogs validate IDs and parameter annotations drive cell activity", {
  expect_equal(ctgui_structures()$id, c("linear_growth", "dynamic_var", "dynamic_var_trend"))
  expect_equal(ctgui_measurements()$id, c("single_indicator", "marker", "fixed_loadings"))
  expect_error(ctgui_build_model("not-a-structure"), "structure must be one of")
  expect_error(ctgui_build_model(measurement = "not-a-measurement"), "measurement must be one of")

  expect_false(getFromNamespace("ctgui_cell_active", "ctsemgui")("0"))
  expect_true(getFromNamespace("ctgui_cell_active", "ctsemgui")("-0.2"))
  expect_true(getFromNamespace("ctgui_cell_active", "ctsemgui")("drift_a||TRUE|1|group"))
  expect_false(getFromNamespace("ctgui_cell_active", "ctsemgui")("   "))
})
