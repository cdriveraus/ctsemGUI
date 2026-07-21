test_that("ctgui_spec creates a valid default specification", {
  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("Y1", "Y2")
  )

  expect_s3_class(spec, "ctsemgui_spec")
  expect_equal(ctgui_matrix_names(spec), c(
    "LAMBDA", "T0VAR", "T0MEANS", "MANIFESTMEANS",
    "MANIFESTVAR", "DRIFT", "CINT", "DIFFUSION"
  ))

  validation <- ctgui_validate(spec)
  expect_false(any(validation$severity == "error"))
})

test_that("matrix cell edits support labels and fixed values", {
  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("Y1", "Y2")
  )

  spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta1", "eta2", label = "cross12")
  spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta2", "eta1", value = -0.2)

  drift <- ctgui_matrix(spec, "DRIFT")
  expect_equal(drift["eta1", "eta2"], "cross12")
  expect_equal(drift["eta2", "eta1"], "-0.2")
})

test_that("code export includes ctsem model construction", {
  spec <- ctgui_spec(latent_names = "eta1", manifest_names = "Y1")
  code <- ctgui_export_code(spec)

  expect_match(code, "ctsem::ctModel", fixed = TRUE)
  expect_match(code, "LAMBDA", fixed = TRUE)
  expect_match(code, "DRIFT", fixed = TRUE)
})

test_that("core ctModel arguments survive spec creation and export", {
  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("Y1", "Y2"),
    type = "dt",
    Tpoints = 5,
    manifest_type = c(0, 0),
    tdpred_names = "event",
    tipredDefault = FALSE
  )

  expect_equal(spec$type, "dt")
  expect_equal(spec$Tpoints, 5)
  expect_equal(spec$manifest_type, c(0, 0))
  expect_false(spec$tipredDefault)
  expect_false(any(ctgui_validate(spec)$severity == "error"))

  code <- ctgui_export_code(spec)
  expect_match(code, "Tpoints = 5", fixed = TRUE)
  expect_match(code, "manifesttype = c(0, 0)", fixed = TRUE)
  expect_match(code, "TDpredNames = \"event\"", fixed = TRUE)
  expect_match(code, "tipredDefault = FALSE", fixed = TRUE)
})

test_that("conversion to ctsem is available when ctsem is installed", {
  skip_if_not_installed("ctsem")

  spec <- ctgui_spec(latent_names = "eta1", manifest_names = "Y1")
  model <- ctgui_to_ctsem_model(spec)

  expect_true(inherits(model, "ctStanModel") || inherits(model, "ctsemInit"))
})

test_that("exported matrix code preserves matrix positions", {
  skip_if_not_installed("ctsem")

  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("Y1", "Y2")
  )
  spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta1", "eta2", label = "cross12")

  env <- new.env(parent = globalenv())
  eval(parse(text = ctgui_export_code(spec)), envir = env)
  drift <- ctsem::ctModelMatrices(env$model)$DRIFT

  expect_equal(drift[1, 2], "cross12")
})

test_that("whole matrix edits and generated data work", {
  skip_if_not_installed("ctsem")

  spec <- ctgui_spec(latent_names = "eta1", manifest_names = "Y1")
  drift <- ctgui_matrix(spec, "DRIFT")
  drift[1, 1] <- -0.3
  spec <- ctgui_set_matrix(spec, "DRIFT", drift)

  expect_equal(as.numeric(ctgui_matrix(spec, "DRIFT")[1, 1]), -0.3)

  data <- ctgui_generate_data(spec, n.subjects = 2, Tpoints = 4)
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 8)
  expect_true(all(c("id", "time", "Y1") %in% names(data)))
})

test_that("latex equations are produced from ctsem", {
  skip_if_not_installed("ctsem")

  spec <- ctgui_spec(latent_names = "eta1", manifest_names = "Y1")
  latex <- ctgui_latex(spec)

  expect_type(latex, "character")
  expect_match(latex, "DRIFT", fixed = TRUE)
})

test_that("annotated TI parameter settings survive matrix round trips", {
  skip_if_not_installed("ctsem")

  spec <- ctgui_spec(
    latent_names = "eta", manifest_names = "Y",
    tipred_names = c("age", "group"), tipredDefault = FALSE
  )
  spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta", "eta", label = "auto_eta||TRUE||age")
  expect_equal(ctgui_matrix(spec, "DRIFT")[1, 1], "auto_eta")
  model <- ctgui_to_ctsem_model(spec)
  drift <- model$pars[model$pars$matrix == "DRIFT", , drop = FALSE]
  expect_equal(as.character(drift$param[1]), "auto_eta")
  expect_true(isTRUE(drift$indvarying[1]))
  expect_true(isTRUE(drift$age_effect[1]))
  expect_false(isTRUE(drift$group_effect[1]))

  restored <- ctgui_spec_from_model(model)
  metadata <- restored$parameter_metadata[restored$parameter_metadata$matrix == "DRIFT", , drop = FALSE]
  expect_true(isTRUE(metadata$age_effect[1]))
})

test_that("parameter metadata retains all selected-cell settings", {
  spec <- ctgui_spec(
    latent_names = "eta", manifest_names = "Y",
    tipred_names = c("age", "group"), tipredDefault = FALSE
  )
  spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta", "eta", label = "auto_eta")
  spec <- ctgui_set_parameter_metadata(
    spec, "DRIFT", "eta", "eta",
    transform = "exp(param)", indvarying = TRUE, sdscale = 0.75,
    tipred_effects = "age"
  )
  metadata <- spec$parameter_metadata[spec$parameter_metadata$matrix == "DRIFT", , drop = FALSE]

  expect_equal(metadata$transform[1], "exp(param)")
  expect_true(isTRUE(metadata$indvarying[1]))
  expect_equal(metadata$sdscale[1], 0.75)
  expect_true(isTRUE(metadata$age_effect[1]))
  expect_false(isTRUE(metadata$group_effect[1]))
})
