ctgui_respec_preserving <- getFromNamespace("ctgui_respec_preserving", "ctsemGUI")

quiet_respec <- function(code) suppressWarnings(suppressMessages(force(code)))

base_spec <- function() {
  quiet_respec(ctgui_spec(
    latent_names = c("a", "b"),
    manifest_names = c("ya", "yb"),
    tipred_names = "age"
  ))
}

test_that("adding a variable keeps the model already built", {
  # Adding one variable used to rebuild the specification from defaults, which
  # silently discarded every edited cell. Growing a model is the normal way to
  # build one up, so losing the work is not an acceptable cost of it.
  spec <- quiet_respec(ctgui_set_matrix_value(base_spec(), "DRIFT", "a", "b", label = "cross_ab"))
  spec <- quiet_respec(ctgui_set_matrix_value(spec, "DRIFT", "b", "b", value = -0.5))

  grown <- quiet_respec(ctgui_respec_preserving(
    spec, latent_names = c("a", "b", "c"), manifest_names = c("ya", "yb")
  ))

  expect_equal(grown$matrices$DRIFT["a", "b"], "cross_ab")
  expect_equal(grown$matrices$DRIFT["b", "b"], "-0.5")
  expect_true(all(c("a", "b", "c") %in% rownames(grown$matrices$DRIFT)))
})

test_that("parameter annotations follow their cell", {
  spec <- quiet_respec(ctgui_set_matrix_value(base_spec(), "DRIFT", "a", "b", label = "cross_ab"))
  spec <- quiet_respec(ctgui_set_parameter_metadata(
    spec, "DRIFT", "a", "b", transform = "exp(param)", indvarying = TRUE, sdscale = 0.5
  ))

  grown <- quiet_respec(ctgui_respec_preserving(
    spec, latent_names = c("a", "b", "c"), manifest_names = c("ya", "yb")
  ))

  metadata <- grown$parameter_metadata
  row <- metadata[metadata$matrix == "DRIFT" & metadata$row == "a" & metadata$col == "b", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$transform[1L], "exp(param)")
  expect_true(isTRUE(row$indvarying[1L]))
})

test_that("a renamed variable carries its cells only when the rename is known", {
  spec <- quiet_respec(ctgui_set_matrix_value(base_spec(), "DRIFT", "a", "b", label = "cross_ab"))

  renamed <- quiet_respec(ctgui_respec_preserving(
    spec, latent_names = c("stress", "b"), manifest_names = c("ya", "yb"),
    rename = c(a = "stress")
  ))
  expect_equal(renamed$matrices$DRIFT["stress", "b"], "cross_ab")

  # Without a rename map there is no way to tell a rename from a removal plus
  # an addition, so the new name starts from defaults rather than inheriting
  # cells that may have nothing to do with it.
  replaced <- quiet_respec(ctgui_respec_preserving(
    spec, latent_names = c("stress", "b"), manifest_names = c("ya", "yb")
  ))
  expect_false(identical(replaced$matrices$DRIFT["stress", "b"], "cross_ab"))
})

test_that("removing a variable drops only what it owned", {
  spec <- quiet_respec(ctgui_set_matrix_value(base_spec(), "DRIFT", "a", "a", label = "auto_a"))

  shrunk <- quiet_respec(ctgui_respec_preserving(
    spec, latent_names = "a", manifest_names = "ya"
  ))

  expect_equal(shrunk$matrices$DRIFT["a", "a"], "auto_a")
  expect_equal(rownames(shrunk$matrices$DRIFT), "a")
  expect_false(any(shrunk$parameter_metadata$row == "b"))
})

test_that("data roles and predictors survive a variable change", {
  spec <- base_spec()
  grown <- quiet_respec(ctgui_respec_preserving(
    spec, latent_names = c("a", "b", "c"), manifest_names = c("ya", "yb")
  ))

  expect_equal(grown$tipred_names, "age")
  expect_equal(grown$id, spec$id)
  expect_equal(grown$time, spec$time)
  expect_equal(grown$type, spec$type)
})

test_that("both editing surfaces share one resize path", {
  # The visual editor already preserved edits across a resize while the
  # specification fields did not. The behaviour belongs to the specification,
  # not to whichever screen the change was made on.
  visual_source <- paste(
    readLines(ctgui_test_source_path("R", "visual_spec.R"), warn = FALSE),
    collapse = "\n"
  )
  state_source <- paste(
    readLines(ctgui_test_source_path("R", "model_state_services.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(visual_source, "ctgui_respec_preserving(", fixed = TRUE)
  expect_match(state_source, "ctgui_respec_preserving(", fixed = TRUE)
})
