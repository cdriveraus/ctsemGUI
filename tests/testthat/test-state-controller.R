test_that("matrix schema is the single source for dimensions and visual membership", {
  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2"), manifest_names = c("y1", "y2"),
    tdpred_names = "event", Tpoints = 3
  )
  schema <- getFromNamespace("ctgui_matrix_schema", "ctsemGUI")(spec)
  dims <- getFromNamespace("ctgui_matrix_schema_dims", "ctsemGUI")(spec)
  visual <- getFromNamespace("ctgui_matrix_schema_visual", "ctsemGUI")(spec)

  expect_equal(schema$DRIFT$dims, c(2L, 2L))
  expect_equal(schema$TDPREDVAR$dims, c(3L, 3L))
  expect_equal(schema$T0VAR$triangular, "lower")
  expect_equal(visual[["T0VAR"]], "initial_state")
  expect_equal(dims$TDPREDEFFECT, c(2L, 1L))
  expect_equal(dims$TDPREDMEANS, c(3L, 1L))
})

test_that("the annotation codec preserves five fields and accepts legacy labels", {
  decode <- getFromNamespace("ctgui_parameter_annotation_decode", "ctsemGUI")
  encode <- getFromNamespace("ctgui_parameter_annotation_encode", "ctsemGUI")
  value <- encode("auto", "exp(param)", TRUE, 0.75, c("age", "group"))
  parsed <- decode(value, c("age", "group"))

  expect_equal(value, "auto|exp(param)|TRUE|0.75|age,group")
  expect_equal(parsed$param, "auto")
  expect_equal(parsed$transform, "exp(param)")
  expect_true(parsed$indvarying)
  expect_equal(parsed$sdscale, 0.75)
  expect_equal(parsed$tipreds, c("age", "group"))
  expect_equal(decode("auto|||age", "age")$tipreds, "age")
})

test_that("commits return canonical state and explicit reactive effects", {
  commit_spec <- getFromNamespace("ctgui_commit_spec", "ctsemGUI")
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  spec$model <- NULL
  previous <- spec
  updated <- spec
  updated$matrices$DRIFT[1, 1] <- "auto"

  commit <- commit_spec(previous, updated, reason = "test")
  expect_s3_class(commit, "ctgui_spec_commit")
  expect_true(commit$effects$changed)
  expect_true(commit$effects$invalidate_fit)
  expect_true(commit$effects$refresh_matrices)
  expect_true(commit$effects$refresh_visual)
  expect_equal(commit$effects$reason, "test")
  expect_equal(commit$spec$matrices$DRIFT[1, 1], "auto")
  expect_true(any(commit$spec$parameter_metadata$param == "auto"))

  unchanged <- commit_spec(commit$spec, commit$spec, reason = "unchanged", sync_model = FALSE)
  expect_false(unchanged$effects$changed)
  expect_false(unchanged$effects$invalidate_fit)

  remapped <- commit$spec
  remapped$id <- "participant"
  remap_commit <- commit_spec(commit$spec, remapped, reason = "data-mapping", sync_model = FALSE)
  expect_true(remap_commit$effects$changed)
  expect_true(remap_commit$effects$invalidate_fit)
})

test_that("ctsem adapter reports the required model capabilities", {
  capabilities <- getFromNamespace("ctgui_ctsem_capabilities", "ctsemGUI")()
  expect_true(is.logical(capabilities$installed))
  if (capabilities$installed) {
    expect_true(all(capabilities$required))
    expect_true(nzchar(capabilities$version))
  }
})
