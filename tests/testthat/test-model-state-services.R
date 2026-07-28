test_that("spec field commits distinguish unchanged and structural edits", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("y1", "y2")
  )))
  values <- list(
    latent_names = "eta1, eta2", manifest_names = "y1, y2",
    tdpred_names = "", tipred_names = "", type = "ct",
    tipredDefault = TRUE, id = "id", time = "time"
  )
  fields <- ctgui_spec_fields(values, spec)
  expect_false(ctgui_spec_fields_changed(spec, fields))
  unchanged <- suppressWarnings(suppressMessages(
    ctgui_commit_spec_fields(spec, fields)
  ))
  expect_false(unchanged$effects$changed)

  fields$latent_names <- c("eta1", "eta2", "eta3")
  fields$manifest_names <- c("y1", "y2", "y3")
  fields$manifest_type <- c(0L, 0L, 0L)
  added <- suppressWarnings(suppressMessages(
    ctgui_commit_spec_fields(spec, fields)
  ))
  expect_true(added$effects$changed)
  expect_equal(dim(added$spec$matrices$DRIFT), c(3L, 3L))

  fields$latent_names <- c("renamed", "eta2")
  fields$manifest_names <- c("renamed_y", "y2")
  fields$manifest_type <- c(0L, 0L)
  renamed <- suppressWarnings(suppressMessages(
    ctgui_commit_spec_fields(spec, fields)
  ))
  expect_equal(rownames(renamed$spec$matrices$DRIFT), fields$latent_names)

  fields$latent_names <- "eta2"
  fields$manifest_names <- "y2"
  fields$manifest_type <- 0L
  deleted <- suppressWarnings(suppressMessages(
    ctgui_commit_spec_fields(spec, fields)
  ))
  expect_equal(dim(deleted$spec$matrices$LAMBDA), c(1L, 1L))
})

test_that("matrix edits commit matrices metadata and PARS once", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = "eta", manifest_names = "y",
    tipred_names = "group", tipredDefault = FALSE
  )))
  drift <- spec$matrices$DRIFT
  drift["eta", "eta"] <- "custom_a"
  pars <- ctgui_parse_pars_vector("custom_a\ncustom_b")
  commit <- suppressWarnings(suppressMessages(ctgui_apply_matrix_edits(
    spec,
    matrices = list(DRIFT = drift, PARS = pars),
    metadata = list(list(
      matrix = "DRIFT", row = "eta", col = "eta",
      transform = "param", indvarying = TRUE, sdscale = 2,
      tipred_effects = "group", extra_pars = "custom_a, custom_b"
    ))
  )))
  expect_true(commit$effects$changed)
  expect_equal(commit$spec$matrices$DRIFT["eta", "eta"], "custom_a")
  expect_setequal(as.character(commit$spec$matrices$PARS), c("custom_a", "custom_b"))
  row <- subset(
    commit$spec$parameter_metadata,
    matrix == "DRIFT" & row == "eta" & col == "eta"
  )
  expect_true(row$indvarying)
  expect_equal(row$sdscale, 2)
  expect_true(row$group_effect)
})

test_that("manifest additions create their latent before the loading", {
  draft <- ctgui_spec(latent_names = character(), manifest_names = character())
  commit <- ctgui_add_spec_variable(draft, "manifest", "y", measuring = "eta")

  expect_equal(commit$spec$latent_names, "eta")
  expect_equal(commit$spec$manifest_names, "y")
  expect_identical(commit$spec$matrices$LAMBDA["y", "eta"], 1)
  expect_false(is.null(commit$spec$model))
})

test_that("additional manifest measurements receive a free loading", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y1")
  commit <- ctgui_add_spec_variable(spec, "manifest", "y2", measuring = "eta")

  expect_identical(commit$spec$matrices$LAMBDA["y1", "eta"], "1")
  expect_identical(
    commit$spec$matrices$LAMBDA["y2", "eta"],
    ctgui_auto_label("LAMBDA", "y2", "eta")
  )
  metadata <- subset(commit$spec$parameter_metadata,
    matrix == "LAMBDA" & row == "y2" & col == "eta")
  expect_equal(nrow(metadata), 1L)
})

test_that("explicit free edits honor persisted TI all and none policies", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("y1", "y2"),
    tipred_names = c("all", "none"),
    tipredDefault = FALSE
  )))
  spec$visual$tipred_defaults <- list(all = TRUE, none = FALSE)
  updated <- suppressWarnings(suppressMessages(ctgui_set_matrix_value(
    spec, "CINT", "eta1", "CINT", free = TRUE
  )))
  row <- subset(
    updated$parameter_metadata,
    matrix == "CINT" & row == "eta1" & col == "CINT"
  )
  expect_true(row$all_effect)
  expect_false(row$none_effect)
})

test_that("project normalization preserves annotations and visual layouts", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = "eta", manifest_names = "y"
  )))
  spec <- ctgui_set_parameter_metadata(
    spec, "DRIFT", "eta", "eta",
    transform = "2 * param", indvarying = TRUE, sdscale = 1.5
  )
  spec$visual$layouts$state_space <- list(
    "latent:eta" = list(x = 125, y = 245)
  )
  loaded <- ctgui_project_spec(spec)
  expect_s3_class(loaded, "ctsemgui_spec")
  expect_equal(loaded$visual$layouts$state_space, spec$visual$layouts$state_space)
  row <- subset(
    loaded$parameter_metadata,
    matrix == "DRIFT" & row == "eta" & col == "eta"
  )
  expect_equal(row$transform, "2 * param")
  expect_true(row$indvarying)
  expect_equal(row$sdscale, 1.5)
})

test_that("data role and summary services are deterministic", {
  spec <- suppressWarnings(suppressMessages(ctgui_spec(
    latent_names = "eta", manifest_names = "y",
    tipred_names = "group", id = "person", time = "wave"
  )))
  data <- data.frame(
    person = c(1, 1, 2, 2), wave = c(0, 1, 0, 1),
    y = c(1, 2, 3, NA), group = c(0, 0, 1, 1)
  )
  roles <- ctgui_data_role_selection(data, spec)
  expect_equal(roles$manifest_names, "y")
  expect_equal(roles$tipred_names, "group")
  expect_equal(roles$id, "person")
  expect_equal(roles$time, "wave")
  expect_false("person" %in% roles$manifest_choices)
  expect_false("person" %in% roles$tdpred_choices)
  expect_false("person" %in% roles$tipred_choices)
  expect_false("wave" %in% roles$manifest_choices)
  manual <- ctgui_data_role_selection(
    data.frame(observed = 1),
    suppressWarnings(suppressMessages(ctgui_spec(
      latent_names = "eta", manifest_names = "typed_y",
      tdpred_names = "typed_event", tipred_names = "typed_group",
      id = "typed_id", time = "typed_time"
    )))
  )
  expect_true(all(c(
    "observed", "typed_y", "typed_event", "typed_group",
    "typed_id", "typed_time"
  ) %in% manual$choices))
  expect_equal(manual$manifest_names, "typed_y")
  expect_false("typed_id" %in% manual$manifest_choices)
  expect_false("typed_time" %in% manual$manifest_choices)
  expect_equal(nrow(ctgui_data_preview(data)), 4L)
  expect_true("y" %in% ctgui_data_summary(data)$variable)
  expect_equal(
    subset(ctgui_missingness_summary(data), variable == "y")$missing,
    1L
  )
  expect_true("y" %in% ctgui_within_between_summary(data, spec)$variable)
  expect_equal(nrow(ctgui_tipred_subject_data(data, spec)$values), 2L)

  matrix_data <- as.matrix(data)
  roles <- ctgui_data_role_selection(matrix_data, spec)
  expect_true(all(c("person", "wave", "y", "group") %in% roles$choices))
  expect_false("person" %in% roles$tipred_choices)
  expect_equal(nrow(ctgui_data_preview(matrix_data)), 4L)
  expect_true("y" %in% ctgui_data_summary(matrix_data)$variable)
})
