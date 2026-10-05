# What ctsem is handed has to be what the GUI shows, and edits that change
# nothing must leave it alone. Each test here is a way that failed: none of
# them errored, all of them changed the fitted model.

quiet <- function(code) suppressWarnings(suppressMessages(force(code)))
fitted_pars <- function(spec) {
  pars <- quiet(ctgui_to_ctsem_model(spec))$pars
  pars <- pars[!is.na(pars$param), , drop = FALSE]
  pars$transform <- gsub("\\s+", "", pars$transform)
  pars
}
payload <- function(spec, latent_names = spec$latent_names, tipred_names = spec$tipred_names,
    type = spec$type, tipredDefault = spec$tipredDefault) {
  ctgui_spec_fields(list(latent_names = paste(latent_names, collapse = ", "),
    manifest_names = paste(spec$manifest_names, collapse = ", "), tdpred_names = spec$tdpred_names,
    tipred_names = tipred_names, type = type, tipredDefault = tipredDefault,
    id = spec$id, time = spec$time), spec)
}
commit_fields <- function(spec, fields) quiet(ctgui_commit_result(ctgui_commit_spec_fields(spec, fields)))

test_that("TI effects shown are the TI effects fitted, and unticking one removes it", {
  skip_if_not_installed("ctsem")
  spec <- quiet(ctgui_example_spec(ctgui_example("real_data")))
  pars <- fitted_pars(spec)
  metadata <- spec$parameter_metadata
  for (tipred in spec$tipred_names) {
    field <- paste0(tipred, "_effect")
    shown <- stats::setNames(metadata[[field]], metadata$param)
    fitted <- stats::setNames(as.logical(pars[[field]]), pars$param)
    expect_equal(fitted[names(shown)], shown, info = tipred)
  }
  # With tipredDefault on, a parameter with every predictor unticked was
  # still fitted with every predictor: the omitted field meant "default".
  cell <- metadata[metadata$matrix == "DRIFT", ][1L, ]
  off <- quiet(ctgui_set_parameter_metadata(spec, cell$matrix, cell$row, cell$col, tipred_effects = character()))
  row <- fitted_pars(off)
  row <- row[row$param == cell$param, ]
  for (tipred in spec$tipred_names) expect_false(as.logical(row[[paste0(tipred, "_effect")]][1L]), info = tipred)
})

test_that("a model built with tipredDefault = FALSE loads without gaining TI effects", {
  skip_if_not_installed("ctsem")
  model <- quiet(ctsem::ctModel(LAMBDA = diag(2), manifestNames = c("y1", "y2"),
    latentNames = c("a", "b"), TIpredNames = "g", tipredDefault = FALSE, type = "ct", silent = TRUE))
  loaded <- quiet(ctgui_project_spec(model))
  expect_false(any(as.logical(fitted_pars(loaded)$g_effect)))
  expect_false(isTRUE(loaded$tipredDefault))

  # ctsem 3.12 records the setting, which then wins over what the effects
  # suggest; 3.11 does not, and the effects decide.
  recorded <- quiet(ctsem::ctModel(LAMBDA = diag(2), manifestNames = c("y1", "y2"),
    latentNames = c("a", "b"), TIpredNames = "g", tipredDefault = TRUE, type = "ct", silent = TRUE))
  recorded[["tipredDefault"]] <- FALSE
  expect_false(isTRUE(quiet(ctgui_project_spec(recorded))$tipredDefault))
  recorded[["tipredDefault"]] <- NULL
  expect_true(isTRUE(quiet(ctgui_project_spec(recorded))$tipredDefault))
})

test_that("toggling tipredDefault changes, and shows, every parameter's TI effects", {
  skip_if_not_installed("ctsem")
  spec <- quiet(ctgui_example_spec(ctgui_example("real_data")))
  for (state in c(!isTRUE(spec$tipredDefault), isTRUE(spec$tipredDefault))) {
    spec <- commit_fields(spec, payload(spec, tipredDefault = state))
    for (tipred in spec$tipred_names) {
      expect_true(all(spec$parameter_metadata[[paste0(tipred, "_effect")]] == state), info = tipred)
      expect_true(all(as.logical(fitted_pars(spec)[[paste0(tipred, "_effect")]]) == state), info = tipred)
    }
  }
})

test_that("renaming a process in the Specification keeps its parameters' settings", {
  skip_if_not_installed("ctsem")
  spec <- quiet(ctgui_example_spec(ctgui_example("real_data")))
  spec <- quiet(ctgui_set_parameter_metadata(spec, "DRIFT", "etaY1", "etaY2", transform = "exp(param)"))
  spec <- quiet(ctgui_set_parameter_metadata(spec, "DRIFT", "etaY1", "etaY1", indvarying = TRUE, sdscale = 2,
    tipred_effects = "TI1"))
  before <- fitted_pars(spec)
  renamed <- commit_fields(spec, payload(spec, latent_names = c("renamed", spec$latent_names[-1])))
  expect_equal(renamed$latent_names[1], "renamed")
  after <- fitted_pars(renamed)
  fields <- c("param", "transform", "indvarying", "sdscale", paste0(spec$tipred_names, "_effect"))
  key <- function(p) p[order(p$matrix, p$param), fields]
  expect_equal(key(after), key(before), ignore_attr = TRUE)
})

test_that("a renamed TI predictor keeps its effects", {
  skip_if_not_installed("ctsem")
  spec <- quiet(ctgui_example_spec(ctgui_example("real_data")))
  spec <- commit_fields(spec, payload(spec, tipredDefault = FALSE))
  spec <- quiet(ctgui_set_parameter_metadata(spec, "DRIFT", "etaY1", "etaY1", tipred_effects = "TI1"))
  renamed <- commit_fields(spec, payload(spec, tipred_names = c("age", spec$tipred_names[-1])))
  pars <- fitted_pars(renamed)
  expect_true(as.logical(pars$age_effect[pars$param == "drift_etaY1"]))
  expect_equal(sum(as.logical(pars$age_effect)), 1L)
})

test_that("an equality constraint across matrix defaults shares one transform", {
  skip_if_not_installed("ctsem")
  spec <- quiet(ctgui_example_spec(ctgui_example("coupled")))
  label <- as.character(spec$matrices$DRIFT["stress", "stress"])
  shared <- quiet(ctgui_set_matrix_value(spec, "DRIFT", "stress", "sleep", label = label))
  # The new cell names the transform the parameter already has, rather than
  # taking the off-diagonal default. Checked on what the GUI writes, not on a
  # built model: ctsem 3.12 compares those transforms as text, spacing
  # included, so it refuses the same function written two ways
  # (review/GUI-ctsem-issues-2026-10-05.md).
  squash <- function(x) gsub("\\s+", "", x)
  original <- fitted_pars(spec)
  resolved <- squash(original$transform[original$param == label][1L])
  cell <- ctgui_matrices_with_metadata(shared)$DRIFT["stress", "sleep"]
  expect_equal(squash(strsplit(cell, "|", fixed = TRUE)[[1L]][2L]), resolved)
})

test_that("switching to discrete time uses ctsem's discrete-time defaults, also for a saved spec", {
  skip_if_not_installed("ctsem")
  spec <- quiet(ctgui_example_spec(ctgui_example("coupled")))
  dt_default <- function() {
    m <- quiet(ctsem::ctModel(LAMBDA = diag(1), manifestNames = "y", latentNames = "eta", type = "dt", silent = TRUE))
    gsub("\\s+", "", m$pars$transform[m$pars$matrix == "DRIFT"])
  }
  check <- function(spec) {
    dt <- commit_fields(spec, payload(spec, type = "dt"))
    pars <- fitted_pars(dt)
    expect_equal(unique(pars$transform[pars$param %in% c("drift_stress", "drift_sleep")]), dt_default())
  }
  check(spec)
  # A spec saved before transforms were kept blank holds ctsem's resolved
  # continuous-time strings.
  frozen <- spec
  pars <- frozen$pars
  index <- match(frozen$parameter_metadata$param, pars$param)
  frozen$parameter_metadata$transform <- as.character(pars$transform[index])
  check(frozen)
})

test_that("the TI view's commit leaves parameters it does not draw alone", {
  skip_if_not_installed("ctsem")
  spec <- quiet(ctgui_example_spec(ctgui_example("real_data")))
  graph <- ctgui_visual_graph(spec, "tipred_effects")
  after <- quiet(ctgui_visual_apply_graph(spec, graph))
  expect_equal(fitted_pars(after), fitted_pars(spec), ignore_attr = TRUE)
})

test_that("a stale matrix-inspector field does not undo a later visual edit, or move cell", {
  skip_if_not_installed("ctsem")
  skip_if_not_installed("shiny")
  suppressWarnings(shiny::testServer(ctgui_app_server(ctgui_spec(), ctgui_help_catalog()), {
    session$setInputs(example_id = "coupled", example_load = 1, matrix_group = "Dynamics")
    inspector <- paste0("matrix_cell_inspector_", ctgui_matrix_id_part("DRIFT"))
    field <- function(name) ctgui_matrix_meta_id("DRIFT", 1, 2, name)
    session$setInputs(matrix_selected_cell = list(matrix = "DRIFT", row = 1, col = 2))
    invisible(output[[inspector]])
    meta <- ctgui_matrix_metadata_row(current_spec(), "DRIFT", "stress", "sleep")
    do.call(session$setInputs, stats::setNames(list(isTRUE(meta$indvarying)), field("indvarying")))
    # Random effects switched on elsewhere, then an unrelated matrix edit.
    commit_current_spec(quiet(ctgui_set_parameter_metadata(current_spec(), "DRIFT", "stress", "sleep",
      indvarying = TRUE)), reason = "visual_path")
    session$setInputs(matrix_commit_nonce = list(id = ctgui_matrix_cell_id("DRIFT", 2, 2),
      value = "drift_sleep_edited", nonce = 1))
    expect_true(isTRUE(ctgui_matrix_metadata_row(current_spec(), "DRIFT", "stress", "sleep")$indvarying))
    # A real edit in the inspector still applies.
    do.call(session$setInputs, stats::setNames(list("exp(param)"), field("transform")))
    session$setInputs(matrix_metadata_commit = 1)
    expect_equal(ctgui_matrix_metadata_row(current_spec(), "DRIFT", "stress", "sleep")$transform, "exp(param)")
  }))
})

test_that("an expression replaced by a plain label takes its PARS parameters with it", {
  skip_if_not_installed("ctsem")
  spec <- quiet(ctgui_example_spec(ctgui_example("coupled")))
  spec <- quiet(ctgui_set_matrix_value(spec, "DRIFT", "stress", "sleep", label = "a + b * stress"))
  spec <- quiet(ctgui_set_parameter_metadata(spec, "DRIFT", "stress", "sleep", extra_pars = "a, b"))
  expect_setequal(fitted_pars(spec)$param[fitted_pars(spec)$matrix == "PARS"], c("a", "b"))
  plain <- quiet(ctgui_set_matrix_value(spec, "DRIFT", "stress", "sleep", label = "plain"))
  expect_false(any(fitted_pars(plain)$matrix == "PARS"))
})
