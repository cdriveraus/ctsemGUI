test_that("visual graphs round trip fitted-model matrices without generation matrices", {
  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2"), manifest_names = c("y1", "y2"),
    tdpred_names = "event", tipred_names = "group"
  )
  spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta1", "eta2", label = "cross")
  spec <- ctgui_set_parameter_metadata(spec, "DRIFT", "eta1", "eta2",
    transform = "param", indvarying = TRUE, tipred_effects = "group", extra_pars = "shape")
  state <- ctgui_visual_graph(spec, "state_space")
  initial <- ctgui_visual_graph(spec, "initial_state")
  restored <- ctgui_visual_apply_graph(spec, state)
  restored <- ctgui_visual_apply_graph(restored, initial)

  expect_equal(as.character(restored$matrices$DRIFT), as.character(spec$matrices$DRIFT))
  expect_equal(as.character(restored$matrices$TDPREDMEANS), as.character(spec$matrices$TDPREDMEANS))
  expect_equal(as.character(restored$matrices$TDPREDVAR), as.character(spec$matrices$TDPREDVAR))
  edge <- Filter(function(edge) identical(edge$matrix, "DRIFT") && identical(edge$row, "eta1") && identical(edge$col, "eta2"), state$edges)[[1]]
  expect_true(edge$indvarying)
  expect_equal(edge$tipred_effects, "group")
  expect_equal(edge$extra_pars, "shape")
})

test_that("default mean constants are offset from their target nodes", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  graph <- ctgui_visual_graph(spec, "state_space")
  initial <- ctgui_visual_graph(spec, "initial_state")
  node <- function(id) Filter(function(item) identical(item$id, id), graph$nodes)[[1L]]
  initial_node <- function(id) Filter(function(item) identical(item$id, id), initial$nodes)[[1L]]

  expect_lt(node("constant:CINT")$y, node("latent:eta")$y)
  expect_gt(node("constant:MANIFESTMEANS")$y, node("manifest:y")$y)
  expect_lt(initial_node("constant:T0MEANS")$y, initial_node("latent:eta")$y)
})

test_that("visual graph messages use the versioned protocol contract", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  graph <- ctgui_visual_graph(spec, "state_space")

  expect_equal(graph$protocol_version, ctgui_visual_graph_protocol_version)
  expect_true(ctgui_visual_validate_graph(graph))
  expect_equal(ctgui_visual_graph_contract()$graph_version, graph$version)

  hostile <- graph
  hostile$nodes[[1L]]$label <- "<img src=x onerror=alert('xss')>"
  expect_true(ctgui_visual_validate_graph(hostile))

  incompatible <- graph
  incompatible$protocol_version <- graph$protocol_version + 1L
  expect_false(ctgui_visual_validate_graph(incompatible))
  expect_error(ctgui_visual_apply_graph(spec, incompatible), "visual graph protocol")
})

test_that("visual browser renderer does not interpolate graph data as HTML", {
  asset <- ctgui_test_asset_path("www", "visual-spec", "visual-spec.js")
  source <- paste(readLines(asset, warn = FALSE), collapse = "\n")

  expect_false(grepl("innerHTML", source, fixed = TRUE))
  expect_false(grepl('document.addEventListener("keydown"', source, fixed = TRUE))
  expect_true(grepl("GRAPH_PROTOCOL_VERSION", source, fixed = TRUE))
  expect_true(grepl("shell.addEventListener(\"keydown\"", source, fixed = TRUE))
  expect_true(grepl("textContent", source, fixed = TRUE))
  expect_true(grepl("Measuring which latent", source, fixed = TRUE))
  expect_true(grepl("ctgui-combo-menu", source, fixed = TRUE))
  expect_true(grepl("ctgui-structural-filters", source, fixed = TRUE))
  expect_true(grepl('"loop-sweep": "25deg"', source, fixed = TRUE))
  expect_true(grepl("Add time-dependent predictor", source, fixed = TRUE))
  expect_true(grepl("Add time-independent predictor", source, fixed = TRUE))
  expect_true(grepl('input.addEventListener("click", renderChoices)', source, fixed = TRUE))
  expect_true(grepl("var excluded = options.excluded || []", source, fixed = TRUE))
  expect_true(grepl("roleList(roles.id)", source, fixed = TRUE))
})

test_that("visual graph maps directed paths and lower-triangular noise paths", {
  spec <- ctgui_spec(latent_names = c("eta1", "eta2"), manifest_names = c("y1", "y2"), tdpred_names = "event")
  graph <- ctgui_visual_graph(spec, "state_space")
  drift <- Filter(function(edge) identical(edge$matrix, "DRIFT") && identical(edge$row, "eta1") && identical(edge$col, "eta2"), graph$edges)[[1]]
  loading <- Filter(function(edge) identical(edge$matrix, "LAMBDA") && identical(edge$row, "y1") && identical(edge$col, "eta1"), graph$edges)[[1]]
  diffusion <- Filter(function(edge) identical(edge$matrix, "DIFFUSION") && identical(edge$row, "eta2") && identical(edge$col, "eta1"), graph$edges)[[1]]
  expect_equal(drift$source, "latent:eta2")
  expect_equal(drift$target, "latent:eta1")
  expect_equal(loading$source, "latent:eta1")
  expect_equal(loading$target, "manifest:y1")
  expect_false(diffusion$directed)
  expect_equal(diffusion$edge_kind, "correlation")
  variance <- Filter(function(edge) identical(edge$matrix, "DIFFUSION") && identical(edge$row, "eta1") && identical(edge$col, "eta1"), graph$edges)[[1]]
  input <- Filter(function(edge) identical(edge$id, "input:DIFFUSION:eta1"), graph$edges)[[1]]
  expect_equal(variance$source, "noise:DIFFUSION:eta1")
  expect_equal(variance$target, variance$source)
  expect_equal(variance$edge_kind, "variance")
  expect_equal(variance$label, as.character(spec$matrices$DIFFUSION["eta1", "eta1"]))
  expect_true(input$visual_only)
  expect_true(input$fixed)
  expect_equal(input$value, "1")
  expect_equal(input$label, "1")
  expect_false(input$selectable)
  expect_equal(input$target, "latent:eta1")
})

test_that("automatic parameter labels follow ctModel defaults", {
  expect_equal(ctgui_auto_label("MANIFESTMEANS", "y1", "MANIFESTMEANS"), "mm_y1")
  expect_equal(ctgui_auto_label("CINT", "eta1", "CINT"), "cint_eta1")
  expect_equal(ctgui_auto_label("DRIFT", "eta1", "eta1"), "drift_eta1")
  expect_equal(ctgui_auto_label("DIFFUSION", "eta1", "eta1"), "diff_eta1")
  expect_equal(ctgui_auto_label("MANIFESTVAR", "y1", "y1"), "mvar_y1")
  expect_equal(ctgui_auto_label("T0VAR", "eta1", "eta1"), "T0var_eta1")
  spec <- ctgui_spec(latent_names = "eta1", manifest_names = "y1")
  expect_equal(spec$matrices$MANIFESTMEANS["y1", 1L], "mm_y1")
  expect_equal(spec$matrices$DIFFUSION["eta1", "eta1"], "diff_eta1")
})

test_that("visual parameter metadata can override ctsem random-effect defaults", {
  spec <- ctgui_spec(latent_names = "eta1", manifest_names = "y1")
  spec <- ctgui_set_matrix_value(spec, "CINT", "eta1", "CINT", label = "cint_eta1")
  spec <- ctgui_set_parameter_metadata(spec, "CINT", "eta1", "CINT", indvarying = TRUE)
  spec <- ctgui_set_parameter_metadata(spec, "CINT", "eta1", "CINT", indvarying = FALSE)
  spec <- ctgui_set_parameter_metadata(spec, "MANIFESTMEANS", "y1", "MANIFESTMEANS", indvarying = FALSE)
  cint <- ctgui_visual_metadata(spec, "CINT", "eta1", "CINT")
  manifest_mean <- ctgui_visual_metadata(spec, "MANIFESTMEANS", "y1", "MANIFESTMEANS")
  expect_false(cint$indvarying[1L])
  expect_false(manifest_mean$indvarying[1L])
})

test_that("new visual parameters inherit the same ctsem metadata defaults as matrix edits", {
  base <- ctgui_spec(latent_names = "eta", manifest_names = c("y1", "y2"))
  matrix_spec <- ctgui_set_matrix_value(base, "LAMBDA", "y2", "eta", free = TRUE)
  visual_spec <- ctgui_visual_update_edge(base, list(
    matrix = "LAMBDA", row = "y2", col = "eta", value = "__free__"
  ))
  matrix_meta <- ctgui_visual_metadata(matrix_spec, "LAMBDA", "y2", "eta")
  visual_meta <- ctgui_visual_metadata(visual_spec, "LAMBDA", "y2", "eta")
  expect_true(nzchar(matrix_meta$transform[1L]))
  expect_equal(visual_meta$transform[1L], matrix_meta$transform[1L])
  expect_equal(visual_meta$indvarying[1L], matrix_meta$indvarying[1L])
  expect_equal(visual_meta$sdscale[1L], matrix_meta$sdscale[1L])
})

test_that("additional visual PARS parameters inherit ctsem metadata defaults", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  updated <- ctgui_visual_update_edge(spec, list(
    matrix = "DRIFT", row = "eta", col = "eta",
    value = as.character(spec$matrices$DRIFT["eta", "eta"]),
    extra_pars = "shape"
  ))
  metadata <- ctgui_visual_metadata(updated, "PARS", "PARS1", "PARS")
  expect_equal(metadata$param[1L], "shape")
  expect_equal(metadata$transform[1L], "param")
  updated <- ctgui_set_parameter_metadata(
    updated, "PARS", "PARS1", "PARS", transform = ""
  )
  metadata <- ctgui_visual_metadata(updated, "PARS", "PARS1", "PARS")
  expect_equal(metadata$transform[1L], "param")
})

test_that("blank identity transforms are displayed explicitly", {
  expect_equal(ctgui_display_transform(""), "param")
  expect_equal(ctgui_display_transform(NA_character_), "param")
  expect_equal(ctgui_display_transform("exp(param)"), "exp(param)")
})

test_that("initial visual view preserves T0VAR cells suppressed by random T0MEANS", {
  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("y1", "y2")
  )
  spec <- ctgui_set_matrix_value(spec, "T0VAR", "eta2", "eta1", label = "initial_correlation")
  spec <- ctgui_set_parameter_metadata(
    spec, "T0MEANS", "eta2", "T0MEANS", indvarying = FALSE
  )
  spec <- ctgui_set_parameter_metadata(
    spec, "T0MEANS", "eta1", "T0MEANS", indvarying = TRUE
  )
  before <- spec$matrices$T0VAR
  graph <- ctgui_visual_graph(spec, "initial_state")
  expect_false(any(vapply(graph$edges, function(edge) {
    identical(edge$matrix, "T0VAR") && !isTRUE(edge$visual_only) &&
      (identical(edge$row, "eta1") || identical(edge$col, "eta1"))
  }, logical(1L))))
  suppressed <- Filter(function(edge) {
    identical(edge$id, "inactive:T0VAR:eta1")
  }, graph$edges)[[1L]]
  expect_equal(suppressed$label, "1e-6 (ignored)")
  expect_true(suppressed$visual_only)
  noise <- Filter(function(node) identical(node$id, "noise:T0VAR:eta1"), graph$nodes)[[1L]]
  expect_equal(noise$label, "noise\neta1")
  updated <- ctgui_visual_apply_graph(spec, graph)
  expect_equal(updated$matrices$T0VAR["eta1", ], before["eta1", ])
  expect_equal(updated$matrices$T0VAR[, "eta1"], before[, "eta1"])

  restored <- ctgui_set_parameter_metadata(
    spec, "T0MEANS", "eta1", "T0MEANS", indvarying = FALSE
  )
  restored_graph <- ctgui_visual_graph(restored, "initial_state")
  expect_false(any(vapply(restored_graph$edges, function(edge) {
    identical(edge$id, "inactive:T0VAR:eta1")
  }, logical(1L))))
  expect_true(any(vapply(restored_graph$edges, function(edge) {
    identical(edge$matrix, "T0VAR") && !isTRUE(edge$visual_only) &&
      identical(edge$row, "eta1") && identical(edge$col, "eta1")
  }, logical(1L))))
  expect_true(any(vapply(restored_graph$edges, function(edge) {
    identical(edge$matrix, "T0VAR") && !isTRUE(edge$visual_only) &&
      identical(edge$row, "eta2") && identical(edge$col, "eta1")
  }, logical(1L))))
})

test_that("single-edge visual updates toggle covariance random effects without graph replacement", {
  spec <- ctgui_spec(latent_names = c("eta1", "eta2"), manifest_names = c("y1", "y2"))
  graph <- ctgui_visual_graph(spec, "state_space")
  edge <- Filter(function(item) identical(item$matrix, "DIFFUSION") &&
    identical(item$row, "eta1") && identical(item$col, "eta1"), graph$edges)[[1L]]
  drift_before <- spec$matrices$DRIFT
  edge$indvarying <- TRUE
  updated <- ctgui_visual_update_edge(spec, edge)
  expect_true(ctgui_visual_metadata(updated, "DIFFUSION", "eta1", "eta1")$indvarying[1L])
  expect_equal(updated$matrices$DRIFT, drift_before)
  edge$indvarying <- FALSE
  updated <- ctgui_visual_update_edge(updated, edge)
  expect_false(ctgui_visual_metadata(updated, "DIFFUSION", "eta1", "eta1")$indvarying[1L])
  expect_equal(updated$matrices$DRIFT, drift_before)
})

test_that("visual layouts and variable additions are stored without changing model exports", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  graph <- ctgui_visual_graph(spec, "state_space")
  graph$nodes[[1]]$x <- 333
  graph$nodes[[length(graph$nodes) + 1L]] <- list(
    id = "latent:eta2", kind = "latent", name = "eta2", original_name = "eta2", x = 180, y = 280
  )
  updated <- ctgui_visual_apply_graph(spec, graph)
  expect_equal(updated$version, 3L)
  expect_equal(updated$latent_names, c("eta", "eta2"))
  expect_equal(updated$visual$layouts$state_space[["latent:eta"]]$x, 333)
  model <- ctgui_to_ctsem_model(updated)
  expect_false("visual" %in% names(model))
})

test_that("visual variable renaming preserves incident parameters and TD predictors can be removed", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y", tdpred_names = "event")
  spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta", "eta", label = "auto")
  graph <- ctgui_visual_graph(spec, "state_space")
  graph$nodes[[which(vapply(graph$nodes, function(node) identical(node$id, "latent:eta"), logical(1L)))]]$name <- "state"
  for (i in seq_along(graph$edges)) {
    graph$edges[[i]]$row <- sub("^eta$", "state", graph$edges[[i]]$row)
    graph$edges[[i]]$col <- sub("^eta$", "state", graph$edges[[i]]$col)
  }
  graph$nodes <- Filter(function(node) node$kind != "tdpred", graph$nodes)
  updated <- ctgui_visual_apply_graph(spec, graph)
  expect_equal(updated$latent_names, "state")
  expect_length(updated$tdpred_names, 0L)
  expect_equal(updated$matrices$DRIFT["state", "state"], "auto")
})

test_that("new latent variables receive noise nodes and default diffusion covariances", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  graph <- ctgui_visual_graph(spec, "state_space")
  graph$nodes[[length(graph$nodes) + 1L]] <- list(
    id = "latent:eta2", kind = "latent", name = "eta2", label = "eta2",
    original_name = "eta2", x = 500, y = 300
  )
  updated <- ctgui_visual_apply_graph(spec, graph)
  refreshed <- ctgui_visual_graph(updated, "state_space")
  expect_true(any(vapply(refreshed$nodes, function(node) identical(node$id, "noise:DIFFUSION:eta2"), logical(1L))))
  expect_false(identical(as.character(updated$matrices$DIFFUSION["eta2", "eta"]), "0"))
  expect_true(any(vapply(refreshed$edges, function(edge) identical(edge$matrix, "DIFFUSION") && identical(edge$row, "eta2") && identical(edge$col, "eta"), logical(1L))))
})

test_that("removing variables also removes their regenerated noise nodes", {
  spec <- ctgui_spec(latent_names = c("eta1", "eta2"), manifest_names = c("y1", "y2"))
  graph <- ctgui_visual_graph(spec, "state_space")
  graph$nodes <- Filter(function(node) {
    !(node$kind == "latent" && node$name == "eta2") &&
      !(node$kind == "manifest" && node$name == "y2")
  }, graph$nodes)
  updated <- ctgui_visual_apply_graph(spec, graph)
  refreshed <- ctgui_visual_graph(updated, "state_space")
  ids <- vapply(refreshed$nodes, function(node) node$id, character(1L))
  expect_false("noise:DIFFUSION:eta2" %in% ids)
  expect_false("noise:MANIFESTVAR:y2" %in% ids)
  expect_equal(updated$latent_names, "eta1")
  expect_equal(updated$manifest_names, "y1")
})

test_that("visual additions retain standard estimated noise variances", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  graph <- ctgui_visual_graph(spec, "state_space")
  graph$nodes[[length(graph$nodes) + 1L]] <- list(
    id = "latent:eta2", kind = "latent", name = "eta2", original_name = "eta2", x = 530, y = 255
  )
  updated <- ctgui_visual_apply_graph(spec, graph)
  expect_true(ctgui_visual_cell_active(updated$matrices$DIFFUSION["eta2", "eta2"]))
  expect_true(ctgui_visual_cell_active(updated$matrices$DRIFT["eta2", "eta2"]))
  graph <- ctgui_visual_graph(updated, "state_space")
  graph$nodes[[length(graph$nodes) + 1L]] <- list(
    id = "manifest:y2", kind = "manifest", name = "y2", original_name = "y2", x = 530, y = 430
  )
  updated <- ctgui_visual_apply_graph(updated, graph)
  expect_true(ctgui_visual_cell_active(updated$matrices$MANIFESTVAR["y2", "y2"]))
  expect_true(ctgui_visual_cell_active(updated$matrices$MANIFESTMEANS["y2", 1L]))
  expect_true(ctgui_visual_metadata(updated, "MANIFESTMEANS", "y2", colnames(updated$matrices$MANIFESTMEANS)[1L])$indvarying[1L])
})

test_that("TI visual graph round trips predictor effects", {
  expect_warning(
    spec <- ctgui_spec(latent_names = "eta", manifest_names = "y", tipred_names = c("group", "age"), tipredDefault = FALSE),
    "TI predictors included but no effects specified", fixed = TRUE
  )
  spec <- ctgui_set_parameter_metadata(spec, "DRIFT", "eta", "eta", tipred_effects = "group")
  graph <- ctgui_visual_graph(spec, "tipred_effects")
  expect_true(any(vapply(graph$nodes, function(node) identical(node$kind, "parameter"), logical(1L))))
  edge <- Filter(function(item) identical(item$edge_kind, "tipred_effect") && identical(item$tipred, "group"), graph$edges)[[1L]]
  expect_equal(edge$colour, "#0f766e")
  expect_equal(edge$label, "")
  graph$edges <- Filter(function(item) !identical(item$id, edge$id), graph$edges)
  expect_warning(
    updated <- ctgui_visual_apply_graph(spec, graph),
    "TI predictors included but no effects specified", fixed = TRUE
  )
  meta <- ctgui_visual_metadata(updated, "DRIFT", "eta", "eta")
  expect_false(meta$group_effect[1L])
})

test_that("TI-effect deletion preserves submitted node positions", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y", tipred_names = "group")
  spec <- ctgui_set_parameter_metadata(spec, "DRIFT", "eta", "eta", tipred_effects = "group")
  graph <- ctgui_visual_graph(spec, "tipred_effects")
  graph$nodes[[1L]]$x <- 125; graph$nodes[[1L]]$y <- 225
  parameter_index <- which(vapply(graph$nodes, function(node) identical(node$kind, "parameter"), logical(1L)))[1L]
  graph$nodes[[parameter_index]]$x <- 725; graph$nodes[[parameter_index]]$y <- 325
  graph$edges <- Filter(function(edge) !identical(edge$edge_kind, "tipred_effect"), graph$edges)

  updated <- ctgui_visual_apply_graph(spec, graph)
  refreshed <- ctgui_visual_graph(updated, "tipred_effects")
  expect_equal(refreshed$nodes[[1L]]$x, 125)
  expect_equal(refreshed$nodes[[1L]]$y, 225)
  parameter <- Filter(function(node) identical(node$kind, "parameter"), refreshed$nodes)[[1L]]
  expect_equal(parameter$x, 725)
  expect_equal(parameter$y, 325)
})

test_that("TI predictors added in the state-space visual graph update the specification", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  graph <- ctgui_visual_graph(spec, "state_space")
  graph$nodes[[length(graph$nodes) + 1L]] <- list(
    id = "tipred:group", kind = "tipred", name = "group", label = "group",
    original_name = "group", tipred_default = TRUE, x = 85, y = 430
  )
  updated <- ctgui_visual_apply_graph(spec, graph)
  expect_equal(updated$tipred_names, "group")
  expect_true(all(updated$parameter_metadata$group_effect))
  updated <- ctgui_set_matrix_value(updated, "CINT", "eta", colnames(updated$matrices$CINT)[1L], label = "cint_eta")
  expect_true(ctgui_visual_metadata(updated, "CINT", "eta", colnames(updated$matrices$CINT)[1L])$group_effect[1L])
})

test_that("TI view adds and deletes predictors in the specification", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y", tipred_names = c("group", "age"))
  graph <- ctgui_visual_graph(spec, "tipred_effects")
  graph$nodes <- Filter(function(node) !identical(node$id, "tipred:group"), graph$nodes)
  updated <- ctgui_visual_apply_graph(spec, graph)
  expect_equal(updated$tipred_names, "age")

  graph <- ctgui_visual_graph(updated, "tipred_effects")
  graph$nodes[[length(graph$nodes) + 1L]] <- list(
    id = "tipred:cohort", kind = "tipred", name = "cohort", label = "cohort",
    original_name = "cohort", tipred_default = FALSE, x = 540, y = 315
  )
  updated <- ctgui_visual_apply_graph(updated, graph)
  expect_equal(updated$tipred_names, c("age", "cohort"))
  expect_false(any(updated$parameter_metadata$cohort_effect))
  updated <- ctgui_set_matrix_value(updated, "CINT", "eta", colnames(updated$matrices$CINT)[1L], label = "cint_eta")
  expect_false(ctgui_visual_metadata(updated, "CINT", "eta", colnames(updated$matrices$CINT)[1L])$cohort_effect[1L])
})

test_that("TI graph overlays non-editable random-effect variances and correlations", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y", tipred_names = "group")
  spec <- ctgui_set_parameter_metadata(spec, "DRIFT", "eta", "eta", indvarying = TRUE)
  graph <- ctgui_visual_graph(spec, "tipred_effects")
  overlay <- Filter(function(edge) grepl("^random_effect_", edge$edge_kind %||% ""), graph$edges)
  expect_true(any(vapply(overlay, function(edge) identical(edge$edge_kind, "random_effect_variance"), logical(1L))))
  expect_true(all(vapply(overlay, function(edge) isTRUE(edge$visual_only) && identical(edge$selectable, FALSE), logical(1L))))
})

test_that("visual deletion can return the specification to an uninstantiated draft", {
  spec <- ctgui_spec(latent_names = "eta", manifest_names = "y")
  graph <- ctgui_visual_graph(spec, "state_space")
  graph$nodes <- Filter(function(node) !(node$kind %in% c("latent", "manifest")), graph$nodes)
  updated <- ctgui_visual_apply_graph(spec, graph)
  expect_length(updated$latent_names, 0L)
  expect_length(updated$manifest_names, 0L)
  expect_null(updated$model)
  expect_identical(updated$source, "draft")
})
