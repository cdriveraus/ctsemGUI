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
