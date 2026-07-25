quiet_ctgui <- function(code) {
  suppressWarnings(suppressMessages(force(code)))
}

ctgui_canonical_snapshot <- function(spec, include_model = TRUE) {
  metadata <- spec$parameter_metadata
  if (!is.null(metadata) && nrow(metadata)) {
    metadata <- metadata[do.call(order, metadata[c("matrix", "row", "col")]), ,
      drop = FALSE]
    rownames(metadata) <- NULL
  }
  matrices <- lapply(spec$matrices, function(value) {
    if (!is.matrix(value)) return(value)
    matrix(as.character(value), nrow(value), ncol(value),
      dimnames = dimnames(value))
  })
  snapshot <- list(
    names = list(
      latent = spec$latent_names, manifest = spec$manifest_names,
      tdpred = spec$tdpred_names, tipred = spec$tipred_names
    ),
    matrices = matrices,
    metadata = metadata,
    pars = spec$matrices$PARS
  )
  if (isTRUE(include_model)) {
    model <- quiet_ctgui(ctgui_to_ctsem_model(spec))
    snapshot$model_matrices <- quiet_ctgui(
      getFromNamespace("ctgui_ctsem_matrices", "ctsemgui")(model)
    )
  }
  snapshot
}

test_that("matrix and visual mutations converge on one canonical model state", {
  visual_update <- getFromNamespace("ctgui_visual_update_edge", "ctsemgui")
  base <- quiet_ctgui(ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("y1", "y2"),
    tipred_names = c("group", "age"),
    tipredDefault = FALSE
  ))

  matrix_path <- quiet_ctgui(
    ctgui_set_matrix_value(base, "DRIFT", "eta1", "eta2", label = "cross")
  )
  matrix_path <- quiet_ctgui(ctgui_set_parameter_metadata(
    matrix_path, "DRIFT", "eta1", "eta2",
    transform = "exp(param)", indvarying = TRUE, sdscale = 0.5,
    tipred_effects = "group", extra_pars = c("shape", "rate")
  ))

  edge <- list(
    matrix = "DRIFT", row = "eta1", col = "eta2", value = "cross",
    transform = "exp(param)", indvarying = TRUE, sdscale = 0.5,
    tipred_effects = "group", extra_pars = c("shape", "rate")
  )
  # A newly free cell first receives ctsem's matrix-specific defaults. The
  # second edit is the same user action as editing its metadata controls.
  visual_path <- quiet_ctgui(visual_update(base, edge))
  visual_path <- quiet_ctgui(visual_update(visual_path, edge))

  expect_equal(
    ctgui_canonical_snapshot(visual_path),
    ctgui_canonical_snapshot(matrix_path)
  )
  expect_equal(
    as.character(matrix_path$matrices$PARS[, 1L]),
    c("shape", "rate")
  )
})

test_that("guided and complete builders converge beyond raw matrix values", {
  template <- quiet_ctgui(ctgui_build_model(
    "dynamic_var_trend", "marker",
    names = list(
      factor_names = c("affect", "arousal"),
      tipred_names = "group"
    ),
    options = list(
      n = 2L, indicators_per_factor = 2L,
      trend_type = "exponential", trend_coupling = "free",
      free_noise_correlations = TRUE, tipredDefault = FALSE
    )
  ))
  guided <- quiet_ctgui(ctgui_spec(
    latent_names = template$latent_names,
    manifest_names = template$manifest_names,
    tipred_names = "group", tipredDefault = FALSE
  ))
  guided <- quiet_ctgui(ctgui_build_matrices(
    guided, "dynamic_var_trend",
    list(
      dynamic_latents = template$builder$dynamic_latents,
      trend_latents = template$builder$trend_latents,
      trend_type = "exponential", trend_coupling = "free",
      free_noise_correlations = TRUE
    )
  ))
  guided <- quiet_ctgui(ctgui_build_measurement_matrices(
    guided, "marker",
    list(
      factor_latents = template$builder$dynamic_latents,
      trend_latents = template$builder$trend_latents,
      manifest_blocks = template$builder$manifest_blocks
    )
  ))

  expect_equal(
    ctgui_canonical_snapshot(guided),
    ctgui_canonical_snapshot(template)
  )
})

test_that("commits characterize unchanged and persisted project state", {
  commit_spec <- getFromNamespace("ctgui_commit_spec", "ctsemgui")
  spec <- quiet_ctgui(ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("y1", "y2"),
    tdpred_names = "event", tipred_names = "group",
    tipredDefault = FALSE
  ))
  spec <- quiet_ctgui(
    ctgui_set_matrix_value(spec, "CINT", "eta1", "CINT", label = "intercept")
  )
  spec <- quiet_ctgui(ctgui_set_parameter_metadata(
    spec, "CINT", "eta1", "CINT",
    transform = "param + offset", indvarying = TRUE, sdscale = 0.25,
    tipred_effects = "group", extra_pars = "offset"
  ))

  unchanged <- quiet_ctgui(commit_spec(
    spec, unserialize(serialize(spec, NULL)), reason = "project-load"
  ))
  expect_false(unchanged$effects$changed)
  expect_false(unchanged$effects$invalidate_fit)
  expect_equal(
    ctgui_canonical_snapshot(unchanged$spec),
    ctgui_canonical_snapshot(spec)
  )
})

test_that("variable add rename and delete preserve surviving canonical cells", {
  spec <- quiet_ctgui(ctgui_spec(
    latent_names = "eta", manifest_names = "y", tdpred_names = "event"
  ))
  graph <- ctgui_visual_graph(spec, "state_space")
  graph$nodes[[length(graph$nodes) + 1L]] <- list(
    id = "latent:eta2", kind = "latent", name = "eta2",
    original_name = "eta2", label = "eta2", x = 400, y = 200
  )
  added <- quiet_ctgui(ctgui_visual_apply_graph(spec, graph))
  added <- quiet_ctgui(
    ctgui_set_matrix_value(added, "DRIFT", "eta2", "eta", label = "coupling")
  )

  graph <- ctgui_visual_graph(added, "state_space")
  index <- which(vapply(graph$nodes, function(node) {
    identical(node$id, "latent:eta2")
  }, logical(1L)))
  graph$nodes[[index]]$name <- "state2"
  for (edge_index in seq_along(graph$edges)) {
    graph$edges[[edge_index]]$row <- sub(
      "^eta2$", "state2", graph$edges[[edge_index]]$row
    )
    graph$edges[[edge_index]]$col <- sub(
      "^eta2$", "state2", graph$edges[[edge_index]]$col
    )
  }
  renamed <- quiet_ctgui(ctgui_visual_apply_graph(added, graph))
  expect_equal(renamed$matrices$DRIFT["state2", "eta"], "coupling")

  graph <- ctgui_visual_graph(renamed, "state_space")
  graph$nodes <- Filter(function(node) {
    !(identical(node$kind, "latent") && identical(node$name, "state2")) &&
      !(identical(node$kind, "tdpred") && identical(node$name, "event"))
  }, graph$nodes)
  deleted <- quiet_ctgui(ctgui_visual_apply_graph(renamed, graph))
  expect_equal(deleted$latent_names, "eta")
  expect_length(deleted$tdpred_names, 0L)
  expect_false("state2" %in% rownames(deleted$matrices$DRIFT))
  expect_false("state2" %in% deleted$parameter_metadata$row)
})

test_that("free fixed expression metadata and TI policies survive canonicalization", {
  spec <- quiet_ctgui(ctgui_spec(
    latent_names = c("eta1", "eta2"), manifest_names = c("y1", "y2"),
    tipred_names = c("all", "none"), tipredDefault = FALSE
  ))
  spec$visual$tipred_defaults <- list(all = TRUE, none = FALSE)
  spec <- quiet_ctgui(
    ctgui_set_matrix_value(spec, "CINT", "eta1", "CINT", free = TRUE)
  )
  spec <- quiet_ctgui(ctgui_set_parameter_metadata(
    spec, "CINT", "eta1", "CINT",
    transform = "exp(param) + offset", indvarying = TRUE, sdscale = 0.4,
    extra_pars = "offset"
  ))
  metadata <- ctgui_visual_metadata(spec, "CINT", "eta1", "CINT")
  expect_true(metadata$all_effect)
  expect_false(metadata$none_effect)
  expect_true(metadata$indvarying)
  expect_equal(metadata$sdscale, 0.4)
  expect_equal(metadata$transform, "exp(param) + offset")
  expect_equal(as.character(spec$matrices$PARS[, 1L]), "offset")

  fixed <- quiet_ctgui(
    ctgui_set_matrix_value(spec, "CINT", "eta1", "CINT", free = FALSE)
  )
  expect_equal(as.character(fixed$matrices$CINT["eta1", "CINT"]), "0")
  expect_false(any(
    fixed$parameter_metadata$matrix == "CINT" &
      fixed$parameter_metadata$row == "eta1" &
      fixed$parameter_metadata$col == "CINT"
  ))
})

test_that("dormant T0VAR survives ctsem model construction", {
  spec <- quiet_ctgui(ctgui_spec(
    latent_names = c("eta1", "eta2"), manifest_names = c("y1", "y2")
  ))
  spec <- quiet_ctgui(
    ctgui_set_matrix_value(spec, "T0VAR", "eta2", "eta1", label = "t0_cov")
  )
  spec <- quiet_ctgui(ctgui_set_parameter_metadata(
    spec, "T0MEANS", "eta1", "T0MEANS", indvarying = TRUE
  ))
  before <- spec$matrices$T0VAR
  model <- quiet_ctgui(ctgui_to_ctsem_model(spec))
  restored <- quiet_ctgui(ctgui_spec_from_model(model))

  expect_equal(spec$matrices$T0VAR, before)
  # ctsem intentionally suppresses T0VAR cells while the corresponding
  # initial mean varies; the GUI's authoritative state must retain them.
  expect_equal(as.character(spec$matrices$T0VAR["eta2", "eta1"]), "t0_cov")
  expect_true(is.matrix(restored$matrices$T0VAR))
})

test_that("ctsem 3.11.1 capability and GUI round-trip contract is explicit", {
  capabilities <- getFromNamespace("ctgui_ctsem_capabilities", "ctsemgui")()
  expected_exports <- c(
    "ctModel", "ctModelMatrices", "ctFit", "ctOptimUncertainty",
    "ctSummaryMatrices", "ctFitCovCheck", "ctPredict", "ctDiscretePars"
  )
  expect_true(capabilities$installed)
  expect_gte(utils::packageVersion("ctsem"), package_version("3.11.1"))
  expect_true(all(expected_exports %in% getNamespaceExports("ctsem")))

  spec <- quiet_ctgui(ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("y1", "y2"),
    id = "participant", time = "occasion",
    tipred_names = "group", tipredDefault = FALSE
  ))
  spec <- quiet_ctgui(
    ctgui_set_matrix_value(spec, "DRIFT", "eta1", "eta2", label = "cross")
  )
  model <- quiet_ctgui(ctgui_to_ctsem_model(spec))
  expect_true(all(c(
    "pars", "latentNames", "manifestNames", "subjectIDname", "timeName",
    "continuoustime"
  ) %in% names(model)))
  restored <- quiet_ctgui(ctgui_spec_from_model(model))
  expect_equal(restored$latent_names, spec$latent_names)
  expect_equal(restored$manifest_names, spec$manifest_names)
  expect_equal(restored$id, spec$id)
  expect_equal(restored$time, spec$time)
  expect_equal(
    quiet_ctgui(getFromNamespace("ctgui_ctsem_matrices", "ctsemgui")(
      ctgui_to_ctsem_model(restored)
    )),
    quiet_ctgui(getFromNamespace("ctgui_ctsem_matrices", "ctsemgui")(model))
  )
})

test_that("all visual projections preserve hostile labels and layouts as data", {
  spec <- quiet_ctgui(ctgui_spec(
    latent_names = "eta", manifest_names = "y",
    tipred_names = "group", tipredDefault = FALSE
  ))
  hostile <- "<img src=x onerror=alert('unsafe')>"
  for (view in c("state_space", "initial_state", "tipred_effects")) {
    graph <- ctgui_visual_graph(spec, view)
    expect_true(length(graph$nodes) > 0L, info = view)
    graph$nodes[[1L]]$label <- hostile
    graph$nodes[[1L]]$x <- 321
    graph$nodes[[1L]]$y <- 123
    expect_true(
      getFromNamespace("ctgui_visual_validate_graph", "ctsemgui")(graph),
      info = view
    )
    spec <- quiet_ctgui(ctgui_visual_apply_graph(spec, graph))
    expect_equal(
      spec$visual$layouts[[view]][[graph$nodes[[1L]]$id]]$x,
      321,
      info = view
    )
  }
})

test_that("plot collections cover empty named unnamed ggplot and base results", {
  gg <- ggplot2::ggplot(
    data.frame(x = 1:2, y = 2:1), ggplot2::aes(x, y)
  ) + ggplot2::geom_point()
  graphics_file <- tempfile(fileext = ".pdf")
  grDevices::pdf(graphics_file)
  on.exit({
    if (grDevices::dev.cur() > 1L) grDevices::dev.off()
    unlink(graphics_file)
  }, add = TRUE)
  graphics::plot(1:2, 2:1)
  base <- grDevices::recordPlot()
  grDevices::dev.off()

  expect_identical(ctgui_plot_collection(NULL), list())
  expect_identical(ctgui_plot_collection(list()), list())
  expect_named(ctgui_plot_collection(gg), "Plot")
  expect_named(ctgui_plot_collection(list(Scatter = gg)), "Scatter")
  expect_named(
    ctgui_plot_collection(list(gg, base, function() graphics::plot(1))),
    c("Plot 1", "Plot 2", "Plot 3")
  )
  expect_s3_class(ctgui_plot_collection(list(Base = base))$Base, "recordedplot")
})
