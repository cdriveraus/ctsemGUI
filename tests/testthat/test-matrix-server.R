test_that("matrix batches preserve annotations and commit as one spec", {
  apply_batch <- getFromNamespace("ctgui_apply_matrix_batch", "ctsemGUI")
  spec <- ctgui_spec(
    latent_names = "eta", manifest_names = "y", tipred_names = "group"
  )
  spec <- ctgui_set_matrix_value(
    spec, "DRIFT", "eta", "eta", label = "decay"
  )
  values <- list(DRIFT = spec$matrices$DRIFT)
  values$DRIFT["eta", "eta"] <- "decay_edited"
  metadata <- list(list(
    matrix = "DRIFT", row = "eta", col = "eta",
    transform = "exp(param)", indvarying = TRUE, sdscale = 0.5,
    tipred_effects = "group", extra_pars = "gain"
  ))

  result <- apply_batch(spec, values, metadata)

  expect_equal(result$spec$matrices$DRIFT["eta", "eta"], "decay_edited")
  row <- getFromNamespace("ctgui_matrix_metadata_row", "ctsemGUI")(
    result$spec, "DRIFT", "eta", "eta"
  )
  expect_equal(row$transform, "exp(param)")
  expect_true(row$indvarying)
  expect_equal(row$sdscale, 0.5)
  expect_true(row$group_effect)
  expect_equal(row$extra_pars, "gain")
  expect_true(
    "gain" %in%
      getFromNamespace("ctgui_pars_vector", "ctsemGUI")(result$spec)
  )
  expect_setequal(result$changed, c("DRIFT", "parameter options"))
})

test_that("matrix server renders the retained Matrices tab controls", {
  skip_if_not_installed("shiny")
  server <- getFromNamespace("ctgui_app_server", "ctsemGUI")(
    ctgui_spec(latent_names = "eta", manifest_names = "y"),
    getFromNamespace("ctgui_help_catalog", "ctsemGUI")()
  )

  expect_no_error(suppressWarnings(shiny::testServer(server, {
    session$setInputs(matrix_group = "Dynamics")
    html <- paste(as.character(output$matrix_dynamics_editor), collapse = "\n")
    expect_match(html, "DRIFT", fixed = TRUE)
    expect_match(html, "matrix_network_DRIFT", fixed = TRUE)
  })))
})

test_that("matrix server commits the value carried by its atomic browser event", {
  skip_if_not_installed("shiny")
  server <- getFromNamespace("ctgui_app_server", "ctsemGUI")(
    ctgui_spec(latent_names = "eta", manifest_names = "y"),
    getFromNamespace("ctgui_help_catalog", "ctsemGUI")()
  )

  expect_no_error(suppressWarnings(shiny::testServer(server, {
    session$setInputs(matrix_group = "Dynamics")
    session$setInputs(matrix_commit_nonce = list(
      nonce = 1,
      id = "matrix_cell_CINT_1_1",
      value = "cint_smoke"
    ))
    session$flushReact()

    expect_equal(
      current_spec()$matrices$CINT[1, 1],
      "cint_smoke"
    )
  })))
})

test_that("a stale matrix event cannot restore a visually deleted diffusion path", {
  skip_if_not_installed("shiny")
  initial <- ctgui_spec(
    latent_names = c("eta1", "eta2"), manifest_names = c("y1", "y2")
  )
  server <- getFromNamespace("ctgui_app_server", "ctsemGUI")(
    initial, getFromNamespace("ctgui_help_catalog", "ctsemGUI")()
  )

  expect_no_error(suppressWarnings(shiny::testServer(server, {
    session$setInputs(matrix_group = "Dynamics")
    # Keep the browser's last matrix event at the old correlation value.
    session$setInputs(matrix_commit_nonce = list(
      nonce = 1,
      id = ctgui_matrix_cell_id("DIFFUSION", 2L, 1L),
      value = "diff_eta2_eta1"
    ))

    graph <- ctgui_visual_graph(current_spec(), "state_space")
    graph$edges <- Filter(function(edge) {
      !(identical(edge$matrix, "DIFFUSION") &&
        identical(edge$row, "eta2") && identical(edge$col, "eta1"))
    }, graph$edges)
    session$setInputs(visual_spec_canvas_graph = graph)
    expect_equal(current_spec()$matrices$DIFFUSION["eta2", "eta1"], "0")

    session$setInputs(tab_commit_nonce = list(
      nonce = 2, specification_authored = FALSE, specification = NULL
    ))
    expect_equal(current_spec()$matrices$DIFFUSION["eta2", "eta1"], "0")
  })))
})
