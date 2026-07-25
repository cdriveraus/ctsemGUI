test_that("visual draft service builds the three canonical projections", {
  spec <- ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("y1", "y2"),
    tipred_names = "group"
  )
  drafts <- ctgui_visual_draft_set(spec)

  expect_named(
    drafts, c("state_space", "initial_state", "tipred_effects"),
    ignore.order = FALSE
  )
  expect_true(all(vapply(drafts, ctgui_visual_validate_graph, logical(1L))))
  expect_true(all(vapply(drafts, function(graph) {
    identical(graph$protocol_version, ctgui_visual_graph_protocol_version)
  }, logical(1L))))
})

test_that("visual server commits layouts and graph edits through its boundary", {
  skip_if_not_installed("shiny")
  initial <- ctgui_spec(latent_names = "eta", manifest_names = "y")

  server <- function(input, output, session) {
    current_spec <- shiny::reactiveVal(initial)
    current_data <- shiny::reactiveVal(data.frame(
      id = 1L, time = 0, y = 1
    ))
    commits <- shiny::reactiveVal(character())
    messages <- shiny::reactiveVal(list())
    sync_count <- shiny::reactiveVal(0L)
    fit_status <- shiny::reactiveVal("")
    matrix_status <- shiny::reactiveVal("")

    commit <- function(updated, reason = "edit", ...) {
      current_spec(updated)
      commits(c(commits(), reason))
      invisible(list())
    }
    sync <- function(spec) sync_count(sync_count() + 1L)
    send <- function(type, message) {
      messages(c(messages(), list(list(type = type, message = message))))
    }
    visual <- ctgui_visual_server(
      input, output, session, current_spec, current_data, commit, sync,
      fit_status, matrix_status, send_message = send,
      notify = function(...) NULL
    )
  }

  suppressWarnings(shiny::testServer(server, {
    visual$reset()
    session$flushReact()
    expect_named(
      visual$drafts(),
      c("state_space", "initial_state", "tipred_effects"),
      ignore.order = FALSE
    )
    expect_equal(tail(messages(), 1L)[[1L]]$type, "ctgui-visual-load")
    expect_equal(
      tail(messages(), 1L)[[1L]]$message$data_columns,
      c("id", "time", "y")
    )

    graph <- visual$drafts()$state_space
    graph$nodes[[1L]]$x <- 777
    graph$layout_only <- TRUE
    session$setInputs(visual_spec_canvas_graph = graph)
    expect_equal(
      current_spec()$visual$layouts$state_space[[graph$nodes[[1L]]$id]]$x,
      777
    )
    expect_equal(tail(commits(), 1L), "visual_layout")
    expect_equal(visual$status(), "Visual layout saved.")

    graph <- visual$drafts()$state_space
    edge_index <- which(vapply(graph$edges, function(edge) {
      identical(edge$matrix, "DRIFT") &&
        identical(edge$row, "eta") && identical(edge$col, "eta")
    }, logical(1L)))[1L]
    graph$edges[[edge_index]]$value <- "changed_drift"
    graph$edges[[edge_index]]$label <- "changed_drift"
    graph$edges[[edge_index]]$fixed <- FALSE
    graph$layout_only <- FALSE
    session$setInputs(visual_spec_canvas_graph = graph)
    expect_equal(current_spec()$matrices$DRIFT["eta", "eta"], "changed_drift")
    expect_equal(tail(commits(), 1L), "visual_graph")
    expect_equal(sync_count(), 1L)
    expect_equal(matrix_status(), "Visual change applied to model matrices.")
  }))
})
