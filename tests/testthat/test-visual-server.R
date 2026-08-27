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

    session$setInputs(visual_spec_canvas_reset_layout = list(view = "state_space"))
    expect_equal(current_spec()$visual$layouts$state_space, list())
    expect_equal(tail(commits(), 1L), "visual_layout_reset")
    expect_equal(visual$status(), "Visual layout reset to its default positions.")

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

test_that("replacing the whole model refreshes the visual editor", {
  # The editor rebuilds itself when the Model sub-tab is opened. Opening an
  # example or applying a template switches the top-level tab instead, so no
  # sub-tab change ever fires and the canvas keeps showing a model that is no
  # longer there.
  source <- paste(
    readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE),
    collapse = "\n"
  )

  expect_match(source, "refresh_visual_editor <- function", fixed = TRUE)
  # Every path that swaps the model wholesale has to say so: opening an
  # example, applying a template, and moving through history.
  expect_gte(length(gregexpr("refresh_visual_editor(", source, fixed = TRUE)[[1L]]), 3L)

  # Refreshing on every commit would send the graph back mid-interaction and
  # undo a drag in progress, so it must not be wired into commit_current_spec.
  lines <- readLines(ctgui_test_source_path("R", "app_server.R"), warn = FALSE)
  start <- grep("^commit_current_spec <- function", lines)
  expect_length(start, 1L)
  end <- start + which(lines[start:length(lines)] == "}")[1L] - 1L
  expect_false(any(grepl("refresh_visual_editor", lines[start:end], fixed = TRUE)))
})
