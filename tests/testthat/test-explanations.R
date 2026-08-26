ctgui_explanation_catalog <- getFromNamespace("ctgui_explanation_catalog", "ctsemGUI")
ctgui_explanation_ui <- getFromNamespace("ctgui_explanation_ui", "ctsemGUI")
ctgui_explanation_keys <- getFromNamespace("ctgui_explanation_keys", "ctsemGUI")

test_that("every explanation is written at both levels", {
  catalog <- ctgui_explanation_catalog()
  expect_gt(length(catalog), 0L)

  for (key in names(catalog)) {
    entry <- catalog[[key]]
    expect_true(nzchar(entry$brief), info = key)
    expect_true(nzchar(entry$detail), info = key)
    # The brief line orients; the detail teaches. If they were the same length
    # the toggle would not be offering the reader a real choice.
    expect_lt(nchar(entry$brief), nchar(entry$detail))
    # A brief line that runs past a sentence has stopped being brief.
    expect_lt(nchar(entry$brief), 140L)
  }
})

test_that("detail is marked so the browser can hide it without the server", {
  skip_if_not_installed("shiny")

  html <- as.character(ctgui_explanation_ui("dynamics"))
  catalog <- ctgui_explanation_catalog()

  expect_match(html, "ctgui-explain-detail", fixed = TRUE)
  expect_match(html, catalog$dynamics$brief, fixed = TRUE)
  expect_match(html, substr(catalog$dynamics$detail, 1L, 40L), fixed = TRUE)
})

test_that("an unknown key contributes nothing rather than an empty box", {
  expect_null(ctgui_explanation_ui("not_a_panel"))
})

test_that("the application reaches every explanation it defines", {
  ui_files <- c(
    ctgui_test_source_path("R", "app_ui.R"),
    ctgui_test_source_path("R", "app_server.R")
  )
  source <- paste(unlist(lapply(ui_files, readLines, warn = FALSE)), collapse = "\n")

  # An explanation nothing renders is text nobody will ever read; the previous
  # arrangement stranded one that way when its panel was removed.
  for (key in ctgui_explanation_keys()) {
    expect_match(source, paste0('"', key, '"'), fixed = TRUE, info = key)
  }
})

test_that("the toggle defaults to showing explanations", {
  javascript <- paste(
    readLines(ctgui_test_asset_path("www", "app", "app.js"), warn = FALSE),
    collapse = "\n"
  )
  css <- paste(
    readLines(ctgui_test_asset_path("www", "app", "app.css"), warn = FALSE),
    collapse = "\n"
  )

  # Hiding is opt-in: the stored value names the brief state, so an absent or
  # unreadable preference lands on the fuller explanation.
  expect_match(javascript, 'getItem(EXPLAIN_KEY) !== "brief"', fixed = TRUE)
  expect_match(javascript, "ctgui-explain-brief", fixed = TRUE)
  expect_match(css, "ctgui-explain-brief .ctgui-explain-detail", fixed = TRUE)
})
