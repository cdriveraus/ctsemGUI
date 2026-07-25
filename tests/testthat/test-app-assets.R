test_that("application CSS and event glue are external and versioned", {
  skip_if_not_installed("shiny")

  ui <- ctgui_app_ui(
    ctgui_spec(),
    ctgui_help_catalog(),
    list(
      visual_asset_url = function(file) file,
      application_asset_version = "asset-contract"
    )
  )
  render_tags <- getFromNamespace("renderTags", "htmltools")
  rendered <- render_tags(ui)

  expect_match(rendered$html, 'id="ctgui-app"', fixed = TRUE)
  expect_match(
    rendered$head,
    'href="ctsemgui-assets/app/app.css?v=asset-contract"',
    fixed = TRUE
  )
  expect_match(
    rendered$head,
    'src="ctsemgui-assets/app/app.js?v=asset-contract"',
    fixed = TRUE
  )
  expect_false(grepl("<style", rendered$head, fixed = TRUE))
  expect_false(grepl("addCustomMessageHandler", rendered$head, fixed = TRUE))
})

test_that("application event handlers are scoped to the application root", {
  javascript <- paste(
    readLines(testthat::test_path("..", "..", "inst", "www", "app", "app.js")),
    collapse = "\n"
  )

  expect_match(javascript, '$("#ctgui-app")', fixed = TRUE)
  expect_false(grepl("$(document).on", javascript, fixed = TRUE))
  expect_false(grepl("document.addEventListener", javascript, fixed = TRUE))
  expect_false(grepl("innerHTML", javascript, fixed = TRUE))
  expect_match(javascript, 'app.on("click", "#run_fit"', fixed = TRUE)
  expect_match(javascript, 'Shiny.addCustomMessageHandler("ctgui-fit-finished"', fixed = TRUE)
})

test_that("application stylesheet retains the established UI contracts", {
  css <- paste(
    readLines(testthat::test_path("..", "..", "inst", "www", "app", "app.css")),
    collapse = "\n"
  )

  expect_match(css, ".matrix-cell-inspector", fixed = TRUE)
  expect_match(css, ".matrix-network-layout", fixed = TRUE)
  expect_match(css, ".arg-help:hover", fixed = TRUE)
  expect_match(css, ".fit-inline-output", fixed = TRUE)
  expect_match(css, "@media (max-width: 760px)", fixed = TRUE)
})
