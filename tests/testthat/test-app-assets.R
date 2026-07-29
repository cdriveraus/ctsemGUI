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
  expect_match(rendered$html, 'id="toggle_app_width"', fixed = TRUE)
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

test_that("application and visual assets have independent cache versions", {
  source <- paste(readLines(ctgui_test_source_path("R", "app.R")),
    collapse = "\n")

  expect_match(source, 'application_asset_files <- file.path(www_path, "app"',
    fixed = TRUE)
  expect_match(source, "application_asset_version = application_asset_version",
    fixed = TRUE)
})

test_that("reference section exposes the complete curated reading list", {
  skip_if_not_installed("shiny")

  ui <- ctgui_app_ui(
    ctgui_spec(),
    ctgui_help_catalog(),
    list(
      visual_asset_url = function(file) file,
      application_asset_version = "reference-contract"
    )
  )
  html <- getFromNamespace("renderTags", "htmltools")(ui)$html

  expect_match(html, 'data-value="Reference"', fixed = TRUE)
  expect_lt(
    regexpr('data-value="Output"', html, fixed = TRUE)[[1L]],
    regexpr('data-value="Reference"', html, fixed = TRUE)[[1L]]
  )
  expect_match(html, "ctgui-reference-grid", fixed = TRUE)
  expect_match(html, "ctsem GitHub quick start", fixed = TRUE)
  expect_match(html, "Core papers", fixed = TRUE)
  expect_match(html, "Further reading", fixed = TRUE)
  expect_match(html, "Practical posts", fixed = TRUE)
  expect_match(html, "Driver, C. C. (2025). Inference with cross-lagged effects—Problems in time. Psychological Methods, 30(1), 174–202.", fixed = TRUE)
  expect_match(html, "Ryan, O., &amp; Hamaker, E. L. (2022). Time to intervene: A continuous-time approach to network analysis and centrality. Psychometrika, 87(1), 214–252.", fixed = TRUE)
  expect_match(html, "https://doi.org/10.18637/jss.v077.i05", fixed = TRUE)
  expect_match(html, "https://doi.org/10.1037/met0000665", fixed = TRUE)
  expect_match(html, "https://doi.org/10.1007/s11336-021-09767-0", fixed = TRUE)
  expect_match(html, "https://doi.org/10.1080/00273171.2018.1496813", fixed = TRUE)
  expect_match(html, "https://doi.org/10.1037/a0038889", fixed = TRUE)
  expect_match(html, "https://cdriver.netlify.app/post/ctsem-quick-start/", fixed = TRUE)
  expect_match(html, "https://cdriver.netlify.app/post/", fixed = TRUE)
  expect_match(html, "Worked example; its code may use legacy ctsem function names.", fixed = TRUE)

  ui_source <- paste(readLines(ctgui_test_source_path("R", "app_ui.R"), warn = FALSE), collapse = "\n")
  reference_source <- paste(readLines(ctgui_test_source_path("R", "references.R"), warn = FALSE), collapse = "\n")
  expect_match(ui_source, "ctgui_reference_ui()", fixed = TRUE)
  expect_false(grepl("ctsem GitHub quick start", ui_source, fixed = TRUE))
  expect_match(reference_source, "ctgui_reference_catalog", fixed = TRUE)
  expect_match(reference_source, "Time to intervene", fixed = TRUE)
})

test_that("application event handlers are scoped to the application root", {
  javascript <- paste(
    readLines(ctgui_test_asset_path("www", "app", "app.js")),
    collapse = "\n"
  )

  expect_match(javascript, '$("#ctgui-app")', fixed = TRUE)
  expect_false(grepl("$(document).on", javascript, fixed = TRUE))
  expect_false(grepl("document.addEventListener", javascript, fixed = TRUE))
  expect_false(grepl("innerHTML", javascript, fixed = TRUE))
  expect_match(javascript, 'app.on("click", "#run_fit"', fixed = TRUE)
  expect_match(javascript, 'app.on("click", "#toggle_app_width"', fixed = TRUE)
  expect_match(javascript, 'ctgui-full-width', fixed = TRUE)
  expect_match(javascript, 'app.on("mousedown", ".selectize-control"', fixed = TRUE)
  expect_match(javascript, "selectize.open()", fixed = TRUE)
  expect_match(javascript, 'Shiny.addCustomMessageHandler("ctgui-fit-finished"', fixed = TRUE)
})

test_that("matrix commits carry the edited value atomically", {
  javascript <- paste(
    readLines(ctgui_test_asset_path("www", "app", "app.js")),
    collapse = "\n"
  )

  expect_match(
    javascript,
    'Shiny.setInputValue("matrix_commit_nonce", {',
    fixed = TRUE
  )
  expect_match(javascript, "id: this.id", fixed = TRUE)
  expect_match(javascript, "value: $(this).val()", fixed = TRUE)
  expect_match(javascript, "if (!event.originalEvent) return;", fixed = TRUE)
  expect_false(grepl(
    'Shiny.setInputValue(this.id, $(this).val()',
    javascript,
    fixed = TRUE
  ))
})

test_that("tab commits carry specification values atomically", {
  app_js <- paste(
    readLines(
      ctgui_test_source_path("inst", "www", "app", "app.js"),
      warn = FALSE
    ),
    collapse = "\n"
  )

  expect_match(app_js, "function specificationPayload()", fixed = TRUE)
  expect_match(app_js, 'specification: specificationDirty ? specificationPayload() : null', fixed = TRUE)
  expect_match(app_js, "specification_authored: specificationDirty", fixed = TRUE)
  expect_match(app_js, '"latent_names", "manifest_names"', fixed = TRUE)
  expect_match(app_js, '[id^=\\"manifest_type_\\"]', fixed = TRUE)
})

test_that("application stylesheet retains the established UI contracts", {
  css <- paste(
    readLines(ctgui_test_asset_path("www", "app", "app.css")),
    collapse = "\n"
  )

  expect_match(css, ".matrix-cell-inspector", fixed = TRUE)
  expect_match(css, ".matrix-network-layout", fixed = TRUE)
  expect_match(css, ".arg-help:hover", fixed = TRUE)
  expect_match(css, ".fit-inline-output", fixed = TRUE)
  expect_match(css, ".ctgui-readonly-variable-list", fixed = TRUE)
  expect_match(css, ".container-fluid { max-width: 1440px; }", fixed = TRUE)
  expect_match(css, "#ctgui-app.ctgui-full-width", fixed = TRUE)
  expect_match(css, "@media (max-width: 760px)", fixed = TRUE)
})
