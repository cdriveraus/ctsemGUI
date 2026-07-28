test_that("optimized uncertainty controls use their ctsem Rd entries", {
  help <- ctgui_help_catalog()

  expect_null(help$help_uncertainty_method$text)
  expect_null(help$help_uncertainty_control$text)
  expect_match(ctgui_ctsem_help_text("ctOptimUncertainty", "uncertainty"), "Uncertainty approximation", fixed = TRUE)
  expect_match(ctgui_ctsem_help_text("ctOptimUncertainty", "control"), "method-specific options", fixed = TRUE)
})

test_that("every ctsem help dialog is sourced from readable Rd text", {
  skip_if_not_installed("ctsem")
  help <- ctgui_help_catalog()
  rd_help <- Filter(function(x) !is.null(x$topic), help)

  for (entry in rd_help) {
    text <- ctgui_ctsem_help_text(entry$topic, entry$param %||% NULL)
    expect_false(startsWith(text, "No ctsem help found"))
    expect_false(startsWith(text, "No argument help found"))
    expect_false(grepl("\\\\n", text))
    expect_false(grepl(rawToChar(as.raw(8)), text, fixed = TRUE))
    expect_false(grepl("_[[:cntrl:]]", text))
  }
})
