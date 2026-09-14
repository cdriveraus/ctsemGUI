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
    # A page ctsem ships that R cannot parse leaves the panel with nothing to
    # show, and the GUI now says so rather than blaming the argument. That is
    # still a dialog with no help in it, so it fails here too.
    expect_false(startsWith(text, "Could not render"))
    expect_false(grepl("\\\\n", text))
    expect_false(grepl(rawToChar(as.raw(8)), text, fixed = TRUE))
    # Rd2txt renders emphasis for a terminal as an underline: the character,
    # a backspace, then an underscore. Left in, that reads as "_s_i_m_p_l_e".
    # Matching any control character instead flagged an emphasised word that
    # merely happened to end a line, which is fine as it stands.
    expect_false(grepl(paste0("_", rawToChar(as.raw(8))), text, fixed = TRUE))
    expect_false(grepl(paste0(rawToChar(as.raw(8)), "_"), text, fixed = TRUE))
  }
})

test_that("a topic documented under an alias is still found", {
  skip_if_not_installed("ctsem")
  # ctsem documents several functions per page and keeps renamed ones as
  # aliases: ctFitCovCheck lives in ctFitCheckCov.Rd. Looking for a file named
  # after the topic reported "No ctsem help found" for a function that is
  # perfectly well documented, which is what \alias entries exist to prevent.
  rd_db <- tools::Rd_db("ctsem")
  ctgui_ctsem_rd_file <- getFromNamespace("ctgui_ctsem_rd_file", "ctsemGUI")
  ctgui_rd_aliases <- getFromNamespace("ctgui_rd_aliases", "ctsemGUI")

  skip_if_not("ctFitCheckCov.Rd" %in% names(rd_db), "ctsem without ctFitCheckCov")
  expect_true("ctFitCovCheck" %in% ctgui_rd_aliases(rd_db[["ctFitCheckCov.Rd"]]))
  expect_equal(ctgui_ctsem_rd_file(rd_db, "ctFitCovCheck"), "ctFitCheckCov.Rd")
  # A page named after its topic still resolves to itself.
  expect_equal(ctgui_ctsem_rd_file(rd_db, "ctFitCheckCov"), "ctFitCheckCov.Rd")
  expect_null(ctgui_ctsem_rd_file(rd_db, "notAFunctionAnywhere"))

  expect_false(startsWith(ctgui_ctsem_help_text("ctFitCovCheck"), "No ctsem help"))
  expect_match(ctgui_ctsem_help_text("ctFitCovCheck", "lags"), "lags", fixed = TRUE)
})

test_that("an argument is read from the Rd tree, not from rendered text", {
  skip_if_not_installed("ctsem")
  # Exact where a regex over Rd2txt output is not, and it still works on a page
  # Rd2txt refuses outright -- one apostrophe in an \item opens a quoted string
  # that never closes, and everything after it is lost.
  ctgui_rd_argument_text <- getFromNamespace("ctgui_rd_argument_text", "ctsemGUI")
  rd_db <- tools::Rd_db("ctsem")
  skip_if_not("ctFitCheckCov.Rd" %in% names(rd_db), "ctsem without ctFitCheckCov")

  text <- ctgui_rd_argument_text(rd_db[["ctFitCheckCov.Rd"]], "cor")
  expect_true(startsWith(text, "cor:"))
  expect_false(grepl("\n", text, fixed = TRUE))
  expect_null(ctgui_rd_argument_text(rd_db[["ctFitCheckCov.Rd"]], "notAnArgument"))
})
