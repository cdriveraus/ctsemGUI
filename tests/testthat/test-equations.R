ctgui_equation_view <- getFromNamespace("ctgui_equation_view", "ctsemGUI")
ctgui_equation_blocks <- getFromNamespace("ctgui_equation_blocks", "ctsemGUI")
ctgui_equation_macros <- getFromNamespace("ctgui_equation_macros", "ctsemGUI")
ctgui_equation_is_known_shape <- getFromNamespace("ctgui_equation_is_known_shape", "ctsemGUI")
ctgui_equation_view_ui <- getFromNamespace("ctgui_equation_view_ui", "ctsemGUI")

quiet_equations <- function(code) suppressWarnings(suppressMessages(force(code)))

equation_spec <- function(...) {
  quiet_equations(ctgui_spec(
    latent_names = c("eta1", "eta2"),
    manifest_names = c("y1", "y2"),
    ...
  ))
}

test_that("ctModelLatex still emits the document shape this viewer splits", {
  skip_if_not_installed("ctsem")

  # ctModelLatex() is not a documented interface. This test exists to make a
  # change in its output a loud failure here rather than a silent fallback to
  # raw LaTeX in the running application. If it fails, compare the current
  # output against ctgui_equation_split_rows() and update the parser; do not
  # relax the assertion.
  latex <- quiet_equations(ctgui_latex(equation_spec()))
  expect_true(
    ctgui_equation_is_known_shape(latex),
    info = "ctsem's equation layout changed; ctgui_equation_split_rows() needs updating."
  )
  expect_equal(ctgui_equation_view(latex)$status, "rows")
})

test_that("recognised documents split into labelled rows", {
  skip_if_not_installed("ctsem")

  blocks <- ctgui_equation_blocks(quiet_equations(ctgui_latex(equation_spec())))
  expect_gt(length(blocks), 1L)

  labels <- vapply(blocks, function(block) block$label, character(1L))
  expect_true(all(nzchar(labels)))
  # A label is prose for the reader, so none of the box-layout TeX that
  # produced it may survive into the rendered page.
  expect_false(any(grepl("parbox|linebreak|centering|[{}]", labels)))
})

test_that("row fragments are self-contained math without document scaffolding", {
  skip_if_not_installed("ctsem")

  blocks <- ctgui_equation_blocks(quiet_equations(
    ctgui_latex(equation_spec(tdpred_names = "TD1", tipred_names = "age"))
  ))
  expect_gt(length(blocks), 1L)

  for (block in blocks) {
    expect_match(block$tex, "^\\\\begin\\{aligned\\}")
    expect_match(block$tex, "\\\\end\\{aligned\\}$")
    # flalign*, \parbox and \setcounter are page instructions KaTeX rejects;
    # a trailing \\ inside aligned renders as a stray empty line.
    expect_false(grepl("flalign", block$tex, fixed = TRUE))
    expect_false(grepl("parbox", block$tex, fixed = TRUE))
    expect_false(grepl("setcounter", block$tex, fixed = TRUE))
    expect_false(grepl("nonumber", block$tex, fixed = TRUE))
    expect_false(grepl("\\\\\\\\\\s*\\\\end\\{aligned\\}$", block$tex))
  }
})

test_that("every macro the fragments use is supplied to the renderer", {
  skip_if_not_installed("ctsem")

  blocks <- ctgui_equation_blocks(quiet_equations(ctgui_latex(equation_spec())))
  tex <- paste(vapply(blocks, function(block) block$tex, character(1L)), collapse = " ")

  # ctsem's \vect has no KaTeX equivalent. It carries the vector symbol in
  # every underbrace annotation, so an unsupplied macro is not a cosmetic
  # loss: the row fails to typeset entirely.
  expect_true(grepl("\\vect", tex, fixed = TRUE))
  expect_true("\\vect" %in% names(ctgui_equation_macros()))
})

test_that("an unfamiliar document falls back to source rather than a guess", {
  # Silently parsing a layout this code does not understand would produce a
  # plausible-looking but wrong equation, which is worse than showing source.
  unfamiliar <- ctgui_equation_view("\\begin{equation} x = y \\end{equation}")
  expect_equal(unfamiliar$status, "source")
  expect_match(unfamiliar$message, "LaTeX source", fixed = TRUE)
  expect_equal(unfamiliar$source, "\\begin{equation} x = y \\end{equation}")
  expect_length(unfamiliar$blocks, 0L)

  # flalign* without any \parbox carries no row labels to split on.
  headless <- ctgui_equation_view("\\begin{flalign*} x = y \\end{flalign*}")
  expect_equal(headless$status, "source")
})

test_that("a ctsem rendering failure is reported as such", {
  failed <- ctgui_equation_view("Could not create equations: no model")
  expect_equal(failed$status, "source")
  expect_match(failed$message, "Could not create equations", fixed = TRUE)

  fallback <- ctgui_equation_view("% ctsem renderer error: boom\nDRIFT")
  expect_equal(fallback$status, "source")
  expect_match(fallback$message, "ctsem could not render", fixed = TRUE)
})

test_that("absent equations are distinguished from unrenderable ones", {
  for (value in list(character(), "", NA_character_, "   ")) {
    view <- ctgui_equation_view(value)
    expect_equal(view$status, "empty")
    expect_length(view$blocks, 0L)
  }
})

test_that("rows reach the page as TeX for the browser, never as an image", {
  skip_if_not_installed("shiny")

  html <- as.character(ctgui_equation_view_ui(list(
    status = "rows", source = "", message = "",
    blocks = list(list(label = "Deterministic change:", tex = "\\begin{aligned}a&=b\\end{aligned}"))
  )))
  expect_match(html, "data-ctgui-tex", fixed = TRUE)
  expect_match(html, "Deterministic change:", fixed = TRUE)
  expect_false(grepl("<img", html, fixed = TRUE))

  source_html <- as.character(ctgui_equation_view_ui(list(
    status = "source", blocks = list(), source = "\\alpha", message = "Unfamiliar layout."
  )))
  expect_match(source_html, "Unfamiliar layout.", fixed = TRUE)
  expect_match(source_html, "alpha", fixed = TRUE)

  empty_html <- as.character(ctgui_equation_view_ui(
    list(status = "empty", blocks = list(), source = "", message = ""),
    empty_message = "Nothing yet."
  ))
  expect_match(empty_html, "Nothing yet.", fixed = TRUE)
})
