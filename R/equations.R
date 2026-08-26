# Browser-renderable model equations -------------------------------------------

# ctModelLatex() emits a standalone LaTeX document body built around a flalign*
# environment whose rows are introduced by \parbox row labels.  That is written
# for a TeX engine, and requiring one just to look at the equations excludes
# most people who would use a graphical interface at all.  The structure is
# regular enough to split here: each \parbox opens a labelled row, and
# everything up to the next \parbox is that row's mathematics.
#
# ctModelLatex() is not a documented interface and its output is free to
# change.  Nothing below treats the shape as guaranteed: the document is
# checked against the form this code understands before it is taken apart, and
# anything unrecognised is handed to the user as LaTeX source with a note
# rather than parsed on the assumption that it fits.  A wrong guess would
# otherwise surface as a plausible-looking but silently wrong equation, which
# is worse than showing source.

ctgui_equation_block <- function(label, tex) {
  list(label = label, tex = tex)
}

# The document form this parser understands.  Both markers must be present:
# flalign* is the row container and \parbox is what carries the row labels.
ctgui_equation_is_known_shape <- function(latex) {
  grepl("\\\\begin\\{flalign", latex) && grepl("\\\\parbox\\{", latex)
}

# ctgui_latex() answers with prose when ctsem itself could not produce
# equations, so a document that is really an error message is detected before
# anything tries to render it as mathematics.
ctgui_equation_source_message <- function(latex) {
  if (grepl("^Could not create equations:", latex)) return(latex)
  if (grepl("ctsem renderer error:", latex, fixed = TRUE)) {
    return("ctsem could not render these equations. Showing the model matrices it produced instead.")
  }
  ""
}

# Row labels arrive as \parbox{10em}{\centering{Initial\linebreak and subject...}}.
# The width and centring are page-layout instructions with no meaning here, and
# \linebreak marks a wrap chosen to suit a 10em box rather than a sentence
# break, so it becomes an ordinary space.
ctgui_equation_label_text <- function(label) {
  text <- sub("^\\s*\\\\parbox\\{[^}]*\\}", "", label)
  text <- gsub("\\\\linebreak", " ", text)
  text <- gsub("\\\\centering", "", text)
  text <- gsub("[{}]", "", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

# Structural TeX that positions the rows on a page: it either has no browser
# equivalent or actively breaks the fragment when left in.
ctgui_equation_strip_document <- function(latex) {
  out <- latex
  out <- gsub("\\\\setcounter\\{[^}]*\\}\\{[^}]*\\}", "", out)
  out <- gsub("\\\\(begin|end)\\{flalign\\*?\\}", "", out)
  out <- gsub("\\\\(begin|end)\\{aligned\\}", "", out)
  out <- gsub("\\\\nonumber", "", out)
  out
}

# A row ends in the separators that carried it to the next line of the page
# (\\, and the blank \\ \\ pairs ctsem uses for vertical spacing).  They are
# noise once each row is its own element, and a trailing \\ makes KaTeX report
# an empty final line.
ctgui_equation_trim_row <- function(tex) {
  out <- trimws(tex)
  repeat {
    trimmed <- trimws(sub("(\\\\\\\\|&|\\s)+$", "", out))
    if (identical(trimmed, out)) break
    out <- trimmed
  }
  trimws(sub("^\\s*&", "", out))
}

# Rows keep the & alignment marks that positioned them against each other, so
# each fragment is wrapped in aligned rather than rendered as loose math.
ctgui_equation_wrap_row <- function(tex) {
  if (!nzchar(tex)) return("")
  paste0("\\begin{aligned}", tex, "\\end{aligned}")
}

ctgui_equation_split_rows <- function(latex) {
  body <- ctgui_equation_strip_document(latex)

  pattern <- "\\\\parbox\\{[^}]*\\}\\{((?:[^{}]|\\{[^{}]*\\})*)\\}"
  matches <- gregexpr(pattern, body, perl = TRUE)[[1L]]
  if (identical(as.integer(matches), -1L)) return(list())

  starts <- as.integer(matches)
  lengths <- attr(matches, "match.length")
  labels <- vapply(seq_along(starts), function(i) {
    ctgui_equation_label_text(substr(body, starts[i], starts[i] + lengths[i] - 1L))
  }, character(1L))

  math_starts <- starts + lengths
  math_ends <- c(starts[-1L] - 1L, nchar(body))

  blocks <- lapply(seq_along(starts), function(i) {
    tex <- ctgui_equation_trim_row(substr(body, math_starts[i], math_ends[i]))
    ctgui_equation_block(labels[i], ctgui_equation_wrap_row(tex))
  })
  Filter(function(block) nzchar(block$tex), blocks)
}

#' Prepare ctsem LaTeX equations for display
#'
#' @param latex A LaTeX string as returned by `ctgui_latex()`.
#' @return A list with `status` (`"rows"`, `"source"` or `"empty"`), the
#'   `blocks` to render when the document was recognised, the `source` text,
#'   and a `message` explaining any fallback.
#' @keywords internal
ctgui_equation_view <- function(latex) {
  empty <- list(status = "empty", blocks = list(), source = "", message = "")
  if (length(latex) != 1L || is.na(latex) || !nzchar(trimws(latex))) return(empty)

  failure <- ctgui_equation_source_message(latex)
  if (nzchar(failure)) {
    return(list(status = "source", blocks = list(), source = latex, message = failure))
  }

  if (!ctgui_equation_is_known_shape(latex)) {
    return(list(
      status = "source", blocks = list(), source = latex,
      message = paste(
        "These equations are not in the layout this viewer knows how to split",
        "into rows, so the LaTeX source is shown instead."
      )
    ))
  }

  blocks <- ctgui_equation_split_rows(latex)
  if (!length(blocks)) {
    return(list(
      status = "source", blocks = list(), source = latex,
      message = "No equation rows could be read from this document, so the LaTeX source is shown instead."
    ))
  }
  list(status = "rows", blocks = blocks, source = latex, message = "")
}

#' Split ctsem LaTeX equations into labelled, browser-renderable rows
#'
#' @param latex A LaTeX string as returned by `ctgui_latex()`.
#' @return A list of blocks, each with a `label` and a `tex` fragment. An
#'   unrecognised document yields no blocks; use [ctgui_equation_view()] when
#'   the caller needs to know why.
#' @keywords internal
ctgui_equation_blocks <- function(latex) {
  ctgui_equation_view(latex)$blocks
}

# KaTeX has no \vect; ctsem uses it for the bold vector symbols that carry the
# meaning of every underbrace annotation, so it is supplied to the renderer
# rather than stripped.  A macro ctsem adds later is not silently lost: KaTeX
# reports the undefined control sequence and the row shows its own source.
ctgui_equation_macros <- function() {
  list("\\vect" = "\\boldsymbol{#1}")
}
