# Shared application UI helpers ---------------------------------------------

ctgui_plot_export_controls <- function(id, height = 420) {
  shiny::div(
    class = "plot-export",
    shiny::numericInput(
      paste0(id, "_export_width"), "Width (px)",
      value = 700, min = 100, step = 10
    ),
    shiny::numericInput(
      paste0(id, "_export_height"), "Height (px)",
      value = height, min = 100, step = 10
    ),
    shiny::numericInput(
      paste0(id, "_export_dpi"), "DPI",
      value = 96, min = 36, step = 12
    ),
    shiny::downloadButton(paste0(id, "_png"), "PNG"),
    shiny::downloadButton(paste0(id, "_pdf"), "PDF")
  )
}

ctgui_help_link <- function(help_catalog, help_id) {
  help <- help_catalog[[help_id]]
  if (is.null(help)) stop("No help entry found for ", help_id, call. = FALSE)
  tooltip <- ctgui_help_tooltip(help)
  shiny::actionLink(
    help_id, "?", class = "arg-help", title = tooltip,
    `aria-label` = paste("Help:", tooltip)
  )
}

ctgui_arg_label <- function(help_catalog, label, help_id, title = NULL) {
  help <- help_catalog[[help_id]]
  if (is.null(help)) stop("No help entry found for ", help_id, call. = FALSE)
  tooltip <- ctgui_help_tooltip(help)
  shiny::tagList(
    shiny::span(label, title = tooltip),
    ctgui_help_link(help_catalog, help_id)
  )
}

# Equation rows are rendered in the browser from TeX fragments carried on the
# element itself, so the server never has to hold rendered output and a row
# that fails to typeset can still show its own source.
#
# A view whose document ctsem produced in an unfamiliar shape falls back to the
# source rather than to a guess: the reader can still copy it into a TeX
# document, and the note says why they are looking at it.
ctgui_equation_view_ui <- function(view, empty_message = "No equations are available.") {
  if (identical(view$status, "empty")) {
    return(shiny::div(class = "equation-empty", shiny::tags$p(empty_message)))
  }
  if (identical(view$status, "source")) {
    return(shiny::div(
      class = "equation-source-fallback",
      if (nzchar(view$message)) shiny::tags$p(class = "warning-note", view$message),
      # When ctsem's failure text is itself the whole document, repeating it as
      # source says nothing the note has not already said.
      if (!identical(trimws(view$source), trimws(view$message))) shiny::tags$pre(view$source)
    ))
  }
  ctgui_equation_rows_ui(view$blocks, empty_message = empty_message)
}

ctgui_equation_rows_ui <- function(blocks, empty_message = "No equations are available.") {
  if (!length(blocks)) {
    return(shiny::div(class = "equation-empty", shiny::tags$p(empty_message)))
  }
  shiny::tagList(lapply(blocks, function(block) {
    shiny::div(
      class = "equation-row",
      if (nzchar(block$label)) {
        shiny::div(class = "equation-row-label", block$label)
      } else {
        shiny::div(class = "equation-row-label")
      },
      shiny::div(
        class = "equation-row-math",
        shiny::div(class = "equation-math", `data-ctgui-tex` = block$tex)
      )
    )
  }))
}
