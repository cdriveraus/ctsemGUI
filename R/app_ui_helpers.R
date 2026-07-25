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
