utils::globalVariables(c("label", "loop", "label_pos", "label_size", "node_type", "edge_colour", "edge_linetype"))

#' Create the ctsemgui Shiny application.
#'
#' @return A `shiny.appobj` ready to run or test.
#' @keywords internal
ctgui_create_app <- function(initial_spec = ctgui_spec(), help_catalog = ctgui_help_catalog()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is required to launch the ctsemgui app", call. = FALSE)
  }
  www_path <- system.file("www", package = "ctsemgui")
  if (nzchar(www_path)) shiny::addResourcePath("ctsemgui-assets", www_path)
  visual_asset_files <- file.path(www_path, "visual-spec", c("visual-spec.js", "visual-spec.css"))
  visual_asset_version <- if (length(visual_asset_files) && all(file.exists(visual_asset_files))) {
    format(max(file.info(visual_asset_files)$mtime), "%Y%m%d%H%M%S")
  } else {
    as.character(utils::packageVersion("ctsemgui"))
  }
  application_asset_files <- file.path(www_path, "app", c("app.js", "app.css"))
  application_asset_version <- if (
      length(application_asset_files) && all(file.exists(application_asset_files))) {
    format(max(file.info(application_asset_files)$mtime), "%Y%m%d%H%M%S")
  } else {
    as.character(utils::packageVersion("ctsemgui"))
  }
  assets <- list(
    www_path = www_path,
    visual_asset_version = visual_asset_version,
    application_asset_version = application_asset_version,
    visual_asset_url = function(file) {
      paste0("ctsemgui-assets/visual-spec/", file, "?v=", visual_asset_version)
    }
  )
  shiny::shinyApp(
    ui = ctgui_app_ui(initial_spec, help_catalog, assets),
    server = ctgui_app_server(initial_spec, help_catalog)
  )
}

#' Launch ctsemgui
#'
#' Launch the ctsemgui Shiny application for specifying, fitting, diagnosing,
#' and exporting continuous-time structural equation models with ctsem.
#'
#' @param launch.browser Passed to `shiny::runApp()`.
#' @param ... Additional arguments passed to `shiny::runApp()`.
#' @export
#' @examples
#' \dontrun{
#' ctgui_launch_app()
#' }
ctgui_launch_app <- function(launch.browser = interactive(), ...) {
  shiny::runApp(ctgui_create_app(), launch.browser = launch.browser, ...)
}
