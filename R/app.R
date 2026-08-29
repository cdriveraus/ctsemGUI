utils::globalVariables(c(
  "name", "label", "loop", "label_pos", "label_size", "node_type",
  "edge_colour", "edge_linetype"
))

#' Create the ctsemGUI Shiny application.
#'
#' @param initial_state Initial session state from `ctgui_launch_state()`,
#'   holding the opened specification, data, and fit the app starts with.
#' @param initial_spec Convenience override for `initial_state$spec`.
#' @return A `shiny.appobj` ready to run or test.
#' @keywords internal
ctgui_create_app <- function(
    initial_spec = NULL,
    help_catalog = ctgui_help_catalog(),
    initial_state = ctgui_launch_state()) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The shiny package is required to launch the ctsemGUI app", call. = FALSE)
  }
  www_path <- system.file("www", package = "ctsemGUI")
  if (nzchar(www_path)) shiny::addResourcePath("ctsemgui-assets", www_path)
  visual_asset_files <- file.path(www_path, "visual-spec", c("visual-spec.js", "visual-spec.css"))
  visual_asset_version <- if (length(visual_asset_files) && all(file.exists(visual_asset_files))) {
    format(max(file.info(visual_asset_files)$mtime), "%Y%m%d%H%M%S")
  } else {
    as.character(utils::packageVersion("ctsemGUI"))
  }
  application_asset_files <- file.path(www_path, "app", c("app.js", "app.css"))
  application_asset_version <- if (
      length(application_asset_files) && all(file.exists(application_asset_files))) {
    format(max(file.info(application_asset_files)$mtime), "%Y%m%d%H%M%S")
  } else {
    as.character(utils::packageVersion("ctsemGUI"))
  }
  assets <- list(
    www_path = www_path,
    visual_asset_version = visual_asset_version,
    application_asset_version = application_asset_version,
    visual_asset_url = function(file) {
      paste0("ctsemgui-assets/visual-spec/", file, "?v=", visual_asset_version)
    }
  )
  if (!is.null(initial_spec)) initial_state$spec <- initial_spec
  shiny::shinyApp(
    ui = ctgui_app_ui(initial_state$spec, help_catalog, assets,
      initial_tab = initial_state$tab),
    server = ctgui_app_server(initial_state$spec, help_catalog,
      initial_state = initial_state)
  )
}

#' Launch ctsemGUI
#'
#' Launch the ctsemGUI Shiny application for specifying, fitting, diagnosing,
#' and exporting continuous-time structural equation models with ctsem.
#'
#' Called with no arguments the app opens empty, ready to build a model from
#' scratch. Supply `model`, `data`, or `fit` to open work that already exists
#' in the R session: a model written with `ctsem::ctModel()` opens in the
#' visual specification editor with its matrices, transforms, random effects,
#' and TI-predictor effects intact, and a fit additionally makes the
#' diagnostics and output tabs available immediately.
#'
#' Model matrices, parameter labels, transforms, random effects, and
#' TI-predictor moderation are carried into the GUI. `ctModel()` settings the
#' GUI does not itself edit, such as `tipredeffectscale` or the raw population
#' standard deviation controls, are not represented in the specification and
#' revert to their ctsem defaults if the model is rebuilt from the GUI.
#'
#' @param model A model created by `ctsem::ctModel()`, a specification
#'   previously saved by the GUI, a fitted ctsem model, or the path to an
#'   `.rds` file containing one of those. `NULL` starts an empty session.
#' @param data Long-format data to open as the active data set: a
#'   `data.frame`, a matrix, or the path to a `.csv` or `.rds` file.
#' @param fit A fitted ctsem model, as returned by `ctsem::ctFit()`, or the
#'   path to an `.rds` file containing one. When `model` is not given, the
#'   model the fit was fitted with is opened.
#' @param launch.browser Passed to `shiny::runApp()`.
#' @param ... Additional arguments passed to `shiny::runApp()`.
#' @export
#' @examples
#' \dontrun{
#' # Build a model from scratch
#' ctgui_launch_app()
#'
#' # Open a model that was specified in a script
#' model <- ctsem::ctModel(type = "ct", n.latent = 2, n.manifest = 2,
#'   latentNames = c("eta1", "eta2"), manifestNames = c("Y1", "Y2"),
#'   LAMBDA = diag(2))
#' ctgui_launch_app(model, data = mydata)
#'
#' # Open a fit for its diagrams, equations, and diagnostics
#' ctgui_launch_app(fit = myfit)
#' }
ctgui_launch_app <- function(model = NULL, data = NULL, fit = NULL,
    launch.browser = interactive(), ...) {
  state <- ctgui_launch_state(
    model = model, data = data, fit = fit,
    data_label = ctgui_argument_label(substitute(data))
  )
  shiny::runApp(ctgui_create_app(initial_state = state),
    launch.browser = launch.browser, ...)
}

# A data frame passed by name is more recognisable in the app labelled by that
# name than by an anonymous placeholder.
ctgui_argument_label <- function(expression) {
  if (!is.symbol(expression)) return(NULL)
  as.character(expression)
}
