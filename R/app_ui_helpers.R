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

# Templates are chosen by what they mean, not by an identifier, so each option
# carries its own one-line summary and the longer explanation appears with the
# rest of the teaching text.
ctgui_blueprint_choice_ui <- function(selected = "coupled") {
  structures <- ctgui_blueprint_structures()
  shiny::radioButtons(
    "build_structure", NULL,
    choiceValues = names(structures),
    choiceNames = unname(lapply(structures, function(definition) {
      shiny::div(
        class = "blueprint-choice-body",
        shiny::tags$strong(definition$title),
        shiny::tags$p(class = "help-note", definition$summary),
        shiny::tags$p(class = "help-note ctgui-explain-detail", definition$detail)
      )
    })),
    selected = selected
  )
}

ctgui_build_ui <- function() {
  shiny::tagList(
    shiny::div(
      class = "control-band",
      ctgui_explanation_ui("build"),
      shiny::radioButtons(
        "build_mode", "What to do",
        choices = c(
          "Build a fresh model" = "replace",
          "Add processes to the current model" = "extend"
        ),
        selected = "replace"
      )
    ),
    shiny::div(
      class = "control-band",
      # In extend mode the template describes only the processes being added,
      # not the model as a whole. Reading "Model shape" there suggests the
      # existing model is about to be reshaped, which is the opposite of what
      # happens.
      shiny::conditionalPanel(
        "input.build_mode != 'extend'",
        shiny::tags$h4("Model shape"),
        shiny::tags$p(class = "help-note", "The shape of the model you are about to build.")
      ),
      shiny::conditionalPanel(
        "input.build_mode == 'extend'",
        shiny::tags$h4("Shape of the processes you are adding"),
        shiny::tags$p(
          class = "help-note",
          "This describes only the new processes. The model you already have keeps its own shape and none of its parameters change."
        )
      ),
      ctgui_blueprint_choice_ui()
    ),
    shiny::div(
      class = "control-band",
      shiny::tags$h4("Processes and indicators"),
      shiny::div(
        class = "control-grid",
        shiny::textInput("build_processes", "Process names", value = "process1, process2"),
        shiny::numericInput("build_indicators", "Indicators per process", value = 1, min = 1, step = 1),
        shiny::checkboxInput("build_noise_correlations", "Let system noise correlate across processes", value = TRUE),
        shiny::conditionalPanel(
          "input.build_mode == 'extend'",
          shiny::checkboxInput(
            "build_connect_existing",
            "Freely estimate effects between existing and new processes",
            value = FALSE
          )
        )
      ),
      shiny::tags$p(
        class = "help-note",
        "Latent processes take the names you give here; manifest variables are numbered from them. Rename anything afterwards in the visual editor."
      )
    ),
    shiny::div(
      class = "control-band",
      shiny::tags$h4("What this will do"),
      shiny::verbatimTextOutput("build_summary"),
      shiny::actionButton("build_apply", "Build model", class = "btn-primary")
    )
  )
}

# Examples are chosen by what they demonstrate, so each option shows its own
# description rather than a bare title.
ctgui_example_choice_ui <- function(selected = "coupled") {
  catalog <- ctgui_example_catalog()
  shiny::radioButtons(
    "example_id", NULL,
    choiceValues = names(catalog),
    choiceNames = unname(lapply(catalog, function(example) {
      shiny::div(
        class = "blueprint-choice-body",
        shiny::tags$strong(example$title),
        shiny::tags$p(class = "help-note", example$brief),
        shiny::tags$p(class = "help-note ctgui-explain-detail", example$detail)
      )
    })),
    selected = selected
  )
}

ctgui_examples_ui <- function() {
  shiny::tagList(
    shiny::div(
      class = "control-band",
      ctgui_explanation_ui("examples"),
      ctgui_example_choice_ui()
    ),
    shiny::div(
      class = "control-band",
      shiny::tags$h4("What to look at"),
      shiny::verbatimTextOutput("example_guidance"),
      shiny::actionButton("example_load", "Open this example", class = "btn-primary"),
      shiny::tags$p(
        class = "help-note",
        "Opening an example replaces the current data and model."
      )
    )
  )
}
