description <- read.dcf("DESCRIPTION")
imports <- trimws(unlist(strsplit(description[1L, "Imports"], ",", fixed = TRUE)))
suggests <- trimws(unlist(strsplit(description[1L, "Suggests"], ",", fixed = TRUE)))
enhances <- if ("Enhances" %in% colnames(description)) description[1L, "Enhances"] else NA_character_

required_imports <- c("ctsem (>= 3.11.1)", "shiny")
missing_imports <- setdiff(required_imports, imports)
if (length(missing_imports)) {
  stop("Missing runtime Imports: ", paste(missing_imports, collapse = ", "), call. = FALSE)
}
if ("shiny" %in% suggests || !is.na(enhances) || "htmltools" %in% imports) {
  stop("DESCRIPTION must keep shiny and ctsem in Imports and omit htmltools/Enhances", call. = FALSE)
}

asset_contract <- list(
  app = c("app.css", "app.js"),
  `visual-spec` = c("cytoscape.min.js", "visual-spec.css", "visual-spec.js")
)
source_contract <- list(
  `visual-spec` = "cytoscape.umd.js"
)
for (asset_group in names(asset_contract)) {
  asset_dir <- file.path("inst", "www", asset_group)
  expected_assets <- c(asset_contract[[asset_group]], source_contract[[asset_group]])
  assets <- list.files(asset_dir, pattern = "\\.(css|js)$", full.names = FALSE)
  if (!setequal(assets, expected_assets)) {
    stop(
      "Unexpected ", asset_group, " assets: expected ", paste(expected_assets, collapse = ", "),
      "; found ", paste(assets, collapse = ", "),
      call. = FALSE
    )
  }
}
if (any(grepl(
  "edgehandles",
  list.files(file.path("inst", "www"), recursive = TRUE, full.names = FALSE),
  fixed = TRUE
))) {
  stop("Removed cytoscape-edgehandles assets are still present", call. = FALSE)
}

app_files <- list.files("R", pattern = "^app.*\\.R$", full.names = TRUE)
app_source <- paste(unlist(lapply(app_files, readLines, warn = FALSE)), collapse = "\n")
first_party_assets <- unlist(asset_contract, use.names = FALSE)
unreferenced <- first_party_assets[!vapply(first_party_assets, grepl, logical(1L),
  x = app_source, fixed = TRUE)]
if (length(unreferenced)) {
  stop("Unreferenced application assets: ", paste(unreferenced, collapse = ", "), call. = FALSE)
}
if (grepl("tags$style", app_source, fixed = TRUE) ||
    grepl("addCustomMessageHandler", app_source, fixed = TRUE)) {
  stop("Application CSS and general event glue must live in versioned assets", call. = FALSE)
}

message("Package hygiene checks passed.")
