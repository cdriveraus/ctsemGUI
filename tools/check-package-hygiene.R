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

asset_dir <- file.path("inst", "www", "visual-spec")
expected_assets <- c("cytoscape.min.js", "visual-spec.css", "visual-spec.js")
assets <- list.files(asset_dir, pattern = "\\.(css|js)$", full.names = FALSE)
if (!setequal(assets, expected_assets)) {
  stop(
    "Unexpected visual-spec assets: expected ", paste(expected_assets, collapse = ", "),
    "; found ", paste(assets, collapse = ", "),
    call. = FALSE
  )
}
if (any(grepl("edgehandles", list.files(asset_dir, full.names = FALSE), fixed = TRUE))) {
  stop("Removed cytoscape-edgehandles assets are still present", call. = FALSE)
}

app_files <- list.files("R", pattern = "^app.*\\.R$", full.names = TRUE)
app_source <- paste(unlist(lapply(app_files, readLines, warn = FALSE)), collapse = "\n")
unreferenced <- expected_assets[!vapply(expected_assets, grepl, logical(1L),
  x = app_source, fixed = TRUE)]
if (length(unreferenced)) {
  stop("Unreferenced visual-spec assets: ", paste(unreferenced, collapse = ", "), call. = FALSE)
}

message("Package hygiene checks passed.")
