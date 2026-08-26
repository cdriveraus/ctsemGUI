# Model history ----------------------------------------------------------------

# Every edit already passes through ctgui_commit_spec(), which knows the
# specification before and after and whether anything meaningful changed. That
# is exactly what a history needs, so keeping the committed specifications in a
# stack gives undo, redo, and a readable account of what was done to the model.
#
# The account is the part that matters. A list of timestamps says nothing; a
# user who has been clicking around the visual editor for ten minutes needs to
# see "Freed DRIFT[stress, sleep]" to know which step to go back to.

ctgui_history_limit <- 50L

ctgui_history_entry <- function(spec, reason, description) {
  list(spec = spec, reason = reason, description = description)
}

ctgui_history_new <- function(spec, description = "Starting point") {
  structure(
    list(entries = list(ctgui_history_entry(spec, "start", description)), position = 1L),
    class = "ctgui_history"
  )
}

ctgui_history_current <- function(history) {
  history$entries[[history$position]]$spec
}

# Node positions are saved with the specification, so laying out the graph
# counts as a change to it. It is not a change to the model, and recording one
# would put steps in the log that undo appears not to act on.
ctgui_history_is_model_change <- function(previous, updated) {
  if (identical(previous, updated)) return(FALSE)
  compare <- function(spec) {
    spec$visual <- NULL
    snapshot <- ctgui_spec_semantic_snapshot(spec)
    # A specification may carry an empty field as a named NULL or omit the name
    # altogether depending on how it was built. Both mean "nothing here", so
    # comparing them as different would report a model change that is only a
    # difference in representation.
    snapshot[!vapply(snapshot, is.null, logical(1L))]
  }
  !identical(compare(previous), compare(updated))
}

ctgui_history_can_undo <- function(history) history$position > 1L
ctgui_history_can_redo <- function(history) history$position < length(history$entries)

# Pushing after an undo discards the redo branch, which is what every editor
# does: once you change something from an earlier state, the abandoned future
# is no longer reachable from where you are.
ctgui_history_push <- function(history, spec, reason = "edit", description = NULL) {
  if (is.null(description)) {
    description <- ctgui_history_describe(ctgui_history_current(history), spec, reason)
  }
  entries <- history$entries[seq_len(history$position)]
  entries[[length(entries) + 1L]] <- ctgui_history_entry(spec, reason, description)
  if (length(entries) > ctgui_history_limit) {
    entries <- entries[seq(length(entries) - ctgui_history_limit + 1L, length(entries))]
  }
  history$entries <- entries
  history$position <- length(entries)
  history
}

ctgui_history_undo <- function(history) {
  if (ctgui_history_can_undo(history)) history$position <- history$position - 1L
  history
}

ctgui_history_redo <- function(history) {
  if (ctgui_history_can_redo(history)) history$position <- history$position + 1L
  history
}

ctgui_history_go <- function(history, position) {
  position <- suppressWarnings(as.integer(position))
  if (length(position) == 1L && !is.na(position) &&
      position >= 1L && position <= length(history$entries)) {
    history$position <- position
  }
  history
}

# What the user did, said in the terms they did it in.
ctgui_history_describe <- function(previous, updated, reason = "edit") {
  label <- ctgui_history_reason_label(reason)

  # Opening an example or applying a template rewrites the whole model at once.
  # Naming the action says more than an inventory of what it produced, so the
  # variables it created become a note on the action rather than the entry.
  if (reason %in% c("blueprint", "example")) {
    variables <- ctgui_history_describe_variables(previous, updated)
    if (!length(variables)) return(label)
    return(paste0(label, ": ", paste(variables, collapse = "; ")))
  }

  parts <- c(
    ctgui_history_describe_variables(previous, updated),
    ctgui_history_describe_options(previous, updated),
    ctgui_history_describe_cells(previous, updated),
    ctgui_history_describe_annotations(previous, updated)
  )
  if (!length(parts)) return(label)
  if (length(parts) > 3L) return(paste0(label, " (", length(parts), " changes)"))
  paste(parts, collapse = "; ")
}

ctgui_history_reason_label <- function(reason) {
  switch(reason %||% "edit",
    blueprint = "Built from a template",
    example = "Opened a worked example",
    start = "Starting point",
    `matrix-cell` = "Edited a matrix cell",
    `add-manifest` = "Added a manifest variable",
    `add-tdpred` = "Added a time-dependent predictor",
    `add-tipred` = "Added a time-independent predictor",
    visual = "Edited in the visual editor",
    specification = "Changed the specification",
    data_roles = "Changed the data roles",
    "Changed the model"
  )
}

ctgui_history_describe_variables <- function(previous, updated) {
  kinds <- list(
    latent_names = "latent process", manifest_names = "manifest variable",
    tdpred_names = "time-dependent predictor", tipred_names = "time-independent predictor"
  )
  unlist(lapply(names(kinds), function(field) {
    added <- setdiff(updated[[field]], previous[[field]])
    removed <- setdiff(previous[[field]], updated[[field]])
    c(
      if (length(added)) paste0("Added ", kinds[[field]], " ", paste(added, collapse = ", ")),
      if (length(removed)) paste0("Removed ", kinds[[field]], " ", paste(removed, collapse = ", "))
    )
  }), use.names = FALSE)
}

ctgui_history_describe_options <- function(previous, updated) {
  fields <- list(
    type = "Time model", id = "ID column", time = "Time column",
    tipredDefault = "Default TI moderation"
  )
  unlist(lapply(names(fields), function(field) {
    before <- previous[[field]]
    after <- updated[[field]]
    if (identical(before, after)) return(NULL)
    paste0(fields[[field]], " set to ", paste(format(after), collapse = ", "))
  }), use.names = FALSE)
}

ctgui_history_describe_cells <- function(previous, updated) {
  shared <- intersect(names(previous$matrices), names(updated$matrices))
  changes <- character()
  for (name in shared) {
    before <- previous$matrices[[name]]
    after <- updated$matrices[[name]]
    if (!is.matrix(before) || !is.matrix(after)) next
    # A resize is already reported as an added or removed variable; comparing
    # differently shaped matrices cell by cell would repeat it as noise.
    if (!identical(dim(before), dim(after)) || !identical(dimnames(before), dimnames(after))) next
    differing <- which(as.character(before) != as.character(after))
    for (index in differing) {
      row <- rownames(after)[((index - 1L) %% nrow(after)) + 1L]
      col <- colnames(after)[((index - 1L) %/% nrow(after)) + 1L]
      cell <- if (ncol(after) == 1L) {
        paste0(name, "[", row, "]")
      } else {
        paste0(name, "[", row, ", ", col, "]")
      }
      changes <- c(changes, paste0(
        ctgui_history_cell_verb(after[[index]]), " ", cell, " to ", after[[index]]
      ))
      if (length(changes) > 3L) return(changes)
    }
  }
  changes
}

# Whether a parameter varies between subjects, how it is transformed and which
# predictors moderate it are model decisions as much as freeing a cell is, so
# they belong in the log rather than being lumped under "changed the model".
ctgui_history_describe_annotations <- function(previous, updated) {
  before <- previous$parameter_metadata
  after <- updated$parameter_metadata
  if (is.null(after) || !nrow(after)) return(character())
  if (is.null(before)) before <- after[0L, , drop = FALSE]

  key <- function(frame) paste(frame$matrix, frame$row, frame$col, sep = "\r")
  before_key <- key(before)
  shared <- intersect(names(before), names(after))
  tracked <- setdiff(shared, c("matrix", "row", "col", "param"))

  changes <- character()
  for (index in seq_len(nrow(after))) {
    match_index <- match(key(after[index, , drop = FALSE]), before_key)
    if (is.na(match_index)) next
    differing <- tracked[vapply(tracked, function(field) {
      !identical(after[[field]][index], before[[field]][match_index])
    }, logical(1L))]
    if (!length(differing)) next
    cell <- paste0(after$matrix[index], "[", after$row[index], ", ", after$col[index], "]")
    changes <- c(changes, paste0(
      "Changed ", paste(ctgui_history_annotation_label(differing), collapse = ", "), " on ", cell
    ))
    if (length(changes) > 3L) return(changes)
  }
  changes
}

ctgui_history_annotation_label <- function(fields) {
  vapply(fields, function(field) {
    switch(field,
      indvarying = "RandomEffects",
      sdscale = "RandomEffectsScale",
      transform = "the transform",
      if (grepl("_effect$", field)) paste0(sub("_effect$", "", field), " moderation") else field
    )
  }, character(1L), USE.NAMES = FALSE)
}

ctgui_history_cell_verb <- function(value) {
  if (ctgui_cell_active(value)) "Freed" else "Fixed"
}

# The log reads newest first, because the entry a user wants is almost always
# the one they just made.
ctgui_history_log <- function(history) {
  positions <- rev(seq_along(history$entries))
  data.frame(
    step = positions,
    state = vapply(positions, function(index) {
      if (index == history$position) "current" else if (index < history$position) "" else "undone"
    }, character(1L)),
    change = vapply(positions, function(index) history$entries[[index]]$description, character(1L)),
    stringsAsFactors = FALSE
  )
}
