# The application is the only public API. Tests exercise implementation helpers
# through the package namespace rather than relying on exported bindings.
ctgui_internal_names <- c(
  "ctgui_build_matrices", "ctgui_build_measurement_matrices", "ctgui_build_model",
  "ctgui_export_code", "ctgui_generate_data", "ctgui_graph_edges", "ctgui_latex",
  "ctgui_matrix", "ctgui_matrix_names", "ctgui_set_matrix", "ctgui_set_matrix_value",
  "ctgui_set_parameter_metadata", "ctgui_spec", "ctgui_spec_from_model",
  "ctgui_structures", "ctgui_measurements", "ctgui_to_ctsem_model", "ctgui_validate",
  "ctgui_validate_data", "ctgui_visual_apply_graph", "ctgui_visual_graph"
)
for (ctgui_internal_name in ctgui_internal_names) {
  assign(ctgui_internal_name, getFromNamespace(ctgui_internal_name, "ctsemgui"), envir = environment())
}
