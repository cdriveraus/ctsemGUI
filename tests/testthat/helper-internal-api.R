# The application is the only public API. Tests exercise implementation helpers
# through the package namespace rather than relying on exported bindings.
ctgui_internal_names <- c(
  "ctgui_build_matrices", "ctgui_build_measurement_matrices", "ctgui_build_model",
  "ctgui_export_code", "ctgui_generate_data", "ctgui_graph_edges", "ctgui_latex",
  "ctgui_matrix", "ctgui_matrix_names", "ctgui_set_matrix", "ctgui_set_matrix_value",
  "ctgui_set_parameter_metadata", "ctgui_spec", "ctgui_spec_from_model",
  "ctgui_structures", "ctgui_measurements", "ctgui_to_ctsem_model", "ctgui_validate",
  "ctgui_validate_data", "ctgui_visual_apply_graph", "ctgui_visual_graph",
  "ctgui_visual_cell_active", "ctgui_visual_metadata",
  "ctgui_uncertainty_method_choices", "ctgui_uncertainty_draw_choices",
  "ctgui_uncertainty_control", "ctgui_uncertainty_optimcontrol",
  "ctgui_uncertainty_merge_optimcontrol", "ctgui_optim_uncertainty_eligibility",
  "ctgui_uncertainty_summary", "ctgui_run_result", "ctgui_ctsem_run",
  "ctgui_result_text", "ctgui_plot_collection", "ctgui_draw_plot",
  "ctgui_fit_comparison_stats",
  "ctgui_parse_names", "ctgui_spec_fields", "ctgui_spec_fields_changed",
  "ctgui_commit_spec_fields", "ctgui_apply_matrix_edits",
  "ctgui_parse_pars_vector", "ctgui_project_spec",
  "ctgui_data_role_selection", "ctgui_tipred_subject_data",
  "ctgui_data_preview", "ctgui_data_summary", "ctgui_missingness_summary",
  "ctgui_within_between_summary"
)
for (ctgui_internal_name in ctgui_internal_names) {
  assign(ctgui_internal_name, getFromNamespace(ctgui_internal_name, "ctsemgui"), envir = environment())
}
