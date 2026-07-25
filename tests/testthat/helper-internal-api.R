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
  "ctgui_ctsem_is_fit", "ctgui_ctsem_fit_model", "ctgui_ctsem_fit_generated",
  "ctgui_ctsem_fit_statistics", "ctgui_ctsem_fit_is_sampled",
  "ctgui_ctsem_fit_missing_components",
  "ctgui_output_data_source", "ctgui_output_data_code", "ctgui_output_base_code",
  "ctgui_output_fit_code", "ctgui_output_uncertainty_code",
  "ctgui_output_diagnostic_code", "ctgui_output_snippet",
  "ctgui_output_workflow_code",
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

ctgui_test_source_path <- function(...) {
  path <- testthat::test_path("..", "..", ...)
  if (!file.exists(path)) {
    testthat::skip("source-only architecture contract")
  }
  path
}

ctgui_test_asset_path <- function(...) {
  source_path <- testthat::test_path("..", "..", "inst", ...)
  if (file.exists(source_path)) return(source_path)
  system.file(..., package = "ctsemgui")
}
