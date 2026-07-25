# ctsemgui UI consolidation: approval review

Status: **proposal only — no UI source changes are authorized by this file.**

## Baseline

The current application has five top-level areas: **Model**, **Data**, **Fit**,
**Diagnostics**, and **Output**.  The duplicated outputs are genuine duplicates:
`validation_table_spec`/`validation_table`, all three data preview outputs,
`fit_log_inline`/`fit_log`, `fit_warnings_inline`/`fit_warnings`, and the two
summary/summary-matrix pairs.  This review preserves each retained output ID's
semantic result; the implementation may rename IDs only with a compatibility
test covering its consumer.

Reference screenshots captured from the integrated, behavior-preserving
baseline:

- [`ui-baseline-model.png`](ui-baseline-model.png): Model > Specification,
  including the separate Data mapping panel and duplicate Model destinations.
- [`ui-baseline-data.png`](ui-baseline-data.png): Data > Import and its embedded
  preview destination.
- [`ui-baseline-fit.png`](ui-baseline-fit.png): Fit > Settings with inline
  messages/warnings and their duplicate tabs.

These images are approval evidence only. They do not authorize source changes.

## Approval table

| Current location and controls/outputs | Proposed location | Decision | Preserved behaviour and risk |
| --- | --- | --- | --- |
| Model > Specification > **Data mapping**: `data_manifest_names`, `data_tdpred_names`, `data_tipred_names`, `data_id`, `data_time`; initial manual fields `manifest_names`, `tdpred_names`, `tipred_names`, `id`, `time` are split between Variables and Options | One **Data roles** band in Model > Specification, immediately after active data is available. It contains creatable selectize controls prepopulated from active-data columns and accepts manually typed manifest/TD/TI/ID/time names when no data exists or a name is not yet present. | Merge, then remove separate mapping panel. | Preserve current values, column-derived choices, manual model specification, and role-to-spec updates. Risk: ordinary `selectizeInput()` rejects novel values unless configured as creatable; this must be explicitly tested before removal. |
| Data > Import preview (`data_preview_import`), Data > Generate preview (`data_preview_generate`), Data > Preview (`data_preview`) | One Data > **Preview** panel using a single `data_preview` output, updated after import/generation. Import and Generate retain a concise status/link to Preview rather than an embedded table. | Merge, then remove two duplicates. | Preserve the first 20 active-data rows and the automatic post-generation transition to Preview. Risk: importing must also select/open Preview or make the route obvious; no duplicate reactive rendering should remain. |
| Model > Specification inline `validation_table_spec`; Model > Validation `validation_table` | Keep validation at the bottom of Model > Specification; remove Model > Validation. | Keep inline; remove duplicate tab. | Preserve the full `ctgui_validate(active_spec())` table. Risk: validation must re-render after every specification/matrix/visual mutation, not only text-field changes. |
| Model > Code (`code_output`), Model > Pars (`pars_table`); Diagnostics > Summary (`fit_summary_diagnostics`) and Summary matrices (`fit_summary_matrices_diagnostics`); Output > Fit Summary, Summary Matrices, Model Pars, Fit Comparison, Generated Code | Output becomes canonical: **Model code**, **Model PARS**, **Fit summary**, **Summary matrices**, **Fit comparison**. Remove the duplicate Model Code/PARS and Diagnostics Summary/Summary matrices tabs. | Merge, then remove duplicates. | Preserve generated code, editable/current model PARS display, active-fit selection, unavailable-fit messages, and output refresh after actions. Risk: users currently consult fit summaries under Diagnostics; Output needs clear labels/status and Diagnostics must retain only action-oriented analyses. |
| Fit > Settings inline `fit_log_inline`, `fit_warnings_inline`; Fit > Messages `fit_log`; Fit > Warnings `fit_warnings` | Keep the inline Message and Warning blocks under Fit Settings; remove Fit > Messages and Fit > Warnings. | Keep inline; remove duplicate tabs. | Preserve complete captured messages and warnings, including fit failures. Risk: multiline output must remain readable and accessible inline; do not collapse warnings to notifications only. |

## Retain pending dedicated review

None of the following is approved for removal or merger in this pass.  Each has
distinct input or output semantics that should survive the initial consolidation.

| Feature | Current location and purpose | Review outcome / guardrail |
| --- | --- | --- |
| TI predictor network | Model > Specification; `tipred_network_plot`, `tipred_network_status` show subject-level predictor correlations and moderated free-parameter targets. | Retain. Its subject-level reduction and moderation view are not duplicated by the matrix or visual editors. Reassess only after role-selection behaviour is live-tested. |
| Fit registry | Fit > Settings; `fit_save_name`, `save_fit`, `active_fit_name`, and `fit_comparison`. | Retain. It is the only candidate-model comparison workflow. Consolidate its explanation/status locally but do not remove. |
| Raw-data plots | Data > Visuals; `raw_plot_*`, `raw_plot`, PNG/PDF exports. | Retain. These are pre-fit data-quality views, not diagnostics. |
| R session and RDS controls | Fit > Settings; model/fit assignment, model/project/fit save/load controls. | Retain. Project RDS preserves GUI-specific state and is not equivalent to raw model RDS. Keep load error messaging and stale-fit invalidation. |
| Uncertainty | Fit > Uncertainty; optimized-fit controls, eligibility, confirmation for bootstrap, status/messages/warnings. | Retain. It has distinct cost and fit-eligibility behaviour. Keep it separate from routine fit settings. |
| Diagnostics | Diagnostics > Generate From Fit, Covariance Check, Prediction plots, Post Predictive, Residual ACF, Dynamics, TI moderation. | Retain all action-oriented diagnostics. Only duplicate summaries relocate to Output; preserve precondition guards, action code capture, plot exports, and per-analysis logs. |

## Acceptance checks before implementation

- With no active data, each Data roles control permits manual entry and produces
  the same specification as today's text inputs. With data, existing selected
  columns remain selected and new column names are offered.
- Import and generation each update the sole preview, while no obsolete preview
  output IDs are rendered or subscribed.
- Validation, code, PARS, fit summaries, fit messages, and fit warnings each
  have exactly one rendered UI destination and retain their current contents.
- Visual, matrix, builder, project-load, and fit workflows still invalidate or
  refresh the canonical destinations correctly.
- Live-browser approval evidence must include: initial empty-data state,
  imported/generative data state, a validation error, a fit failure or warning,
  and Output after an active fit. Capture these before and after shots as part
  of the implementation work.
