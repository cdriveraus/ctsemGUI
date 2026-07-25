# Deferred ctsem co-refactor opportunities

This pass keeps `ctsem` 3.11.1 unchanged. `ctsemgui` therefore owns a small
compatibility adapter and the following deliberate compromises.

| Current ctsemgui compromise | Later ctsem opportunity | Compatibility requirement |
| --- | --- | --- |
| GUI parameter metadata is retained separately and serialized into the five-field matrix-cell annotation only when calling `ctModel()`. | Provide an annotation-preserving matrix/model accessor in ctsem instead of relying on `ctModelMatrices()`, which intentionally drops those annotations. | Preserve transform, individual-variation, scale, TI-effect, and extra-PARS information exactly. |
| `T0VAR` is cached and restored when random `T0MEANS` cause ctsem to suppress the corresponding initial-variance cells. | Expose and document the effective-initial-state rule, or provide a helper that reports active versus suppressed `T0VAR` cells. | Existing random-initial-mean models must retain their fitted semantics and editable dormant values. |
| The GUI maintains a fixed mapping of uncertainty methods, draw modes, and controls. | Export declarative `ctOptimUncertainty()` capability/default metadata. | Avoid scraping arbitrary function formals; labels and accepted combinations need a stable contract. |
| Fit comparison and eligibility code reads version-sensitive fit-object fields through ctsemgui adapter functions. | Provide stable fit-statistic and fit-capability accessors for log likelihood, posterior objective, parameter count, observation count, optimization status, and available uncertainty. | Older/current fit objects should produce explicit unavailable values rather than slot-access errors. |
| Feature availability is checked against the ctsem namespace at runtime. | Publish a small supported-feature/version contract for GUI clients. | Missing optional diagnostics should disable only the affected feature and explain the required ctsem version. |

Any coordinated ctsem change should first add contract tests in ctsem, then
update the ctsemgui adapter and remove only the compatibility code made
unnecessary by that tested API.
