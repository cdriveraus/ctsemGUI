# Maintainability refactor acceptance matrix

This matrix maps the plan's behavioral contracts to automated coverage. It is
an implementation handoff, not a substitute for the final browser smoke test.

| Contract | Automated evidence |
|---|---|
| One canonical mutation result | `test-acceptance-characterization.R`: matrix/metadata and visual equivalence; guided/complete builder equivalence |
| Commit effects and saved projects | Unchanged serialized project commit test; existing state-controller effect tests |
| Variable lifecycle | Add, rename, delete characterization plus existing projection tests |
| Parameter semantics | Free/fixed cells, transform expressions, random effects, scales, extra PARS, TI all/none policies |
| Dormant `T0VAR` | GUI state retention through ctsem model construction; existing initial-view suppression test |
| ctsem compatibility | Minimum version, required exports/model fields, and GUI-to-ctsem-to-GUI matrix round trip |
| Visual protocol | All three projections, hostile text labels, per-view layout persistence, and existing JavaScript source contract |
| Plot return shapes | Empty, single, named, unnamed, ggplot, recorded base plot, and plotting function |
| Shiny workflow seams | Existing `testServer` shell, fit/diagnostic guards, and fit/uncertainty service tests |
| Browser workflows | Manual final gate: import, roles, matrix/visual edits, guards, downloads, console, reload |
