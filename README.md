# ctsemgui

`ctsemgui` is a first-pass companion package for building and editing `ctsem`
models through a GUI-neutral matrix specification.

The current package intentionally starts small:

- create a model specification in R;
- edit ctsem matrices programmatically;
- validate dimensions and names;
- convert a valid spec to `ctsem::ctModel()`;
- export reproducible R code;
- launch a minimal Shiny interface using the same functions.

```r
spec <- ctgui_spec(
  latent_names = c("eta1", "eta2"),
  manifest_names = c("Y1", "Y2")
)

spec <- ctgui_set_matrix_value(spec, "DRIFT", "eta1", "eta2", label = "cross12")
ctgui_validate(spec)
cat(ctgui_export_code(spec))
```
