# frictionless 0.10.0

- `add_resource()` now supports adding CSV file(s) directly as a resource.
  This skips reading/handling by R and gives users control over `path` (#74).
- CSV files in a remotely read package (like `example_package`) are now
  downloaded when writing with `write_package()`, rather than being skipped.
  This is more consistent with locally read packages.
  The behaviour for resources with a `path` containing URLs (only) and resources 
  with `data` remains the same (no files are written).
  The write behaviour is better explained in the documentation (#77).
- `write_package()` now silently returns the output rather than input `package`.
- `create_package()` will set `"profile" = "tabular-data-package"` since 
  packages created by frictionless meet those requirements (#81).
- `create_schema()` interprets empty columns as `string` not `boolean` (#79).
- `add_resource()`/`create_schema()`'s `df` parameter is renamed to `data`.
- `example_package`'s `observations` resource now has URLs as `path` to serve 
  as an example for that.

# frictionless 0.9.0

- Add vignette with overview of functionality (#60).
- Prepare frictionless for rOpenSci submission.
