# frictionless 0.10.0

- `add_resource()` now supports adding CSV file(s) directly as a resource.
  This skips reading/handling by R and gives users control over `path` (#74).
- `write_package()` now downloads CSV files(s) from a remotely read package
  (like `example_package`) rather than skipping them.
  This is more consistent with locally read packages.
  The behaviour for resources with a `path` containing URLs (only) and resources 
  with `data` remains the same (no files are written).
  The write behaviour is better explained in the documentation (#77).
- `write_package()` now silently returns the output rather than input `package`.
- `add_resource()`/`create_schema()`'s `df` parameter is renamed to `data`.
- `create_schema()` interprets empty columns as `string` not `boolean` (#79).
- `example_package`'s `observations` resource now has URLs as `path` to serve 
  as an example for that.

# frictionless 0.9.0

- Add vignette with overview of functionality (#60).
- Prepare pkg for rOpenSci submission.
