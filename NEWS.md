# frictionless 0.10.0

- `add_resource()` now allows adding CSV file(s) directly as a resource, without
  the need to read all data and giving users more control over `path` (#74).
- `write_package()` will now download CSV files(s) from a remotely read package
  (like `example_package`) rather than skipping them.
  The behaviour for resources with a `path` containing URLs (only) and resources 
  with `data` remains the same (no files are written).
  The write behaviour is better explained in the documentation (#77).
- `write_package()` now silently returns the output rather than input `package`.
- `example_package`'s `observations` resource now has URLs as `path` to serve 
  as an example for that.
- `add_resource()` and  `create_schema()`: `df` parameter is renamed to `data`.

# frictionless 0.9.0

- Add vignette with overview of functionality (#60).
- Prepare pkg for rOpenSci submission.
