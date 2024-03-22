# frictionless (development version)

* A Data Package object (`package`) now has a `datapackage` class (#184). As a result:
  * New function `print()` prints a a human-readable summary of the Data Package (##155).
  * New function `as.list()` converts a Data Package back to a list.
  * `check_package()` will warn if the class is missing.
* `read_resource()` now supports column selection via the `col_select` argument from `readr::read_delim()`. This can vastly improve reading speed (#123).
* `add_resource()` now supports adding additional resource properties via the `...` argument.
* `create_package()` now accepts a `descriptor` argument so that a Data Package object can be created from an existing object (#184). It will always validate the created object with `create_package()`.
* `check_package()` is now a public function, so it can be used by other packages (#185).
* `readr::problems()` is included in NAMESPACE so users don't have to load readr to inspect parsing issues. The function is mentioned in the documentation of `read_resource()` (#129).
* `cli::cli_abort()`, `cli::cli_warn()` and `cli::cli_inform()` are used for all errors, warnings, and messages (#163). This has several advantages:
  * Messages use semantic colours for variables, parameters, fields, etc.
  * Messages and warnings can be silenced with a global or local option, see [this blog post](https://ropensci.org/blog/2024/02/06/verbosity-control-packages/).
  * Each call has an [rlang](https://cran.r-project.org/package=rlang) class, e.g. `frictionless_error_fields_without_name`, making it easier to test for specific errors.
* The dependencies [glue](https://cran.r-project.org/package=glue) and [assertthat](https://cran.r-project.org/package=assertthat) are removed (#163). The functionality of glue is replaced by cli, `assertthat::assert()` calls are now `if()` functions.
* Adhere to the requirements of [checklist](https://github.com/inbo/checklist), so that `.zenodo.json` can be created with `checklist::update_citation()`.
* Add [Pieter Huybrechts](https://orcid.org/0000-0002-6658-6062) as author. Welcome Pieter!

# frictionless 1.0.3

* Add [stringi](https://cran.r-project.org/package=stringi) to `Suggests`. It was removed as a dependency from [rmarkdown](https://cran.r-project.org/package=rmarkdown) 2.26, resulting in "stringi package required for encoding operations" build errors on CRAN (#176).

# frictionless 1.0.2

* Add `skip_if_offline()` to selected tests and verbosely include output in vignette examples, to avoid CRAN errors caused by timeouts (#116).

# frictionless 1.0.1

* Rebuild documentation for compatibility with HTML5 on request of CRAN.
* Add funder information.

# frictionless 1.0.0

* First release on [CRAN](https://cran.r-project.org/package=frictionless). 🎉
* Files (`datapackage.json`, resource files, schemas) can now be read from `(s)ftp://` URLs (#102).
* Package website is now served from <https://docs.ropensci.org/frictionless/>.

# frictionless 0.11.0

* `add_resource()` now sets `format`, `mediatype` and `encoding` for added CSV file(s) (#78).
* `add_resource()` now supports adding `schema` via path or URL.
* `write_package()` now supports added data to be gzip compressed before being written to disk (#98).
* `read_resource()` will now warn rather than error on unknown encoding (#86).
* `package` objects no longer have or require the custom attribute `resource_names`, use new function `resources()` instead (#97).
* `package` objects no longer have or require the custom attribute `datapackage`, making it easier to edit them as lists (with e.g. `append()`).

# frictionless 0.10.0

* `add_resource()` now supports adding CSV file(s) directly as a resource. This skips reading/handling by R and gives users control over `path` (#74).
* CSV files in a remotely read package (like `example_package`) are now downloaded when writing with `write_package()`, rather than being skipped. This is more consistent with locally read packages. The behaviour for resources with a `path` containing URLs (only) and resources with `data` remains the same (no files are written). The write behaviour is better explained in the documentation (#77).
* `write_package()` now silently returns the output rather than input `package`.
* `create_package()` will set `"profile" = "tabular-data-package"` since packages created by frictionless meet those requirements (#81).
* `create_schema()` interprets empty columns as `string` not `boolean` (#79).
* `read_package()` can now read from a `datapackage.yaml` file.
* `read_resource()` now accepts YAML Table Schemas and CSV dialects.
* `add_resource()`/`create_schema()`'s `df` argument is renamed to `data`.
* `example_package`'s `observations` resource now has URLs as `path` to serve as an example for that.

# frictionless 0.9.0

* Add vignette with overview of functionality (#60).
* Prepare frictionless for rOpenSci submission.
