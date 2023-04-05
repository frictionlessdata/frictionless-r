# read_resource() returns error on column selection not in schema

    Code
      cat(read_resource(p, "deployments", col_select = "no_such_column"))
    Error <assertError>
      Can't find column(s) `no_such_column` in field names.
      ℹ Field names: `deployment_id`, `longitude`, `latitude`, `start`, `comments`

---

    Code
      cat(read_resource(p, "deployments", col_select = c("no_such_column", "start",
        "no_such_column_either")))
    Error <assertError>
      Can't find column(s) `no_such_column`, `no_such_column_either` in field names.
      ℹ Field names: `deployment_id`, `longitude`, `latitude`, `start`, `comments`

