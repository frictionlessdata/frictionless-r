#' Proposed extension of readr::read_delim
#'
#' Adds a `channels` argument that facilitates the loading of missing values
#'
#'
#' @inheritParams readr::read_delim
#' @param channels A character vector representing the data channels to load.
#'   Available channels are `"values"` and `"missing"`. Use named vectors to
#'   control column suffixes, e.g.
#'   `c(values = "_values", missing = "_missing")`.
#' @return A `tibble()` of loaded values.
#' @family helper functions
#' @noRd
read_delim_ext <- function(file, delim, na = c("", "NA"), col_types = NULL,
                           col_select = NULL, channels = "values", ...) {
  default_suffixes <- c(
    values = "_values",
    missing = "_missing"
  )

  if (is.null(names(channels))) {
    names(channels) <- rlang::rep_along(channels, "")
  }

  if (length(channels) == 1 && names(channels) == "") {
    channel_suffixes <- ""
  } else {
    channel_suffixes <- purrr::map2_vec(
      channels,
      names(channels),
      \(x, n) if (n == "") { default_suffixes[[x]] } else { x }
    )
  }

  channel_names <- purrr::map2_vec(
    channels,
    names(channels),
    \(x, n) dplyr::if_else(n == "", x, n)
  )

  channels <- purrr::set_names(channel_suffixes, channel_names)

  if (is.null(col_select)) {
    selected_col_types <- col_types
  } else {
     selected_col_types <- col_types[tidyselect::eval_select(
      rlang::enquo(col_select),
      col_types,
    )]
  }

  string_df <- readr::read_delim(
    file,
    delim,
    col_select = {{col_select}},
    col_types = readr::cols(.default=readr::col_character()),
    ...
  )

  result <- list()

  if ("values" %in% channel_names) {
    values_df <- string_df %>%
      readr::type_convert(
        col_types=selected_col_types,
        na=na,
      ) %>%
      dplyr::rename_with(\(x) paste0(x, channels[["values"]]))
    result <- append(result, values_df)
  }

  if ("missing" %in% channel_names) {
    missing_df <- string_df %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          \(x) dplyr::if_else(x %in% na, x, NA_character_)
        )
      ) %>%
      dplyr::rename_with(\(x) paste0(x, channels[["missing"]]))
    result <- append(result, missing_df)
  }
  dplyr::bind_cols(result)
}
