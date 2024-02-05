#' Calculate n and percent for checklist-style question data
#'
#' Produces a table with your variable of interest, n, and percentages where multiple columns are pivoted longer and there may be more than one option (column) per row/respondent ID.
#'
#' @param .data The data frame.
#' @param .cols Columns to select with data.
#' @param id Name of identifier column. Defaults to "id".
#'
#' @return A data frame.
#'
#'
#' @export
pct_over_100 <- function(.data, .cols, id = "id") {

  .data |>
    dplyr::select(id, {{ .cols }}) |>
    tidyr::pivot_longer(
      -id
    ) |>
    tidyr::drop_na(value) |>
    dplyr::select(-name) |>
    dplyr::mutate(respondents = dplyr::n_distinct(id)) |>
    dplyr::count(value, respondents) |>
    dplyr::mutate(pct = n / respondents) |>
    dplyr::mutate(pct_formatted = scales::percent(pct, accuracy = 1))
}
