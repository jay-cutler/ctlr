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
#' @examples
#' df <- tibble::tibble(id = 1:3,
#' a = c("a", NA_character_, NA_character_),
#' b = c(NA_character_, "b", "b"),
#' c = c("c", NA_character_, "c"))
#'
#' pct_over_100(df, a:c)
#'
#' @export
pct_over_100 <- function(.data, .cols, id = "id") {

  n_respondents <- .data |>
    dplyr::select(id, {{ .cols }}) |>
    tidyr::pivot_longer(
      -id
    ) |>
    tidyr::drop_na(value) |>
    dplyr::select(-name) |>
    dplyr::summarize(respondents = dplyr::n_distinct(id)) |>
    dplyr::pull()

  .data |>
    dplyr::select(id, {{ .cols }}) |>
    tidyr::pivot_longer(
      -id
    ) |>
    tidyr::drop_na(value) |>
    dplyr::count(value) |>
    dplyr::mutate(respondents = n_respondents) |>
    dplyr::mutate(percent = n / respondents) |>
    dplyr::mutate(percent = scales::percent(percent, accuracy = 1)) |>
    dplyr::select(-respondents)
}
