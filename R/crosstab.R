#' Generate a crosstab between an outcome and a demographic variable
#'
#' Produces a tabyl with demographic responses on the left and the various answer options to the outcome variable on the top. Also includes a column to the right with the n for each demographic response item.
#'
#' @param .data The data frame.
#' @param question Outcome variable.
#' @param demographic Demographic variable to stratify data from outcome variable.
#'
#' @return A tabyl.
#'
#' @importFrom rlang :=
#'
#' @export
crosstab <- function(.data, question, demographic) {

  n_rows <- .data |>
    tidyr::drop_na({{question}}) |>
    tidyr::complete({{question}}) |>
    dplyr::distinct({{question}}) |>
    nrow()

  .data |>
    tidyr::drop_na({{question}}) |>
    dplyr::arrange({{question}}) |>
    dplyr::count({{demographic}}, {{question}}) |>
    tidyr::complete({{question}}, {{demographic}}) |>
    dplyr::mutate(n = tidyr::replace_na(n, 0)) |>
    tidyr::pivot_wider(names_from = {{question}},
                       values_from = n) |>
    dplyr::mutate( dplyr::across(c(2:(n_rows+1)), ~tidyr::replace_na(.x, 0))) |>
    janitor::adorn_totals(where = "col") |>
    dplyr::mutate(dplyr::across(c(2:(n_rows+1)), ~scales::percent(.x/Total, accuracy = 1))) |>
    dplyr::mutate({{demographic}} := as.character({{demographic}})) |>
    dplyr::mutate({{demographic}} := tidyr::replace_na({{demographic}}, "Skipped this question")) |>
    dplyr::filter(Total != 0) |>
    dplyr::rename("n" = Total) |>
    dplyr::rename_with(~stringr::str_to_sentence, {{demographic}})
}
