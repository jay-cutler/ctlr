#' Generate comparison table of data from multiple time points
#'
#' Produces a table with response options (left) and time points (top) in n (%) format.
#'
#' @param .data The data frame.
#' @param comparison_variable Variable to compare response distribution across time points.
#' @param new_name Final name to display for the left (variable) column in the table. If not entered, will default to a sentence case version of the `comparison_variable`.
#' @param time_point Name of time point variable in dataset. Defaults to "fy".
#' @param cap_times Do you want to force the time point columns to be renamed as all caps (T/F)? Defaults to TRUE.
#'
#' @return A data frame.
#'
#' @importFrom rlang :=
#'
#' @export
comparison_table <- function(.data, comparison_variable, new_name = NULL, time_point = "fy", cap_times = TRUE) {

  if(is.null(new_name)){
    var_name <- stringr::str_to_sentence(deparse(substitute(comparison_variable)))
  } else {
    var_name <- new_name
  }

  totals <- .data |>
    tidyr::drop_na({{ comparison_variable }}) |>
    janitor::tabyl({{ comparison_variable }}, {{ time_point }}) |>
    janitor::adorn_totals(where = "row") |>
    dplyr::filter({{ comparison_variable }} == "Total") |>
    dplyr::mutate(dplyr::across(-{{ comparison_variable }}, ~paste(.x, "(100%)", sep = " ")))

  table <- .data |>
    tidyr::drop_na({{ comparison_variable }}) |>
    dplyr::group_by({{ time_point }}) |>
    dplyr::count({{ comparison_variable }}) |>
    dplyr::mutate(pct = n/sum(n))  |>
    janitor::adorn_pct_formatting(digits = 0,
                         rounding = "half to even",
                         affix_sign = TRUE,
                         pct) |>
    dplyr::mutate(n_pct = paste(n, " (", pct, ")", sep = "")) |>
    dplyr::select(-c(n, pct)) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = deparse(substitute({{ time_point }})),
                values_from = "n_pct") |>
    dplyr::arrange({{ comparison_variable }}) |>
    dplyr::bind_rows(totals)

  last_row <- nrow(table)

  if(cap_times == TRUE){
    caps_yn <- table |>
      dplyr::rename(!!var_name := {{ comparison_variable }}) |>
      dplyr::rename_with(~stringr::str_to_upper(.x), -{{ comparison_variable }})
  }
  if(cap_times == FALSE){
    caps_yn <- table |>
      dplyr::rename(!!var_name := {{ comparison_variable }})
  }

  caps_yn |>
    ctlr::mff_table() |>
    flextable::width(j = 1,
          width = 2.5) |>
    flextable::width(j = 2:n_vars,
          width = 5/(n_vars-1)) |>
    flextable::bold(i = last_row)
}
