#' Generate comparison table of data from multiple time points
#'
#' Produces a table with response options (left) and time points (top) in n (%) format.
#'
#' @param .data The data frame.
#' @param comparison_variable Variable to compare response distribution across time points.
#' @param new_name Final name to display for the left (variable) column in the table. If not entered, will default to a sentence case version of the `comparison_variable`.
#' @param time_point Name of time point variable in dataset. Defaults to "fy" (without quotes).
#' @param cap_times Do you want to force the time point columns to be renamed as all caps (T/F)? Defaults to TRUE.
#' @param data_format Format of data presented in table. Can be one of "n" (only display n), "pct" (only display percent), or "n_pct" (display both n and percent; default).
#'
#' @return A data frame.
#'
#' @importFrom rlang :=
#'
#' @export
comparison_table <- function(.data, comparison_variable, new_name = NULL,
                             time_point = fy, cap_times = TRUE, data_format = "n_pct") {

  if (!data_format %in% c("n", "pct", "n_pct")) {
    stop("'data_format' argument must be one of: 'n','pct', 'n_pct'")
  }

  if(is.null(new_name)){
    var_name <- stringr::str_to_sentence(deparse(substitute(comparison_variable)))
  } else {
    var_name <- new_name
  }

  time_point_string <- deparse(substitute({{ time_point }}))

  if(data_format == "n_pct"){
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
      tidyr::pivot_wider(names_from = time_point_string,
                         values_from = "n_pct") |>
      dplyr::arrange({{ comparison_variable }}) |>
      dplyr::bind_rows(totals)
  }

  if(data_format == "pct"){

    table <- .data |>
      tidyr::drop_na({{ comparison_variable }}) |>
      janitor::tabyl({{ comparison_variable }}, {{ time_point }}) |>
      janitor::adorn_percentages(denominator = "col") |>
      janitor::adorn_pct_formatting(rounding = "half to even",
                           digits = 0) |>
      dplyr::arrange({{ comparison_variable }})

  }

  if(data_format == "n"){
    table <- .data |>
      tidyr::drop_na({{ comparison_variable }}) |>
      dplyr::group_by({{ time_point }}) |>
      dplyr::count({{ comparison_variable }}) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(names_from = time_point_string,
                         values_from = "n") |>
      dplyr::arrange({{ comparison_variable }}) |>
      janitor::adorn_totals(where = "row")
  }

  n_rows <- nrow(table)
  n_vars <- ncol(table)

  if(cap_times == TRUE){
    caps_yn <- table |>
      dplyr::rename(!!var_name := {{ comparison_variable }}) |>
      dplyr::rename_with(~stringr::str_to_upper(.x), -{{ comparison_variable }})
  }
  if(cap_times == FALSE){
    caps_yn <- table |>
      dplyr::rename(!!var_name := {{ comparison_variable }})
  }

  if(data_format != "pct"){
    caps_yn |>
      ctlr::mff_table() |>
      flextable::width(j = 1,
                       width = 2.5) |>
      flextable::width(j = 2:n_vars,
                       width = 5/(n_vars-1)) |>
      flextable::bold(i = n_rows)
  } else {
    caps_yn |>
      ctlr::mff_table() |>
      flextable::width(j = 1,
                       width = 2.5) |>
      flextable::width(j = 2:n_vars,
                       width = 5/(n_vars-1))
  }
}
