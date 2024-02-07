#' Generate a basic table with n and percent.
#'
#' Produces a tabyl with n and percent for a given variable. Excludes NAs and by default will order data from highest to lowest percent
#'
#' @param .data The data frame.
#' @param frequency_variable Variable of interest.
#' @param ordering Choose to order data from greatest to lowest percent ("pct") or by factor order ("fct"). Defaults to "pct". (Note: if there are no factor levels set and "fct" is selected, data will arrange variable of choice in alphabetical order.)
#'
#' @return A data frame.
#'
#' @export
frequency_table <- function(.data, frequency_variable, ordering = "pct") {

  if(!ordering %in% c("pct", "fct")) {
    stop("'ordering' argument must be one of: 'pct', 'fct")
  }

  n_respondents <- .data |>
    tidyr::drop_na({{ frequency_variable }}) |>
    nrow()

  df <- .data |>
    dplyr::count({{ frequency_variable }}, .drop = FALSE) |>
    dplyr::mutate(respondents = n_respondents) |>
    dplyr::mutate(percent = n / respondents)

  if(ordering == "pct"){
  df |>
      dplyr::arrange(dplyr::desc(percent)) |>
      dplyr::mutate(percent = scales::percent(percent, accuracy = 1))
  }

  if(ordering == "fct"){
    df |>
      dplyr::arrange({{ frequency_variable }}) |>
      dplyr::mutate(percent = scales::percent(percent, accuracy = 1))
  }

}
