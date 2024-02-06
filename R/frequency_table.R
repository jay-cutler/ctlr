#' Generate a basic table with n and percent.
#'
#' Produces a tabyl with n and percent for a given variable. Excludes NAs and by default will order data from highest to lowest percent
#'
#' @param .data The data frame.
#' @param frequency_variable Variable of interest.
#' @param ordering Choose to order data from greatest to lowest percent ("pct") or by factor order ("fct"). Defaults to "pct". (Note: if there are no factor levels set and "fct" is selected, data will arrange variable of choice in alphabetical order.)
#'
#' @return A tabyl.
#'
#' @export
frequency_table <- function(.data, frequency_variable, ordering) {

  if(!ordering %in% c("pct", "fct")) {
    stop("'ordering' argument must be one of: 'pct', 'fct")
  }

  if(ordering == "pct"){
  .data |>
    tidyr::drop_na({{ frequency_variable }}) |>
    janitor::tabyl({{ frequency_variable }}) |>
    dplyr::arrange(dplyr::desc(percent)) |>
    janitor::adorn_pct_formatting(digits = 0,
                         rounding = "half to even")
  }

  if(ordering == "fct"){
    .data |>
      tidyr::drop_na({{ frequency_variable }}) |>
      janitor::tabyl({{ frequency_variable }}) |>
      dplyr::arrange({{ frequency_variable }}) |>
      janitor::adorn_pct_formatting(digits = 0,
                                    rounding = "half to even")
  }

}
