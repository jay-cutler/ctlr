#' MFF Table
#'
#' Display data frame in MFF flextable format.
#'
#' @param .data The data frame.
#'
#' @return A flextable object.
#'
#' @examples
#' examples go here.
#'
#' @importFrom officer fp_border
#'
#' @export
bottom_border <- officer::fp_border(color = "#13334c", style = "solid", width = 2)

mff_table <- function(.data) {

  number_of_variables <- .data |>
    length()

  .data |>
    flextable::flextable() |>
    flextable::theme_zebra(odd_body = "#E2E6E9") |>
    flextable::fontsize(part = "all", size = 10) |>
    flextable::bold(part = "header", bold = TRUE) |>
    flextable::align(part = "all", align = "center") |>
    flextable::align(part = "body", j = 1, align = "left") |>
    flextable::align(part = "header", j = 1, align = "left") |>
    flextable::bg(part = "header", bg = "#13334c") |>
    flextable::color(part = "header", color = "white") |>
    flextable::font(fontname = "Source Sans Pro", part = "all") |>
    flextable::height_all(part = "all", height = 10) |>
    flextable:: hline_bottom(part = "body", border = bottom_border) |>
    flextable:: width(j = 1,
          width = 4) |>
    flextable:: width(j = 2:number_of_variables,
          width = 3.5 / (number_of_variables - 1)) |>
    flextable::keep_with_next(part = "all")|>
    flextable::set_table_properties(align = "left")
}
