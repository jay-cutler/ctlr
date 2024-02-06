#' Display a heading "bar" as a formatted table for Word documents
#'
#' Generates a heading as a table for Word documents using `flextable`. The colors and fonts utilized are customizable and any additional modifications can be added as additional `flextable` functions to override any defaults set by this function.
#'
#' @param header_text The header name, as a string, that you would like displayed in the table.
#' @param bg_color The background color of the table. Defaults to "#13334c" (dark navy).
#' @param header_font The font of the header. Defaults to "Montserrat".
#' @param text_color The color of the header text. Defaults to white.
#'
#' @return A flextable object.
#'
#' @importFrom rlang :=
#'
#' @export
pretty_header <- function(header_text, bg_color = "#13334c", header_font = "Montserrat", text_color = "white"){

  title <- "\n"

  tibble::tibble(title) |>
    dplyr::rename(!!header_text := title) |>
    dplyr::slice(-1) |>
    flextable::flextable() |>
    flextable::theme_zebra() |>
    flextable::fontsize(part = "header", size = 18) |>
    flextable::bold(part = "header", bold = TRUE) |>
    flextable::align(part = "all", align = "center") |>
    flextable::align(part = "body", j = 1, align = "left") |>
    flextable::align(part = "header", j = 1, align = "left") |>
    flextable::bg(part = "header", bg = bg_color) |>
    flextable::color(part = "header", color = text_color) |>
    flextable::height_all(part = "all", height = 10) |>
    flextable::font(part = "header", fontname = header_font) |>
    flextable::width(width = 7.5) |>
    flextable::padding(padding.top = 6, padding.bottom = 6, padding.left = 10, padding.right = 10, part = "all")
}
