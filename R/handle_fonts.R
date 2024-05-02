#' Register family of fonts with systemfonts()
#'
#' Loads all font styles within a font family, already installed on your system, into R to be used within {ggplot2}.
#'
#' @param font_name Name of the font as it would appear in a word processing software (e.g., "Helvetica Neue", "Open Sans").
#'
#' @export
handle_fonts <- function(font_name) {
  fonts_and_paths <- systemfonts::system_fonts() |>
    dplyr::filter(stringr::str_detect(family, font_name)) |>
    dplyr::select(name, family, style, path)


  fonts_and_paths |>
    purrr::pwalk(~ {
      l <- list(...)
      systemfonts::register_font(
        name = l$name,
        plain = l$path
      )
    })
}
