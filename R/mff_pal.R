#' Set MFF palette
#'
#' Produces a table with your variable of interest, n, and percentages where multiple columns are pivoted longer and there may be more than one option (column) per row/respondent ID.
#'
#' @param palette Selected palette. All palettes are 9 colors except for "Brand" (3) and "DivergingEven" (8). Options include: Brand, Blue, Teal, Orange, Green, Gradient, Diverging, and DivergingEven.
#' @param reverse Do you want the palette to display in reverse order (T/F)? Defaults to FALSE.
#'
#' @return A list of colors' HEX codes in a specified palette.
#'
#' @export
mff_pal <- function(palette = "Blue", reverse = FALSE) {

  mff_palettes <- list(
    "Brand" = c("#22a6b3", "#acd049", "#ff8c00"),
    "Blue" = c('#001d34', '#14344d', '#2e4b66', '#486480', '#627d9b', '#7d98b7', '#99b4d3', '#b5d1f1', '#d4efff'),
    "Teal" = c('#003340', '#004956', '#00606d', '#007885', '#06919e', '#28aab7', '#4bc2cf', '#68dbe9', '#84f5ff'),
    "Orange" = c('#4b0000', '#6b0a00', '#842600', '#9e3d00', '#b95300', '#d56a00', '#f18100', '#ffa122', '#ffc54e'),
    "Green" = c('#081c00', '#1a3200', '#264c00', '#3d6500', '#587f00', '#749904', '#90b52d', '#add14a', '#caee66'),
    "Gradient" = c('#13334c', '#1a455c', '#24586a', '#326b76', '#447f7e', '#599381', '#72a77e', '#8dbc70', '#acd049'),
    "Diverging" = c('#005f6c', '#038895', '#35b1be', '#6ddbe7', '#eeeeee', '#fcbf67', '#e68b1e', '#c25b01', '#963200'),
    "DivergingEven" = c('#005f6c', '#038895', '#35b1be', '#6ddbe7', '#fcbf67', '#e68b1e', '#c25b01', '#963200'))

  pal <- mff_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  pal
}
