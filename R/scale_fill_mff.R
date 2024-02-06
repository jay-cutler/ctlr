#' Use an MFF palette for a color argument.
#'
#' @param palette Selected palette. All palettes are 9 colors except for "Brand" (3) and "DivergingEven" (8). Options include: Brand, Blue, Teal, Orange, Green, Gradient, Diverging, and DivergingEven.
#' @param discrete Is this for a set of discrete or continuous values (T/F)? Defaults to TRUE.
#' @param reverse Do you want the palette to display in reverse order (T/F)? Defaults to FALSE.
#' @param contrast Level of contrast desired between colors in plot. Options are "high", "med", and "low". Defaults to "high".
#'
#' @export
scale_fill_mff <- function(palette = "Blue", discrete = TRUE, reverse = FALSE, contrast = "high") {
  pal <- ctlr::mff_pal(palette = palette, reverse = reverse)

  if(!contrast %in% c("high", "med", "low")) {
    stop("'contrast' argument must be one of: 'high', 'med', low'")
  }

  if(palette != "Brand"){

    if(contrast == "low") {
      pal <- pal[c(3:(length(pal)-2))]
    }

    if(contrast == "med") {
      pal <- pal[c(2:(length(pal)-1))]
    }

  }

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("mff_", palette), palette = grDevices::colorRampPalette(pal))
  } else {
    ggplot2::scale_fill_gradientn(colours = pal)
  }
}
