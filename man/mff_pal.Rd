% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mff_pal.R
\name{mff_pal}
\alias{mff_pal}
\title{Set MFF palette}
\usage{
mff_pal(palette = "Blue", reverse = FALSE)
}
\arguments{
\item{palette}{Selected palette. All palettes are 9 colors except for "Brand" (3) and "DivergingEven" (8). Options include: Brand, Blue, Teal, Orange, Green, Gradient, Diverging, and DivergingEven.}

\item{reverse}{Do you want the palette to display in reverse order (T/F)? Defaults to FALSE.}
}
\value{
A list of colors' HEX codes in a specified palette.
}
\description{
Produces a table with your variable of interest, n, and percentages where multiple columns are pivoted longer and there may be more than one option (column) per row/respondent ID.
}
