% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pretty_section.R
\name{pretty_section}
\alias{pretty_section}
\title{Display a heading "bar" with a section for body text as a formatted table for Word documents}
\usage{
pretty_section(
  header_text,
  bg_color = "#13334c",
  bg_color_body = "#deecf7",
  header_font = "Montserrat",
  body_font = "Source Sans Pro",
  text_color = "white",
  text_color_body = "black"
)
}
\arguments{
\item{header_text}{The header name, as a string, that you would like displayed in the table.}

\item{bg_color}{The background color of the table header. Defaults to "#13334c" (dark navy).}

\item{bg_color_body}{The background color of the table body. Defaults to "#deecf7" (light pale blue).}

\item{header_font}{The font of the header. Defaults to "Montserrat".}

\item{body_font}{The font of the body. Defaults to "Source Sans Pro".}

\item{text_color}{The color of the header text. Defaults to white.}

\item{text_color_body}{The color of the body text. Defaults to black.}
}
\value{
A flextable object.
}
\description{
Generates a heading and text section as a table for Word documents using `flextable`. The colors and fonts utilized are customizable and any additional modifications can be added as additional `flextable` functions to override any defaults set by this function.
}
