% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scale-statgarten.R
\name{scale_color_statgarten_d}
\alias{scale_color_statgarten_d}
\alias{scale_fill_statgarten_d}
\alias{scale_color_statgarten_c}
\alias{scale_fill_statgarten_c}
\title{Statgarten colour palettes}
\usage{
scale_color_statgarten_d(palette = "statgarten")

scale_fill_statgarten_d(palette = "statgarten")

scale_color_statgarten_c(palette = "statgarten")

scale_fill_statgarten_c(palette = "statgarten")
}
\description{
The statgarten colour palettes are designed to be used with the
ggplot2 package. They are based on the colours used in the statgarten.
}
\examples{
# Use statgarten_d with discrete data
library(tidyverse)
iris \%>\%
  ggplot(aes(Sepal.Length, Sepal.Width, colour = Species)) +
    geom_point() +
    scale_color_statgarten_d("seoul") +
    sgthemes::theme_statgarten()

# Use statgarten_c with continuous data
iris \%>\%
  ggplot(aes(Sepal.Length, Sepal.Width, colour = Sepal.Length)) +
    geom_point() +
    scale_color_statgarten_c() +
    sgthemes::theme_statgarten()
}
