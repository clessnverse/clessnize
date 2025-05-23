% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_pixel_font.R
\name{apply_pixel_font}
\alias{apply_pixel_font}
\title{Apply Pixel Operator font to all text elements in a patchwork or plot while preserving sizes}
\usage{
apply_pixel_font(plot, scale_factor = 1)
}
\arguments{
\item{plot}{A ggplot or patchwork object}

\item{scale_factor}{Optional factor to scale all text sizes (default: 1)}
}
\value{
A plot with Pixel Operator font applied to all text elements
}
\description{
This function ensures that all text elements in a ggplot or patchwork object
use the Pixel Operator font family while preserving existing size and style properties.
}
