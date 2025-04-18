% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/histoires_datagochi_usa.R
\name{add_logo_to_plot}
\alias{add_logo_to_plot}
\title{Add a logo to a ggplot}
\usage{
add_logo_to_plot(
  plot,
  logo_path = "https://raw.githubusercontent.com/clessn/img/refs/heads/main/Logo.PNG",
  output_path = "plot_with_logo.png",
  logo_width = 0.1,
  logo_position = "topright",
  margin = 0.01
)
}
\arguments{
\item{plot}{A ggplot object to which the logo will be added.}

\item{logo_path}{A character string specifying the path or URL of the logo image.
Default is `"https://raw.githubusercontent.com/clessn/img/refs/heads/main/Logo.PNG"`.}

\item{output_path}{A character string specifying the file path for saving the output plot with the logo.
Default is `"plot_with_logo.png"`.}

\item{logo_width}{A numeric value specifying the width of the logo as a proportion of the plot width.
Default is `0.1`.}

\item{logo_position}{A character string specifying the position of the logo.
Accepts `"topright"`, `"topleft"`, `"bottomright"`, or `"bottomleft"`. Default is `"topright"`.}

\item{margin}{A numeric value specifying the margin between the logo and the plot edges as a proportion of the plot width.
Default is `0.01`.}
}
\value{
A character string representing the path of the saved image file.
}
\description{
This function takes a ggplot object, adds a logo to it, and saves the result as a PNG file.
}
\details{
This function saves the provided ggplot as a temporary PNG file, reads both the plot and the logo,
resizes the logo, calculates its position on the plot, and overlays the logo. The final image is saved
as a PNG file to the specified output path.
}
\examples{
\dontrun{
library(ggplot2)
plot <- ggplot(mtcars, aes(mpg, hp)) +
  geom_point()
add_logo_to_plot(plot, logo_path = "logo.png", output_path = "plot_with_logo.png")
}

}
