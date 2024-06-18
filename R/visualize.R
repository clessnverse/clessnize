#' Visualize your data
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Functions for data visualization.
#' @inheritParams ggplot2::theme_classic
#' @param primary_colour title and axis colour
#' @param secondary_colour text and axis ticks colour
#' @param minor_colour major gridlines colour
#' @param bg_colour plot background colour
#' @param strip_colour strip background colour
#' @param half_line half line
#' @param base_margin base margin
#' @param boule If TRUE, fills the bars with a PNG image instead of color
#' @param data The data frame to use for the plot
#' @param plot The ggplot object to modify
#' @return A ggplot2 theme or a ggplot2 plot.
#' @details
#' \describe{
#'
#' \item{`theme_clean_light()`}{
#' A clean-looking ggplot2 theme, with x axis line and y major gridlines on a white background.}
#'
#' \item{`theme_clean_dark()`}{
#' A clean-looking ggplot2 theme, with x axis line and y major gridlines or axis ticks on a dark grey background.}
#' }
#' @examples
#' \dontrun{
#' p  <- ggplot2::ggplot(data = ggplot2::mpg) +
#'  ggplot2::geom_point(mapping = ggplot2::aes(x = displ, y = cty, colour = class)) +
#'  ggplot2::labs(
#'    title = "Look at this graph!",
#'    subtitle = "What a great theme, eh?",
#'    caption = "Data: API Twitter \nCLESSN"
#'  ) +
#'  ggplot2::xlab("x axis label") +
#'  ggplot2::ylab("y axis label")
#'
#' p + theme_clean_light()
#' p + theme_clean_dark()
#' }
#' @name visualization
#' @aliases NULL
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_rect
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 margin
#' @importFrom grid rasterGrob
#' @importFrom png readPNG
NULL

#' @export
#' @rdname visualization
theme_clean_light <- function(base_size = 11,
                              base_family = "",
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22,
                              half_line = base_size / 2,
                              base_margin = base_size,

                              primary_colour = "black",
                              secondary_colour = "grey30",
                              minor_colour = "#f7f7f7",
                              bg_colour = "white",
                              strip_colour = "white",
                              boule = FALSE,
                              data = NULL,
                              plot = NULL) {
  if (boule) {
    boule_image_path <- system.file("extdata/boule.png", package = "clessnize")
    return(add_png_fill(plot, data, boule_image_path))
  } else {
    return(
      ggplot2::theme_classic() +
        ggplot2::theme(
          text = ggplot2::element_text(size = base_size, colour = secondary_colour),
          axis.text = ggplot2::element_text(colour = secondary_colour),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_text(
            margin = ggplot2::margin(r = half_line, t = half_line),
            hjust = 0.5
          ),
          axis.title.y = ggplot2::element_text(
            margin = ggplot2::margin(r = half_line, b = half_line),
            hjust = 0.5
          ),
          axis.line.x = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(fill = NA),
          panel.grid.major.y = ggplot2::element_line(colour = minor_colour),
          plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
          plot.title = ggplot2::element_text(
            face = "bold",
            colour = primary_colour,
            size = base_size * 1.5,
            hjust = 0.5
          ),
          plot.background = ggplot2::element_rect(fill = bg_colour, colour = bg_colour),
          panel.background = ggplot2::element_rect(fill = NA),
          strip.background = ggplot2::element_blank()
        )
    )
  }
}

#' @export
#' @rdname visualization
theme_clean_dark <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22,
                             half_line = base_size / 2,
                             base_margin = base_size,

                             primary_colour = "white",
                             secondary_colour = "#f2f2f2",
                             minor_colour = "#525252",
                             bg_colour = "#494949",
                             strip_colour = "grey80",
                             boule = FALSE,
                             data = NULL,
                             plot = NULL) {
  if (boule) {
    boule_image_path <- system.file("extdata/boule.png", package = "clessnize")
    return(add_png_fill(plot, data, boule_image_path))
  } else {
    return(
      ggplot2::theme_classic() +
        ggplot2::theme(
          text = ggplot2::element_text(size = base_size, colour = secondary_colour),
          axis.text = ggplot2::element_text(colour = secondary_colour),
          axis.ticks.x = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_text(
            margin = ggplot2::margin(r = half_line, t = half_line),
            hjust = 0.5
          ),
          axis.title.y = ggplot2::element_text(
            margin = ggplot2::margin(r = half_line, b = half_line),
            hjust = 0.5
          ),
          axis.line.x = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(fill = NA),
          panel.grid.major.y = ggplot2::element_line(colour = minor_colour),
          plot.caption = ggplot2::element_text(hjust = 0, face = "italic"),
          plot.title = ggplot2::element_text(
            face = "bold",
            colour = primary_colour,
            size = base_size * 1.5,
            hjust = 0.5
          ),
          plot.background = ggplot2::element_rect(fill = bg_colour, colour = bg_colour),
          panel.background = ggplot2::element_rect(fill = NA),
          strip.background = ggplot2::element_blank()
        )
    )
  }
}
