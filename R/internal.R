#' Add PNG image overlay to bars
#'
#' @param plot The ggplot object
#' @param data The data frame containing the counts
#' @param image_path The path to the PNG image to overlay
#' @return The ggplot object with the PNG image overlay
#' @noRd
add_png_fill <- function(plot, data, image_path) {
  # Load the PNG image
  image <- png::readPNG(image_path)
  
  for (i in 1:nrow(data)) {
    plot <- plot + ggplot2::annotation_custom(
      grid::rasterGrob(image, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE),
      xmin = i - 0.5, xmax = i + 0.5, ymin = 0, ymax = data$n[i]
    )
  }
  return(plot)
}