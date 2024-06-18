#' Add image overlay to bars
#'
#' @param plot The ggplot object
#' @param data The data frame containing the counts
#' @param image_path The path to the image to overlay
#' @return The ggplot object with the image overlay
#' @noRd
add_image_bars <- function(plot, data, image_path) {
  # Load the image
  image <- png::readPNG(image_path)
  
  for (i in 1:nrow(data)) {
    plot <- plot + ggplot2::annotation_custom(
      grid::rasterGrob(image, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE),
      xmin = i - 0.5, xmax = i + 0.5, ymin = 0, ymax = data$n[i]
    )
  }
  return(plot)
}

#' Create a ggplot with image overlay on bars
#'
#' @param data The data frame to use for the plot
#' @param mapping Default list of aesthetic mappings to use for plot
#' @param image_path The path to the image to overlay on bars
#' @param ... Other arguments passed to ggplot
#' @return A ggplot object with the image overlay on bars
#' @export
ggboule <- function(data, mapping = aes(), image_path = system.file("extdata/boule.png", package = "clessnize"), ...) {
  # Create the base ggplot object
  plot <- ggplot2::ggplot(data, mapping, ...)
  
  # Add geom_bar to the plot
  plot <- plot + ggplot2::geom_bar(stat = "identity", fill = NA, color = "black")
  
  # Apply the image overlay
  plot <- add_image_bars(plot, data, image_path)
  
  return(plot)
}
