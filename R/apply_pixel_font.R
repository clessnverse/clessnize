#' Apply Pixel Operator font to all text elements in a patchwork or plot while preserving sizes
#'
#' This function ensures that all text elements in a ggplot or patchwork object
#' use the Pixel Operator font family while preserving existing size and style properties.
#'
#' @param plot A ggplot or patchwork object
#' @param scale_factor Optional factor to scale all text sizes (default: 1)
#' @return A plot with Pixel Operator font applied to all text elements
#' @export
apply_pixel_font <- function(plot, scale_factor = 1) {
  # Vérifier si les fonts sont déjà chargés
  if (!("PixelOperatorSC" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "PixelOperatorSC", regular = system.file("fonts/PixelOperatorSC.ttf", package = "clessnize"))
  }
  if (!("WebFont" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "WebFont", regular = system.file("fonts/000webfont.ttf", package = "clessnize"))
  }
  
  # Activer showtext
  showtext::showtext_auto()
  
  # Appliquer la police à tous les éléments textuels en préservant les autres attributs
  plot + 
    ggplot2::theme(
      text = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE),
      plot.title = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE),
      plot.subtitle = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE),
      plot.caption = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE),
      axis.title = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE),
      axis.text = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE),
      legend.text = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE),
      legend.title = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE),
      strip.text = ggplot2::element_text(family = "PixelOperatorSC", inherit.blank = TRUE)
    )
}