#' Apply Pixel Operator font to all text elements in a patchwork or plot
#'
#' This function ensures that all text elements in a ggplot or patchwork object
#' use the Pixel Operator font family.
#'
#' @param plot A ggplot or patchwork object
#' @return A plot with Pixel Operator font applied to all text elements
#' @export
apply_pixel_font <- function(plot) {
  # Vérifier si les fonts sont déjà chargés
  if (!("PixelOperatorSC" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "PixelOperatorSC", regular = system.file("fonts/PixelOperatorSC.ttf", package = "clessnize"))
  }
  if (!("WebFont" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "WebFont", regular = system.file("fonts/000webfont.ttf", package = "clessnize"))
  }
  
  # Activer showtext
  showtext::showtext_auto()
  
  # Appliquer la police à tous les éléments textuels
  plot + 
    ggplot2::theme(
      text = ggplot2::element_text(family = "PixelOperatorSC"),
      plot.title = ggplot2::element_text(family = "PixelOperatorSC"),
      plot.subtitle = ggplot2::element_text(family = "PixelOperatorSC"),
      plot.caption = ggplot2::element_text(family = "PixelOperatorSC"),
      axis.title = ggplot2::element_text(family = "PixelOperatorSC"),
      axis.text = ggplot2::element_text(family = "PixelOperatorSC"),
      legend.text = ggplot2::element_text(family = "PixelOperatorSC"),
      legend.title = ggplot2::element_text(family = "PixelOperatorSC"),
      strip.text = ggplot2::element_text(family = "PixelOperatorSC")
    )
}