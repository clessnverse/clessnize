#' Check if font files are accessible
#'
#' This function checks if the specified font files are accessible via system.file().
#' @return A message indicating whether each font file is found or not.
#' @export
check_fonts_access <- function() {
  fonts <- c("PixelOperatorSC.ttf", "000webfont.ttf")
  font_paths <- sapply(fonts, function(font) system.file(paste0("fonts/", font), package = "clessnize"))

  # Vérifier si les chemins ne sont pas vides
  results <- sapply(font_paths, function(path) if (path == "") "Not found" else "Found")
  
  # Créer un message récapitulatif
  message(paste(names(results), ":", results, font_paths, collapse = "\n"))
  
  return("check_fonts_access is done")
}

#' Theme Datagotchi Light
#'
#' A custom ggplot2 theme that applies the PixelOperatorSC or WebFont fonts,
#' depending on the user's selection, with various aesthetic adjustments.
#'
#' @param base_size Numeric, the base font size. Default is 11.
#' @param base_family Character, the base font family. Default is "PixelOperatorSC".
#' @param base_line_size Numeric, the base line size. Default is `base_size / 22`.
#' @param base_rect_size Numeric, the base rectangle size. Default is `base_size / 22`.
#' @param half_line Numeric, half the line size. Default is `base_size / 2`.
#' @param base_margin Numeric, the base margin size. Default is `base_size`.
#' @param primary_colour Character, the primary color used in the theme. Default is "black".
#' @param secondary_colour Character, the secondary color used in the theme. Default is "grey30".
#' @param minor_colour Character, the color for minor elements like grid lines. Default is "#f7f7f7".
#' @param bg_colour Character, the background color. Default is "white".
#' @param strip_colour Character, the strip background color. Default is "white".
#'
#' @details
#' This theme is part of the `clessnize` package and is designed to integrate custom fonts
#' into ggplot2 visualizations with minimal configuration. The function automatically checks
#' if the required fonts are available, and loads them if necessary. The `showtext` package
#' is used to ensure that the fonts render correctly.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   data <- mtcars
#'   ggplot(data, aes(x = mpg, y = wt)) +
#'     geom_point() +
#'     theme_datagotchi_light()
#' }
#'
#' @export
theme_datagotchi_light <- function(base_size = 11,
                              base_family = "PixelOperatorSC",
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22,
                              half_line = base_size / 2,
                              base_margin = base_size,

                              primary_colour = "black",
                              secondary_colour = "grey30",
                              minor_colour = "#f7f7f7",
                              bg_colour = "white",
                              strip_colour = "white") {
  # Vérifier si les fonts sont déjà chargés
  if (!("PixelOperatorSC" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "PixelOperatorSC", regular = system.file("fonts/PixelOperatorSC.ttf", package = "clessnize"))
  }
  if (!("WebFont" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "WebFont", regular = system.file("fonts/000webfont.ttf", package = "clessnize"))
  }
  
  # Activer showtext
  showtext::showtext_auto()
  # Créer le thème en utilisant la font spécifiée
  ggplot2::theme_classic() +
    ggplot2::theme(
      text = ggplot2::element_text(size = base_size, family = base_family, colour = secondary_colour),
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
}

#' Theme Datagotchi Dark
#'
#' A custom ggplot2 theme that applies the PixelOperatorSC or WebFont fonts,
#' depending on the user's selection, with various aesthetic adjustments.
#'
#' @param base_size Numeric, the base font size. Default is 11.
#' @param base_family Character, the base font family. Default is "PixelOperatorSC".
#' @param base_line_size Numeric, the base line size. Default is `base_size / 22`.
#' @param base_rect_size Numeric, the base rectangle size. Default is `base_size / 22`.
#' @param half_line Numeric, half the line size. Default is `base_size / 2`.
#' @param base_margin Numeric, the base margin size. Default is `base_size`.
#' @param primary_colour Character, the primary color used in the theme. Default is "black".
#' @param secondary_colour Character, the secondary color used in the theme. Default is "grey30".
#' @param minor_colour Character, the color for minor elements like grid lines. Default is "#f7f7f7".
#' @param bg_colour Character, the background color. Default is "white".
#' @param strip_colour Character, the strip background color. Default is "white".
#'
#' @details
#' This theme is part of the `clessnize` package and is designed to integrate custom fonts
#' into ggplot2 visualizations with minimal configuration. The function automatically checks
#' if the required fonts are available, and loads them if necessary. The `showtext` package
#' is used to ensure that the fonts render correctly.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' \dontrun{
#'   library(ggplot2)
#'   data <- mtcars
#'   ggplot(data, aes(x = mpg, y = wt)) +
#'     geom_point() +
#'     theme_datagotchi_light()
#' }
#'
#' @export
theme_datagotchi_dark <- function(base_size = 11,
  base_family = "PixelOperatorSC",
  base_line_size = base_size / 22,
  base_rect_size = base_size / 22,
  half_line = base_size / 2,
  base_margin = base_size,

  primary_colour = "white",
  secondary_colour = "#f2f2f2",
  minor_colour = "#525252",
  bg_colour = "#494949",
  strip_colour = "grey80") {
# Vérifier si les fonts sont déjà chargés
if (!("PixelOperatorSC" %in% sysfonts::font_families())) {
sysfonts::font_add(family = "PixelOperatorSC", regular = system.file("fonts/PixelOperatorSC.ttf", package = "clessnize"))
}
if (!("WebFont" %in% sysfonts::font_families())) {
sysfonts::font_add(family = "WebFont", regular = system.file("fonts/000webfont.ttf", package = "clessnize"))
}

# Activer showtext
showtext::showtext_auto()
# Créer le thème en utilisant la font spécifiée
ggplot2::theme_classic() +
ggplot2::theme(
text = ggplot2::element_text(size = base_size, family = base_family, colour = secondary_colour),
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

}


#' Datagotchi Green Light
#'
#' A color code for the Datagotchi theme.
#'
#' @return A character string representing the color code "#1BF640".
#' @export
datagotchi_green_light <- "#1BF640"

#' Datagotchi Green Dark
#' 
#' A color code for the Datagotchi theme.
#' 
#' @return A character string representing the color code "#26E92E".
#' @export
datagotchi_green_dark <- "#26E92E"

#' Party Colors
#'
#' A named vector containing the colors associated with the USA political parties (2024).
#'
#' @format A named character vector with color codes.
#' @export
party_colors <- c("democrat" = "#0076CE",
                  "republican" = "#FF0000",
                  "rfk" = "#FFD700")
#' Add Datagotchi Logo to a ggplot
#'
#' This function overlays the Datagotchi logo onto a ggplot using the `cowplot` package.
#' The logo is placed as an image on top of the existing plot.
#'
#' @param plot A `ggplot` object. The plot to which the logo will be added.
#' @param logo_url A character string specifying the URL of the logo image. 
#'        Defaults to "https://raw.githubusercontent.com/clessn/img/refs/heads/main/Logo.PNG".
#' @param logo_width A numeric value specifying the width of the logo. Defaults to 0.1.
#' @param x_pos A numeric value for the x position of the logo. Defaults to 1 (right edge of the plot).
#' @param y_pos A numeric value for the y position of the logo. Defaults to 0 (bottom edge of the plot).
#' @param hjust Horizontal justification of the logo. Defaults to 1 (right-aligned).
#' @param vjust Vertical justification of the logo. Defaults to 0 (bottom-aligned).
#'
#' @return A `ggplot` object with the logo added.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(cowplot)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' add_datagotchi_logo(p)
#'
#' # Adjust the logo position and size
#' add_datagotchi_logo(p, logo_width = 0.15, x_pos = 0.95, y_pos = 0.05)
add_datagotchi_logo <- function(plot, 
                                logo_url = "https://raw.githubusercontent.com/clessn/img/refs/heads/main/Logo.PNG", 
                                logo_width = 0.1, 
                                x_pos = 1, y_pos = 0, 
                                hjust = 1, vjust = 0) {
  # Create the logo as a ggdraw object
  logo <- cowplot::ggdraw() +
    cowplot::draw_image(logo_url, x = x_pos, y = y_pos, 
                        hjust = hjust, vjust = vjust, 
                        width = logo_width)
  
  # Combine the original plot and the logo
  combined_plot <- cowplot::ggdraw(plot) + 
    cowplot::draw_plot(logo)
  
  return(combined_plot)
}
