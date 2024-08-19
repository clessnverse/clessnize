library(ggplot2)

theme_datagotchi_light <- function(base_size = 11,
                              base_family = "",
                              base_line_size = base_size / 22,
                              base_rect_size = base_size / 22,
                              half_line = base_size / 2,
                              base_margin = base_size,

                              primary_colour = "black",
                              secondary_colour = "grey30",
                              minor_colour = "#f7f7f7",
                              bg_colour = "white",
                              strip_colour = "white") {
  # Charger les fonts personnalisés
  sysfonts::font_add(family = "PixelOperatorSC", regular = system.file("fonts/PixelOperatorSC.ttf", package = "clessnize"))
  sysfonts::font_add(family = "WebFont", regular = system.file("fonts/000webfont.ttf", package = "clessnize"))
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

data <- mtcars

ggplot(data, aes(x = mpg, y = wt)) +
  geom_point() +
  theme_datagotchi_light()
