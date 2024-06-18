library(ggplot2)
library(grid)
library(png)


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
                             
                             boule = FALSE) {
  # Add font
  # sysfonts::font_add_google("Roboto", "roboto")
  # showtext::showtext_auto()
  
  # Base theme
  theme <- ggplot2::theme_classic() +
    # Changes to apply to base theme
    ggplot2::theme(
      text = ggplot2::element_text(size = base_size, colour = secondary_colour),
      axis.text = ggplot2::element_text(colour = secondary_colour),
      axis.ticks.x = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        margin = ggplot2::margin(r = half_line,
                                 t = half_line),
        hjust = 0.5
      ),
      axis.title.y = ggplot2::element_text(
        margin = ggplot2::margin(r = half_line,
                                 b = half_line),
        hjust = 0.5
      ),
      axis.line.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(fill = NA),
      # Align caption with left
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
  
  # Conditionally add background image
  if (boule) {
    boule_image_path <- system.file("boule/image.png", package = "clessnize")
    boule_image <- readPNG(boule_image_path)
    boule_grob <- rasterGrob(boule_image, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = TRUE)
    
    theme <- theme +
      annotation_custom(boule_grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
  }
  
  return(theme)
}

# Example usage
plot <- ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_bar(stat = "identity") +
  theme_clean_dark(boule = TRUE)

ggsave("example_plot.png", plot = plot, width = 10, height = 6, dpi = 300)
