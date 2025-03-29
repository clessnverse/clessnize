###########################################################
# Exemple d'utilisation directe du thème datagotchi_light
###########################################################

library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(clessnize)  # Charger le package clessnize

# S'assurer que les polices sont chargées
check_fonts_access()

# 1. Définir les couleurs officielles des partis
party_colors <- c(
  "PCC" = "#1A4782",  # Conservative - Blue
  "PLC" = "#D71920",   # Liberal - Red
  "BQ" = "#33B2CC",    # Bloc Québécois - Light blue
  "NPD" = "#F58220",   # NDP - Orange
  "PV" = "#3D9B35"     # Green Party - Green
)

# 2. Simuler des données pour l'exemple
museum_data <- data.frame(
  party = rep(c("PCC", "PLC", "BQ", "NPD", "PV"), 3),
  frequency = rep(c("never_pct", "sometimes_pct", "souvent_pct"), each = 5),
  value = c(
    # Valeurs pour "never_pct"
    35, 25, 20, 15, 40,
    # Valeurs pour "sometimes_pct"
    45, 50, 55, 50, 45,
    # Valeurs pour "souvent_pct"
    20, 25, 25, 35, 15
  )
)

# Convertir la colonne party en facteur pour préserver l'ordre
museum_data$party <- factor(museum_data$party, levels = c("PCC", "PLC", "BQ", "NPD", "PV"))

# 3. Déterminer les limites maximales pour l'axe Y
y_max <- max(museum_data$value) * 1.1  # Ajouter 10% pour l'espace
y_max <- ceiling(y_max / 10) * 10      # Arrondir à la dizaine supérieure

# 4. Fonction pour créer les graphiques avec la même échelle
create_museum_plot <- function(data, freq_value, title, show_axis = FALSE) {
  # Filtrer les données pour la fréquence spécifiée
  plot_data <- data %>% filter(frequency == freq_value)
  
  p <- ggplot(plot_data, aes(x = party, y = value, fill = party)) +
    geom_col(width = 0.7) +
    scale_fill_manual(values = party_colors) +
    scale_y_continuous(
      labels = if(show_axis) percent_format(scale = 1) else NULL, 
      limits = c(0, y_max),
      breaks = seq(0, y_max, by = 20)  # Grille cohérente
    ) +
    labs(
      title = toupper(title),
      x = "",
      y = if(show_axis) "Pourcentage de répondants" else ""
    ) +
    # Appliquer directement le thème datagotchi light
    theme_datagotchi_light() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none"
    )
  
  return(p)
}

# 5. Créer les graphiques pour chaque fréquence de visite
plot_never <- create_museum_plot(museum_data, "never_pct", "JAMAIS", show_axis = TRUE) +
  theme(plot.margin = margin(r = 15))

plot_sometimes <- create_museum_plot(museum_data, "sometimes_pct", "PARFOIS", show_axis = FALSE)

plot_souvent <- create_museum_plot(museum_data, "souvent_pct", "SOUVENT", show_axis = FALSE)

# 6. Combiner les graphiques
combined_plot <- plot_never + plot_sometimes + plot_souvent +
  plot_layout(nrow = 1, widths = c(1, 1, 1)) 

# 7. Ajouter titre global et annotations
final_plot <- combined_plot +
  plot_annotation(
    title = "FRÉQUENCE DE VISITES AUX MUSÉES PAR AFFILIATION POLITIQUE",
    subtitle = "Pourcentage de partisans selon leur fréquence de visites aux musées",
    caption = "Source: Exemple de données simulées | n = 1000\nDonnées pondérées selon: sexe, âge, province, langue, niveau d'éducation",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", family = "PixelOperatorSC"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "#555555", margin = margin(t = 5, b = 15), family = "PixelOperatorSC"),
      plot.caption = element_text(hjust = 0.5, size = 10, color = "#666666", lineheight = 1.2, margin = margin(t = 10), family = "PixelOperatorSC")
    )
  )

# 8. Sauvegarder
ggsave("musee_barplot_fr_theme_example.png", 
       final_plot, 
       width = 16, 
       height = 8,
       dpi = 200,
       bg = "white")

# 9. Ajouter le logo CLESSN
logo_path <- system.file("extdata/logo.png", package = "clessnize")
# Si le logo n'existe pas dans le package, utiliser l'URL GitHub
if (logo_path == "") {
  logo_path <- "https://raw.githubusercontent.com/clessn/img/refs/heads/main/Logo.PNG"
}
add_logo_to_plot(final_plot, logo_path = logo_path, output_path = "musee_barplot_fr_theme_example_with_logo.png")

# Afficher le résultat
# Normalement, on le verrait dans RStudio
cat("Graphiques créés avec succès! Vérifiez les fichiers 'musee_barplot_fr_theme_example.png' et 'musee_barplot_fr_theme_example_with_logo.png'.")