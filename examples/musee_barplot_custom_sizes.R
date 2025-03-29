###########################################################
# Exemple d'utilisation avec personnalisation des tailles de texte
###########################################################

library(clessnize)

# 1. Définir les couleurs des partis
party_colors <- c(
  "PCC" = "#1A4782",  # Conservative - Blue
  "PLC" = "#D71920",   # Liberal - Red
  "BQ" = "#33B2CC",    # Bloc Québécois - Light blue
  "NPD" = "#F58220",   # NDP - Orange
  "PV" = "#3D9B35"     # Green Party - Green
)

# 2. Définir le mapping des partis
party_mapping <- list(
  "cpc" = "PCC",
  "lpc" = "PLC",
  "ndp" = "NPD",
  "bq" = "BQ",
  "gpc" = "PV"
)

# 3. Définir les regroupements de catégories
museum_groups <- list(
  "never_pct" = "never",
  "sometimes_pct" = c("almost_never", "sometimes"),
  "souvent_pct" = c("often", "very_often")
)

# 4. Créer le graphique avec des tailles de texte personnalisées
create_survey_bar_chart(
  # Simuler des données pour cet exemple
  data = data.frame(
    dv_voteChoice = sample(c("cpc", "lpc", "ndp", "bq", "gpc"), 1000, replace = TRUE),
    lifestyle_goMuseumsFreq_factor = sample(
      c("never", "almost_never", "sometimes", "often", "very_often"),
      1000, replace = TRUE
    ),
    weight = runif(1000, 0.5, 1.5)
  ),
  
  # Variables à utiliser
  group_var = "dv_voteChoice",
  category_var = "lifestyle_goMuseumsFreq_factor",
  weight_var = "weight",
  
  # Mapping et configuration
  mapping = party_mapping,
  category_groups = museum_groups,
  group_order = c("PCC", "PLC", "BQ", "NPD", "PV"),
  colors = party_colors,
  
  # Titres et annotations
  title = "FRÉQUENCE DE VISITES AUX MUSÉES PAR AFFILIATION POLITIQUE",
  subtitle = "Pourcentage de partisans selon leur fréquence de visites aux musées",
  caption_suffix = "Données pondérées selon: sexe, âge, province, langue, niveau d'éducation",
  value_titles = c("JAMAIS", "PARFOIS", "SOUVENT"),
  
  # Personnalisation des tailles de texte
  title_size = 18,             # Titre principal plus grand
  subtitle_size = 14,          # Sous-titre légèrement plus grand
  caption_size = 8,            # Légende plus petite
  bar_title_size = 16,         # Titres des barres plus grands
  axis_text_size = 14,         # Texte des axes plus grand
  axis_title_size = 14,        # Titre de l'axe Y plus grand
  
  # Sortie
  output_path = "custom_size_example.png",
  width = 16,
  height = 8
)