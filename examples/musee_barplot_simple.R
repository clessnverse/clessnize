###########################################################
# Exemple d'utilisation simplifiée de create_survey_bar_chart
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

# 4. Créer le graphique en une seule fonction
create_survey_bar_chart(
  # Chemin vers les données d'enquête
  data = readRDS("_SharedFolder_datagotchi_federal_2024/data/app/dataClean/datagotchi2025_canada_app_Ponderee20250328.rds"),
  
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
  caption_suffix = "Données pondérées selon: sexe, âge, province, langue, niveau d'éducation, revenu, immigration, type de logement",
  value_titles = c("JAMAIS", "PARFOIS", "SOUVENT"),
  
  # Sortie
  output_path = "graphs/musee_barplot_fr.png",
  logo_path = "_SharedFolder_datagotchi_federal_2024/logos/FR/logo_black.png",
  width = 16,
  height = 8
)