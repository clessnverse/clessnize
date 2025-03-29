# clessnize

Package R pour la visualisation graphique du CLESSN, permettant d'appliquer des thèmes personnalisés, de créer des graphiques interactifs et d'utiliser des gabarits standardisés.

## Installation

```r
# Installation depuis GitHub
# devtools::install_github("clessn/clessnize")

# OU, installation locale
# install.packages("path/to/clessnize", repos = NULL, type = "source")
```

## Fonctionnalités principales

### Thèmes graphiques

Le package offre plusieurs thèmes graphiques pour standardiser les visualisations :

- `theme_datagotchi_light()` : Thème clair avec police Pixel Operator
- `theme_datagotchi_dark()` : Thème sombre avec police Pixel Operator
- `theme_clean_light()` : Thème clair minimaliste
- `theme_clean_dark()` : Thème sombre minimaliste

```r
library(ggplot2)
library(clessnize)

ggplot(mtcars, aes(x = mpg, y = hp, color = factor(cyl))) +
  geom_point() +
  labs(title = "Puissance vs Consommation", x = "Miles par gallon", y = "Chevaux") +
  theme_datagotchi_light()
```

### Graphiques automatisés

#### Graphiques en barres multiples

Création simplifiée de graphiques en barres multiples avec `create_survey_bar_chart()` :

```r
library(clessnize)

# Définir les couleurs des partis
party_colors <- c(
  "PCC" = "#1A4782",  # Conservateur - Bleu
  "PLC" = "#D71920",  # Libéral - Rouge
  "BQ" = "#33B2CC",   # Bloc Québécois - Bleu clair
  "NPD" = "#F58220",  # NPD - Orange
  "PV" = "#3D9B35"    # Parti Vert - Vert
)

# Définir le mapping des partis
party_mapping <- list(
  "cpc" = "PCC",
  "lpc" = "PLC",
  "ndp" = "NPD",
  "bq" = "BQ",
  "gpc" = "PV"
)

# Définir les regroupements de catégories
museum_groups <- list(
  "never_pct" = "never",
  "sometimes_pct" = c("almost_never", "sometimes"),
  "souvent_pct" = c("often", "very_often")
)

# Créer le graphique
create_survey_bar_chart(
  data = sondage_data,  # Vos données d'enquête
  group_var = "dv_voteChoice",  # Variable de groupement (ex: intention de vote)
  category_var = "lifestyle_goMuseumsFreq_factor",  # Variable de catégorie (ex: fréquence de visites)
  weight_var = "weight",  # Variable de pondération
  
  # Mapping et configuration
  mapping = party_mapping,  # Conversion des codes en noms affichés
  category_groups = museum_groups,  # Regroupement de catégories
  group_order = c("PCC", "PLC", "BQ", "NPD", "PV"),  # Ordre des groupes
  colors = party_colors,  # Couleurs des groupes
  
  # Titres et annotations
  title = "FRÉQUENCE DE VISITES AUX MUSÉES PAR AFFILIATION POLITIQUE",
  subtitle = "Pourcentage de partisans selon leur fréquence de visites aux musées",
  caption_suffix = "Données pondérées selon: sexe, âge, province, langue, niveau d'éducation",
  value_titles = c("JAMAIS", "PARFOIS", "SOUVENT"),  # Titres des barres
  
  # Personnalisation des tailles de texte
  title_size = 18,
  subtitle_size = 14,
  caption_size = 10,
  bar_title_size = 16,
  axis_text_size = 14,
  
  # Sortie
  output_path = "graphs/musee_barplot.png",
  logo_path = "logos/logo_clessn.png"  # Logo à ajouter au graphique
)
```

#### Personnalisation avancée avec `create_multi_bar_chart()`

Pour les cas où vous avez déjà des données agrégées ou si vous voulez plus de contrôle :

```r
# Données déjà agrégées
data_agrege <- data.frame(
  party = c("PCC", "PLC", "BQ", "NPD", "PV"),
  never_pct = c(35, 25, 20, 15, 40),
  sometimes_pct = c(45, 50, 55, 50, 45),
  souvent_pct = c(20, 25, 25, 35, 15)
)

create_multi_bar_chart(
  data = data_agrege,
  group_var = "party",
  value_vars = c("never_pct", "sometimes_pct", "souvent_pct"),
  value_titles = c("JAMAIS", "PARFOIS", "SOUVENT"),
  colors = party_colors,
  main_title = "FRÉQUENCE DE VISITES AUX MUSÉES PAR AFFILIATION POLITIQUE",
  y_max = 100,  # Définir manuellement l'échelle de l'axe Y
  percent = TRUE,  # Afficher les valeurs en pourcentage
  output_path = "graphs/musee_barplot_agrege.png"
)
```

### Application de la police Pixel Operator

Pour appliquer la police Pixel Operator à n'importe quel graphique ggplot2 ou patchwork :

```r
library(ggplot2)
library(patchwork)
library(clessnize)

# Créer des graphiques
p1 <- ggplot(mtcars, aes(x = mpg, y = hp)) + 
  geom_point() + 
  labs(title = "MPG vs Puissance")

p2 <- ggplot(mtcars, aes(x = wt, y = hp)) + 
  geom_point() + 
  labs(title = "Poids vs Puissance")

# Combiner avec patchwork
combined <- p1 + p2 + plot_layout(nrow = 1)

# Appliquer la police Pixel Operator
final_plot <- apply_pixel_font(combined)

# Sauvegarder
ggsave("combined_plot.png", final_plot, width = 12, height = 6)
```

### Ajout de logo

Ajoutez facilement un logo à vos graphiques :

```r
library(clessnize)

# Créer un graphique
p <- ggplot(mtcars, aes(x = mpg, y = hp)) + 
  geom_point() + 
  theme_datagotchi_light() +
  labs(title = "MPG vs Puissance")

# Sauvegarder avec logo
ggsave("plot_temp.png", p, width = 8, height = 6)
add_logo_to_plot(
  plot = p,
  logo_path = "path/to/logo.png",
  output_path = "plot_with_logo.png",
  logo_position = "bottomright"
)
```

## Couleurs officielles

Le package fournit des variables contenant les couleurs officielles :

```r
# Couleurs Datagotchi
datagotchi_green_light  # "#1BF640"
datagotchi_green_dark   # "#26E92E"

# Couleurs des partis politiques (USA 2024)
party_colors  # Vecteur nommé de couleurs pour les partis
```

## Vérification de l'accès aux polices

Pour vérifier que les polices sont correctement installées :

```r
check_fonts_access()
```

## Exemples

Le dossier `examples/` contient plusieurs scripts montrant l'utilisation des différentes fonctionnalités du package.

## Dépendances

- ggplot2
- dplyr
- patchwork
- scales
- sysfonts
- showtext
- magick (pour l'ajout de logos)

## Licence

Ce package est distribué sous licence MIT.