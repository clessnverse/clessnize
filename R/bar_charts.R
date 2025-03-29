#' Créer un graphique en barres multiples avec des catégories
#'
#' Cette fonction crée un graphique en barres multiples pour plusieurs catégories,
#' avec une mise en page propre et un style Datagotchi.
#'
#' @param data Un data.frame contenant les données à visualiser
#' @param group_var Nom de la variable de regroupement (par ex. "party")
#' @param value_vars Vecteur de noms des variables de valeurs (par ex. c("never_pct", "sometimes_pct", "often_pct"))
#' @param value_titles Vecteur de titres pour chaque variable de valeur (même longueur que value_vars)
#' @param colors Vecteur nommé de couleurs pour chaque niveau de group_var
#' @param group_levels Vecteur optionnel pour définir l'ordre des niveaux de group_var
#' @param main_title Titre principal du graphique
#' @param subtitle Sous-titre du graphique
#' @param caption Légende du graphique
#' @param y_axis_title Titre de l'axe Y (apparaît uniquement sur le premier graphique)
#' @param y_max Valeur maximale de l'axe Y (si NULL, calculée automatiquement)
#' @param percent Booléen indiquant si les valeurs sont des pourcentages
#' @param output_path Chemin pour sauvegarder le graphique
#' @param width Largeur du graphique en pouces
#' @param height Hauteur du graphique en pouces
#' @param logo_path Chemin vers le logo à ajouter
#' @param logo_position Position du logo ("bottomright", "bottomleft", "topright", "topleft")
#' @param logo_width Largeur du logo en proportion de la largeur du graphique
#' @param title_size Taille du titre principal
#' @param subtitle_size Taille du sous-titre
#' @param caption_size Taille du texte de la légende
#' @param bar_title_size Taille des titres des barres
#' @param axis_text_size Taille du texte des axes
#' @param axis_title_size Taille du titre de l'axe Y
#'
#' @return Le graphique ggplot final
#'
#' @examples
#' \dontrun{
#' # Créer un dataframe de données
#' df <- data.frame(
#'   party = rep(c("PCC", "PLC", "BQ", "NPD", "PV"), 3),
#'   category = rep(c("never", "sometimes", "often"), each = 5),
#'   value = c(
#'     35, 25, 20, 15, 40,
#'     45, 50, 55, 50, 45,
#'     20, 25, 25, 35, 15
#'   )
#' )
#' 
#' # Convertir à un format plus large
#' df_wide <- tidyr::pivot_wider(
#'   df, 
#'   names_from = category, 
#'   values_from = value
#' )
#'
#' # Définir les couleurs
#' party_colors <- c(
#'   "PCC" = "#1A4782",
#'   "PLC" = "#D71920",
#'   "BQ" = "#33B2CC", 
#'   "NPD" = "#F58220",
#'   "PV" = "#3D9B35"
#' )
#'
#' # Créer le graphique
#' create_multi_bar_chart(
#'   data = df_wide,
#'   group_var = "party",
#'   value_vars = c("never", "sometimes", "often"),
#'   value_titles = c("JAMAIS", "PARFOIS", "SOUVENT"),
#'   colors = party_colors,
#'   main_title = "FRÉQUENCE DE VISITES AUX MUSÉES PAR AFFILIATION POLITIQUE",
#'   subtitle = "Pourcentage de partisans selon leur fréquence de visites aux musées",
#'   caption = "Source: Données simulées",
#'   percent = TRUE,
#'   output_path = "musee_barplot.png"
#' )
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_col scale_fill_manual scale_y_continuous labs theme element_text element_blank element_rect margin
#' @importFrom dplyr %>% 
#' @importFrom patchwork plot_layout plot_annotation
#' @importFrom magick image_read image_info image_scale image_composite image_write
#' @importFrom scales percent_format
#' @export
create_multi_bar_chart <- function(data, 
                                 group_var,
                                 value_vars,
                                 value_titles = NULL,
                                 colors = NULL,
                                 group_levels = NULL,
                                 main_title = NULL,
                                 subtitle = NULL,
                                 caption = NULL,
                                 y_axis_title = "Pourcentage",
                                 y_max = NULL,
                                 percent = TRUE,
                                 output_path = "plot.png",
                                 width = 16,
                                 height = 8,
                                 logo_path = NULL,
                                 logo_position = "bottomright",
                                 logo_width = 0.15,
                                 title_size = 14,
                                 subtitle_size = 12,
                                 caption_size = 10,
                                 bar_title_size = 14,
                                 axis_text_size = 12,
                                 axis_title_size = 12) {
  
  # Vérifier si le package patchwork est installé
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Le package 'patchwork' est nécessaire pour cette fonction. Installez-le avec install.packages('patchwork')")
  }
  
  # Vérifier si le package magick est installé si un logo est spécifié
  if (!is.null(logo_path) && !requireNamespace("magick", quietly = TRUE)) {
    stop("Le package 'magick' est nécessaire pour ajouter un logo. Installez-le avec install.packages('magick')")
  }
  
  # Charger les polices si nécessaire
  if (!("PixelOperatorSC" %in% sysfonts::font_families())) {
    sysfonts::font_add(family = "PixelOperatorSC", regular = system.file("fonts/PixelOperatorSC.ttf", package = "clessnize"))
  }
  
  # Activer showtext
  showtext::showtext_auto()
  
  # Vérifier les titres des valeurs
  if (is.null(value_titles)) {
    value_titles <- toupper(value_vars)
  } else if (length(value_titles) != length(value_vars)) {
    stop("value_titles doit avoir la même longueur que value_vars")
  }
  
  # Convertir la variable de groupe en facteur si nécessaire
  if (!is.null(group_levels)) {
    data[[group_var]] <- factor(data[[group_var]], levels = group_levels)
  } else if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- factor(data[[group_var]])
  }
  
  # Définir ou vérifier les couleurs
  if (is.null(colors)) {
    # Créer une palette par défaut
    group_levels <- levels(data[[group_var]])
    n_groups <- length(group_levels)
    colors <- setNames(scales::hue_pal()(n_groups), group_levels)
  } else if (!all(levels(data[[group_var]]) %in% names(colors))) {
    stop("Toutes les valeurs de group_var doivent avoir une couleur correspondante dans 'colors'")
  }
  
  # Déterminer la valeur maximale pour l'axe Y si non spécifiée
  if (is.null(y_max)) {
    max_values <- sapply(value_vars, function(var) max(data[[var]], na.rm = TRUE))
    y_max <- max(max_values) * 1.1  # Ajouter 10% pour l'espace
    y_max <- ceiling(y_max / 10) * 10  # Arrondir à la dizaine supérieure
  }
  
  # Créer une fonction pour générer chaque graphique
  create_single_plot <- function(var, title, show_axis = FALSE) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[group_var]], y = .data[[var]], fill = .data[[group_var]])) +
      ggplot2::geom_col(width = 0.7) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::scale_y_continuous(
        labels = if(show_axis) {
          if(percent) scales::percent_format(scale = 1) else waiver()
        } else NULL,
        limits = c(0, y_max),
        breaks = seq(0, y_max, by = ifelse(y_max > 50, 20, 10))
      ) +
      ggplot2::labs(
        title = toupper(title),
        x = "",
        y = if(show_axis) y_axis_title else ""
      ) +
      ggplot2::theme(
        text = ggplot2::element_text(family = "PixelOperatorSC"),
        plot.title = ggplot2::element_text(
          hjust = 0.5, 
          face = "bold", 
          size = bar_title_size, 
          color = "black", 
          family = "PixelOperatorSC"
        ),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(
          size = axis_text_size, 
          color = "black", 
          family = "PixelOperatorSC"
        ),
        axis.text.y = if(show_axis) {
          ggplot2::element_text(
            size = axis_text_size, 
            color = "black", 
            family = "PixelOperatorSC"
          )
        } else ggplot2::element_blank(),
        axis.title.y = if(show_axis) {
          ggplot2::element_text(
            size = axis_title_size, 
            family = "PixelOperatorSC"
          )
        } else ggplot2::element_blank(),
        legend.position = "none",
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.grid.major.y = ggplot2::element_line(color = "#f7f7f7"),
        axis.line.x = ggplot2::element_blank(),
        axis.line.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
    
    return(p)
  }
  
  # Créer les graphiques pour chaque variable de valeur
  plot_list <- list()
  for (i in seq_along(value_vars)) {
    is_first <- i == 1
    plot_list[[i]] <- create_single_plot(value_vars[i], value_titles[i], show_axis = is_first)
    
    # Ajouter une marge à droite pour le premier graphique
    if (is_first) {
      plot_list[[i]] <- plot_list[[i]] + ggplot2::theme(plot.margin = ggplot2::margin(r = 15))
    }
  }
  
  # Combiner les graphiques
  combined_plot <- patchwork::wrap_plots(plot_list, nrow = 1, widths = rep(1, length(value_vars)))
  
  # Ajouter titre global et annotations
  final_plot <- combined_plot +
    patchwork::plot_annotation(
      title = main_title,
      subtitle = subtitle,
      caption = caption,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          hjust = 0.5, 
          size = title_size, 
          face = "bold", 
          family = "PixelOperatorSC"
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5, 
          size = subtitle_size, 
          color = "#555555", 
          margin = ggplot2::margin(t = 5, b = 15), 
          family = "PixelOperatorSC"
        ),
        plot.caption = ggplot2::element_text(
          hjust = 0.5, 
          size = caption_size, 
          color = "#666666", 
          lineheight = 1.2, 
          margin = ggplot2::margin(t = 10), 
          family = "PixelOperatorSC"
        ),
        plot.background = ggplot2::element_rect(fill = "white", color = NA)
      )
    )
  
  # Sauvegarder le graphique
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir) && output_dir != ".") {
    dir.create(output_dir, recursive = TRUE)
  }
  
  ggplot2::ggsave(output_path, final_plot, width = width, height = height, dpi = 200, bg = "white")
  
  # Ajouter le logo si un chemin est spécifié
  if (!is.null(logo_path)) {
    # Lire l'image sauvegardée
    plot_img <- magick::image_read(output_path)
    
    # Vérifier si le logo existe
    if (!file.exists(logo_path)) {
      warning(paste("Le fichier logo", logo_path, "n'existe pas. Le logo n'a pas été ajouté."))
      return(final_plot)
    }
    
    # Charger et redimensionner le logo
    logo <- magick::image_read(logo_path)
    logo_size <- round(magick::image_info(plot_img)$width * logo_width)
    logo_resized <- magick::image_scale(logo, paste0(logo_size, "x"))
    
    # Calculer la position
    logo_info <- magick::image_info(logo_resized)
    plot_info <- magick::image_info(plot_img)
    margin_px <- round(plot_info$width * 0.01)  # 1% de marge
    
    if (logo_position == "bottomright") {
      x_pos <- plot_info$width - logo_info$width - margin_px
      y_pos <- plot_info$height - logo_info$height - margin_px
    } else if (logo_position == "bottomleft") {
      x_pos <- margin_px
      y_pos <- plot_info$height - logo_info$height - margin_px
    } else if (logo_position == "topright") {
      x_pos <- plot_info$width - logo_info$width - margin_px
      y_pos <- margin_px
    } else if (logo_position == "topleft") {
      x_pos <- margin_px
      y_pos <- margin_px
    } else {
      stop("Position du logo invalide. Utilisez 'bottomright', 'bottomleft', 'topright' ou 'topleft'.")
    }
    
    # Ajouter le logo
    plot_with_logo <- magick::image_composite(
      plot_img, 
      logo_resized, 
      offset = paste0("+", x_pos, "+", y_pos)
    )
    
    # Sauvegarder l'image finale
    magick::image_write(plot_with_logo, output_path)
  }
  
  return(final_plot)
}


#' Créer un graphique en barres multiples à partir de données d'enquête
#'
#' Cette fonction simplifie le processus de création d'un graphique en barres multiples
#' en automatisant le calcul des pourcentages à partir de données d'enquête brutes.
#'
#' @param data Un data.frame contenant les données d'enquête
#' @param group_var Nom de la variable de groupement (ex: "dv_voteChoice")
#' @param category_var Nom de la variable de catégorie (ex: "lifestyle_goMuseumsFreq_factor")
#' @param weight_var Nom de la variable de pondération (default: "weight")
#' @param mapping Un named list qui définit comment convertir les valeurs de group_var en noms affichés
#' @param category_order Un vecteur définissant l'ordre des niveaux de category_var
#' @param category_groups Une liste nommée regroupant les niveaux de category_var en groupes
#' @param group_order Un vecteur définissant l'ordre des niveaux de group_var après la conversion
#' @param colors Un vecteur nommé de couleurs pour chaque niveau de group_var après la conversion
#' @param title Titre principal du graphique
#' @param subtitle Sous-titre du graphique
#' @param caption Légende du graphique (par défaut, contient des infos sur les données)
#' @param caption_suffix Texte à ajouter à la fin de la légende générée automatiquement
#' @param output_path Chemin pour sauvegarder le graphique
#' @param logo_path Chemin vers le logo à ajouter
#' @param ... Arguments supplémentaires passés à create_multi_bar_chart
#'
#' @return Le graphique ggplot final
#'
#' @examples
#' \dontrun{
#' # Simuler un jeu de données d'enquête
#' set.seed(123)
#' survey_data <- data.frame(
#'   dv_voteChoice = sample(c("cpc", "lpc", "ndp", "bq", "gpc"), 1000, replace = TRUE),
#'   lifestyle_goMuseumsFreq_factor = sample(
#'     c("never", "almost_never", "sometimes", "often", "very_often"),
#'     1000, replace = TRUE
#'   ),
#'   weight = runif(1000, 0.5, 1.5)
#' )
#'
#' # Définir le mapping et les couleurs des partis
#' party_mapping <- list(
#'   "cpc" = "PCC",
#'   "lpc" = "PLC",
#'   "ndp" = "NPD",
#'   "bq" = "BQ",
#'   "gpc" = "PV"
#' )
#'
#' party_colors <- c(
#'   "PCC" = "#1A4782",
#'   "PLC" = "#D71920",
#'   "BQ" = "#33B2CC", 
#'   "NPD" = "#F58220",
#'   "PV" = "#3D9B35"
#' )
#'
#' # Définir les groupes de catégories
#' museum_groups <- list(
#'   "never_pct" = "never",
#'   "sometimes_pct" = c("almost_never", "sometimes"),
#'   "souvent_pct" = c("often", "very_often")
#' )
#'
#' # Créer le graphique
#' create_survey_bar_chart(
#'   data = survey_data,
#'   group_var = "dv_voteChoice",
#'   category_var = "lifestyle_goMuseumsFreq_factor",
#'   mapping = party_mapping,
#'   category_groups = museum_groups,
#'   colors = party_colors,
#'   title = "FRÉQUENCE DE VISITES AUX MUSÉES PAR AFFILIATION POLITIQUE",
#'   subtitle = "Pourcentage de partisans selon leur fréquence de visites aux musées",
#'   value_titles = c("JAMAIS", "PARFOIS", "SOUVENT"),
#'   output_path = "musee_barplot_survey.png"
#' )
#' }
#'
#' @importFrom dplyr filter group_by summarize mutate %>% 
#' @importFrom rlang .data sym
#' @export
create_survey_bar_chart <- function(data,
                                  group_var,
                                  category_var,
                                  weight_var = "weight",
                                  mapping = NULL,
                                  category_order = NULL,
                                  category_groups = NULL,
                                  group_order = NULL,
                                  colors = NULL,
                                  title = NULL,
                                  subtitle = NULL,
                                  caption = NULL,
                                  caption_suffix = NULL,
                                  value_titles = NULL,
                                  output_path = "plot.png",
                                  logo_path = NULL,
                                  ...) {
  
  # Vérifier que les variables existent dans les données
  if (!group_var %in% names(data)) stop("La variable de groupe n'existe pas dans les données")
  if (!category_var %in% names(data)) stop("La variable de catégorie n'existe pas dans les données")
  if (!weight_var %in% names(data)) stop("La variable de pondération n'existe pas dans les données")
  
  # Enlever les NA pour les variables de groupe et de catégorie
  data <- data %>% 
    dplyr::filter(!is.na(.data[[group_var]]), !is.na(.data[[category_var]]))
  
  # Calculer les pourcentages pondérés selon les groupes définis
  if (is.null(category_groups)) {
    # Si pas de groupes définis, utiliser chaque niveau de category_var comme un groupe
    category_levels <- if (!is.null(category_order)) {
      category_order
    } else {
      unique(data[[category_var]])
    }
    
    category_groups <- setNames(
      as.list(category_levels), 
      paste0(category_levels, "_pct")
    )
  }
  
  # Préparer un dataframe pour stocker les résultats
  result_data <- data %>% 
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarize(sum_weight = sum(.data[[weight_var]], na.rm = TRUE))
  
  # Calculer le pourcentage pour chaque groupe et catégorie
  for (group_name in names(category_groups)) {
    category_values <- category_groups[[group_name]]
    
    pct_data <- data %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::summarize(
        pct_value = sum((.data[[category_var]] %in% category_values) * .data[[weight_var]], na.rm = TRUE) / 
                    sum(.data[[weight_var]], na.rm = TRUE) * 100
      )
    
    # Renommer la colonne pour correspondre au nom du groupe
    names(pct_data)[2] <- group_name
    
    # Fusionner avec le dataframe de résultats
    result_data <- merge(result_data, pct_data, by = group_var)
  }
  
  # Appliquer le mapping si fourni
  if (!is.null(mapping)) {
    result_data <- result_data %>%
      dplyr::mutate(
        mapped_group = dplyr::case_when(
          !!!lapply(names(mapping), function(x) 
            rlang::expr(.data[[group_var]] == !!x ~ !!mapping[[x]])
          ),
          TRUE ~ as.character(.data[[group_var]])
        )
      )
    
    # Remplacer la variable de groupe par la version mappée
    original_group_var <- group_var
    group_var <- "mapped_group"
  }
  
  # Convertir en facteur avec l'ordre spécifié
  if (!is.null(group_order)) {
    result_data[[group_var]] <- factor(result_data[[group_var]], levels = group_order)
  }
  
  # Définir les noms de variables à utiliser
  value_vars <- names(category_groups)
  
  # Créer une légende par défaut si non spécifiée
  if (is.null(caption)) {
    n_responses <- nrow(data)
    caption <- paste0(
      "Source: n = ", format(n_responses, big.mark = " "), 
      "\nDonnées pondérées"
    )
    
    if (!is.null(caption_suffix)) {
      caption <- paste0(caption, "\n", caption_suffix)
    }
  }
  
  # Créer le graphique final
  create_multi_bar_chart(
    data = result_data,
    group_var = group_var,
    value_vars = value_vars,
    value_titles = value_titles,
    colors = colors,
    main_title = title,
    subtitle = subtitle,
    caption = caption,
    percent = TRUE,
    output_path = output_path,
    logo_path = logo_path,
    ...
  )
}