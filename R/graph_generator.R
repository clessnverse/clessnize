library(dplyr)
library(ggplot2)
library(clessnize)
library(scales)

#' Create standardized data visualization graphs
#'
#' @param graph_type String indicating the type of graph: "percentage" or "difference"
#'   - "percentage": Shows percentages of values within each group
#'   - "difference": Shows difference from national average for each group
#' @param data Dataframe containing the data
#' @param x_variable String with the name of the variable for the x-axis (default: "dv_voteChoice")
#' @param fill_variable String with the name of the variable to use for fill colors
#' @param filter_values Optional vector of values to include from fill_variable
#' @param language "fr" or "en" for output language
#' @param colors Named vector with colors for each value in fill_variable
#' @param fill_labels Named vector with display labels for each value in fill_variable
#' @param x_labels Named vector with display labels for each value in x_variable
#' @param x_order Vector specifying the order of values on the x-axis
#' @param fill_order Vector specifying the order of values in the fill variable
#' @param title Graph title
#' @param subtitle Graph subtitle
#' @param y_title Y-axis title
#' @param output_path Path where to save the graph
#' @param add_logo Logical indicating whether to add the Datagotchi logo
#' @param logos_list Optional list of PNG logos to add (see add_png.R)
#'
#' @return A ggplot object
#' @export
create_standardized_graph <- function(
  graph_type = c("percentage", "difference"),
  data,
  x_variable = "dv_voteChoice",
  fill_variable,
  filter_values = NULL,
  language = "fr",
  colors = NULL,
  fill_labels = NULL,
  x_labels = NULL,
  x_order = NULL,
  fill_order = NULL,
  title = NULL,
  subtitle = NULL,
  y_title = NULL,
  output_path = NULL,
  add_logo = TRUE,
  logos_list = NULL
) {
  # Match the graph_type argument
  graph_type <- match.arg(graph_type)
  # No need to manually source add_png.R as it's part of the package
  # Just validate that add_multiple_pngs function is available
  if (!exists("add_multiple_pngs", mode = "function")) {
    if (!requireNamespace("clessnize", quietly = TRUE)) {
      stop("The clessnize package is required but not available")
    }
  }
  
  # Default party colors
  party_colors <- c(
    "PCC" = "#1A4782",  # Conservative - Blue
    "PLC" = "#D71920",  # Liberal - Red
    "BQ" = "#33B2CC",   # Bloc Québécois - Light blue
    "NPD" = "#F58220",  # NDP - Orange
    "PVC" = "#3D9B35"   # Green Party - Green
  )
  
  # English/French party mappings
  party_mapping <- list(
    "fr" = c("lpc" = "PLC", "cpc" = "PCC", "ndp" = "NPD", "bq" = "BQ", "gpc" = "PVC"),
    "en" = c("lpc" = "LPC", "cpc" = "CPC", "ndp" = "NDP", "bq" = "BQ", "gpc" = "GPC")
  )
  
  # Party order
  party_order <- list(
    "fr" = c("PLC", "PCC", "BQ", "NPD", "PVC"),
    "en" = c("LPC", "CPC", "BQ", "NDP", "GPC")
  )
  
  # Default labels based on language
  default_y_title <- if(language == "fr") "Moyenne canadienne" else "Canadian average"
  y_title <- y_title %||% default_y_title
  
  # Caption text based on language
  caption_text <- if(language == "fr") {
    paste0("Source : Léger-Datagotchi 2025 | n = ", nrow(data), 
           "\nDonnées pondérées selon le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, le status d'immigrant et le type d'habitation")
  } else {
    paste0("Source: Léger-Datagotchi 2025 | n = ", nrow(data), 
           "\nData weighted by gender, age, province, language, education level, income, immigration status, and housing type")
  }
  
  # Filter and prepare data
  df_full <- data %>%
    select(all_of(c(x_variable, fill_variable, "weight")))
  
  # Apply filter if provided
  if (!is.null(filter_values)) {
    df_full <- df_full %>%
      filter(.data[[fill_variable]] %in% filter_values)
  }
  
  # Check if x_variable is party/vote choice
  is_party_graph <- x_variable == "dv_voteChoice"
  
  # Process based on graph type
  if (graph_type == "difference") {
    # Difference from national average for each group
    
    # Calculate national averages
    national_averages <- df_full %>%
      group_by(.data[[fill_variable]]) %>%
      summarize(weighted_count = sum(weight, na.rm = TRUE)) %>%
      mutate(national_pct = weighted_count / sum(weighted_count) * 100)
    
    # Calculate group-specific percentages
    group_stats <- df_full %>%
      filter(!is.na(.data[[x_variable]])) %>%
      # Add party-specific filter if relevant
      {if(is_party_graph) filter(., .data[[x_variable]] != "other") else .} %>%
      group_by(.data[[x_variable]], .data[[fill_variable]]) %>%
      summarize(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(.data[[x_variable]]) %>%
      mutate(group_pct = weighted_count / sum(weighted_count) * 100)
    
    # Create plot data with difference from national
    plot_data <- group_stats %>%
      left_join(select(national_averages, {{ fill_variable }}, national_pct), 
                by = fill_variable) %>%
      mutate(pct_diff_from_national = group_pct - national_pct)
    
    # Apply filter if provided
    if (!is.null(filter_values)) {
      plot_data <- plot_data %>%
        filter(.data[[fill_variable]] %in% filter_values)
    }
    
    # If x_variable is party choice, handle party mapping
    if (is_party_graph) {
      # Replace party abbreviations
      plot_data <- plot_data %>%
        mutate(dv_voteChoice = case_when(
          dv_voteChoice %in% names(party_mapping[[language]]) ~ 
            party_mapping[[language]][dv_voteChoice],
          TRUE ~ as.character(dv_voteChoice)
        ))
      
      # Set default order for party names (if x_order not specified)
      if (is.null(x_order)) {
        plot_data$dv_voteChoice <- factor(plot_data$dv_voteChoice, 
                                         levels = party_order[[language]])
      }
    }
    
    # Apply custom ordering for x variable if provided
    if (!is.null(x_order)) {
      plot_data[[x_variable]] <- factor(plot_data[[x_variable]], levels = x_order)
    }
    
    # Apply custom ordering for fill variable if provided
    if (!is.null(fill_order)) {
      plot_data[[fill_variable]] <- factor(plot_data[[fill_variable]], levels = fill_order)
    }
    
    # Create plot
    p <- ggplot(plot_data, aes(x = .data[[x_variable]], 
                               y = pct_diff_from_national, 
                               fill = .data[[fill_variable]])) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black")
    
    # Default subtitle if not provided
    if (is.null(subtitle)) {
      subtitle <- if(language == "fr") {
        "Écart par rapport à la moyenne canadienne (points de %)"
      } else {
        "Difference from Canadian average (percentage points)"
      }
    }
    
  } else if (graph_type == "percentage") {
    # Percentages within each group
    
    # Calculate group-specific percentages
    plot_data <- df_full %>%
      filter(!is.na(.data[[x_variable]])) %>%
      # Add party-specific filter if relevant
      {if(is_party_graph) filter(., .data[[x_variable]] != "other") else .} %>%
      group_by(.data[[x_variable]], .data[[fill_variable]]) %>%
      summarize(weighted_count = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(.data[[x_variable]]) %>%
      mutate(group_pct = weighted_count / sum(weighted_count) * 100)
    
    # If x_variable is party choice, handle party mapping
    if (is_party_graph) {
      # Replace party abbreviations
      plot_data <- plot_data %>%
        mutate(dv_voteChoice = case_when(
          dv_voteChoice %in% names(party_mapping[[language]]) ~ 
            party_mapping[[language]][dv_voteChoice],
          TRUE ~ as.character(dv_voteChoice)
        ))
      
      # Set default order for party names (if x_order not specified)
      if (is.null(x_order)) {
        plot_data$dv_voteChoice <- factor(plot_data$dv_voteChoice, 
                                         levels = party_order[[language]])
      }
    }
    
    # Apply custom ordering for x variable if provided
    if (!is.null(x_order)) {
      plot_data[[x_variable]] <- factor(plot_data[[x_variable]], levels = x_order)
    }
    
    # Apply custom ordering for fill variable if provided
    if (!is.null(fill_order)) {
      plot_data[[fill_variable]] <- factor(plot_data[[fill_variable]], levels = fill_order)
    }
    
    # Create plot
    p <- ggplot(plot_data, aes(x = .data[[x_variable]], 
                               y = group_pct, 
                               fill = .data[[fill_variable]])) +
      geom_bar(stat = "identity", position = "dodge")
    
    # Default subtitle if not provided
    if (is.null(subtitle)) {
      subtitle <- if(language == "fr") {
        "Pourcentage au sein de chaque groupe"
      } else {
        "Percentage within each group"
      }
    }
  }
  
  # Apply color scale and labels if provided
  if (!is.null(colors)) {
    p <- p + scale_fill_manual(values = colors, labels = fill_labels)
  }
  
  # Apply x-axis labels if provided
  if (!is.null(x_labels)) {
    p <- p + scale_x_discrete(labels = x_labels)
  }
  
  # Add common styling
  p <- p + 
    labs(
      title = title,
      subtitle = subtitle,
      x = "",
      y = y_title,
      fill = "",
      caption = caption_text
    ) +
    clessnize::theme_datagotchi_light() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 1, size = 52),
      axis.text.y = element_text(size = 52),
      axis.title.x = element_text(size = 56, margin = margin(t = 30)),
      axis.title.y = element_text(size = 72, face = "bold", margin = margin(r = 30)),
      plot.title = element_text(size = 102, face = "bold", margin = margin(b = 15)),
      plot.subtitle = element_text(size = 52, margin = margin(b = 15), hjust = 0.5),
      plot.caption = element_text(size = 44, hjust = 0, lineheight = 0.3),
      legend.title = element_text(size = 56),
      legend.text = element_text(size = 52),
      legend.key.size = unit(0.5, "in")
    )
  
  # Save with high resolution if path is provided
  if (!is.null(output_path)) {
    ggsave(p,
           filename = output_path,
           width = 16, 
           height = 9,
           dpi = 300,
           units = "in",
           scale = 1)
    
    # Add logos if requested
    if (add_logo && !is.null(logos_list)) {
      clessnize::add_multiple_pngs(
        base_png_path = output_path,
        output_path = gsub("\\.png$", "_final.png", output_path),
        png_list = logos_list
      )
    }
  }
  
  return(p)
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x
