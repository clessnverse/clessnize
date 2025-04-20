library(dplyr)
library(ggplot2)
library(scales)

#' Create standardized data visualization graphs
#'
#' @param graph_type String indicating the type of graph: "percentage", "difference", "percentage_by_fill", or "difference_by_x"
#'   - "percentage": Shows percentages of values within each group on the x-axis
#'   - "difference": Shows difference from national average for each group
#'   - "percentage_by_fill": Shows percentages of values within each fill category (bars for a fill category across all x-categories sum to 100%)
#'   - "difference_by_x": Shows difference from national average of a numeric variable for each x-category
#' @param data Dataframe containing the data
#' @param x_variable String with the name of the variable for the x-axis (default: "dv_voteChoice")
#' @param fill_variable String with the name of the variable to use for fill colors in "percentage", "difference", and "percentage_by_fill" modes.
#'      In "difference_by_x" mode, this parameter specifies the numeric variable to analyze.
#' @param weights_variable String with the name of the column containing weights (default: NULL for no weighting)
#' @param filter_values Optional vector of values to include from fill_variable
#' @param x_filter_values Optional vector of values to include from x_variable
#' @param language "fr" or "en" for output language
#' @param colors Named vector with colors for each value in fill_variable (or in x_variable for "difference_by_x" mode)
#' @param fill_labels Named vector with display labels for each value in fill_variable
#' @param x_labels Named vector with display labels for each value in x_variable
#' @param x_order Vector specifying the order of values on the x-axis
#' @param fill_order Vector specifying the order of values in the fill variable
#' @param title Graph title
#' @param subtitle Graph subtitle
#' @param y_title Y-axis title
#' @param custom_caption String to completely override the default caption
#' @param add_caption_line String to add as an additional line to the default caption
#' @param output_path Path where to save the graph
#' @param add_logo Logical indicating whether to add the Datagotchi logo
#' @param logos_list Optional list of PNG logos to add (see add_png.R)
#'
#' @return A ggplot object
#' @export
#' 
#' @importFrom dplyr filter group_by summarize mutate left_join case_when select
#' @importFrom ggplot2 ggplot aes geom_bar geom_hline scale_fill_manual scale_x_discrete labs theme element_text margin unit ggsave
#' @importFrom scales percent
#' @importFrom stats weighted.mean
#' @importFrom rlang sym !!
create_standardized_graph <- function(
  graph_type = c("percentage", "difference", "percentage_by_fill", "difference_by_x"),
  data,
  x_variable = "dv_voteChoice",
  fill_variable,
  weights_variable = NULL,  # Changed default to NULL for no weighting
  filter_values = NULL,
  x_filter_values = NULL,
  language = "fr",
  colors = NULL,
  fill_labels = NULL,
  x_labels = NULL,
  x_order = NULL,
  fill_order = NULL,
  title = NULL,
  subtitle = NULL,
  y_title = NULL,
  custom_caption = NULL,  # Parameter to override the default caption
  add_caption_line = NULL,  # Parameter to add a line to the default caption
  output_path = NULL,
  add_logo = TRUE,
  logos_list = NULL
) {
  # Match the graph_type argument
  graph_type <- match.arg(graph_type)
  
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
  
  # Modify caption based on whether weights are used
  caption_weight_text <- if(is.null(weights_variable)) {
    ""  # No weighting text
  } else {
    if(language == "fr") {
      "\nDonnées pondérées selon le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, le status d'immigrant et le type d'habitation"
    } else {
      "\nData weighted by gender, age, province, language, education level, income, immigration status, and housing type"
    }
  }
  
  # Default caption text based on language
  default_caption <- if(language == "fr") {
    paste0("Source : Léger-Datagotchi 2025 | n = ", nrow(data), caption_weight_text)
  } else {
    paste0("Source: Léger-Datagotchi 2025 | n = ", nrow(data), caption_weight_text)
  }
  
  # Determine final caption: custom override, add a line, or use default
  caption_text <- if (!is.null(custom_caption)) {
    # Option 1: Completely override with custom caption
    custom_caption
  } else if (!is.null(add_caption_line)) {
    # Option 2: Add a line to the default caption
    paste0(default_caption, "\n", add_caption_line)
  } else {
    # Option 3: Use default caption as is
    default_caption
  }
  
  # Add weights column if specified
  if (!is.null(weights_variable)) {
    # Check if weights column exists
    if (!weights_variable %in% colnames(data)) {
      stop(paste0("Weight column '", weights_variable, "' not found in data"))
    }
    
    # Create a weight column reference
    data$.__weight_col__ <- data[[weights_variable]]
  } else {
    # Create a column of 1s for unweighted analysis
    data$.__weight_col__ <- 1
  }
  
  # Filter and prepare data
  df_full <- data %>%
    select(all_of(c(x_variable, fill_variable, ".__weight_col__")))
  
  # Apply filter to fill variable if provided
  if (!is.null(filter_values)) {
    df_full <- df_full %>%
      filter(!!sym(fill_variable) %in% filter_values)
  }
  
  # Apply filter to x variable if provided
  if (!is.null(x_filter_values)) {
    df_full <- df_full %>%
      filter(!!sym(x_variable) %in% x_filter_values)
  }
  
  # Check if x_variable is party/vote choice
  is_party_graph <- x_variable == "dv_voteChoice"
  
  # Process based on graph type
  if (graph_type == "difference_by_x") {
    # NEW GRAPH TYPE: Difference from national average for each x group on a single Y variable
    
    # For this graph type, the fill_variable is treated as the Y variable
    y_variable <- fill_variable
    
    # Check if y_variable is numeric and between 0 and 1
    if (!is.numeric(df_full[[y_variable]])) {
      warning(paste0("The Y variable (", y_variable, ") is not numeric. This graph type requires a numeric variable."))
    } else {
      # Check if values are between 0 and 1
      y_min <- min(df_full[[y_variable]], na.rm = TRUE)
      y_max <- max(df_full[[y_variable]], na.rm = TRUE)
      
      if (y_min < 0 || y_max > 1) {
        warning(paste0("The Y variable (", y_variable, ") contains values outside the 0-1 range. For best results, the variable should be coded between 0 and 1."))
      }
    }
    
    # Calculate national average of y_variable
    df_national <- df_full %>%
      filter(!is.na(!!sym(y_variable))) %>%
      filter(!is.na(!!sym(x_variable)))
    
    national_average <- weighted.mean(df_national[[y_variable]], 
                                       df_national$.__weight_col__, 
                                       na.rm = TRUE)
    
    # Calculate group averages
    df_groups <- df_full %>%
      filter(!is.na(!!sym(x_variable))) %>%
      filter(!is.na(!!sym(y_variable)))
    
    # Add party-specific filter if relevant (and no custom x filter provided)
    if(is_party_graph && is.null(x_filter_values)) {
      df_groups <- df_groups %>% filter(!!sym(x_variable) != "other")
    }
    
    # Calculate weighted average for each x group
    group_averages <- df_groups %>%
      group_by(!!sym(x_variable)) %>%
      summarize(
        weighted_average = weighted.mean(!!sym(y_variable), .__weight_col__, na.rm = TRUE),
        total_group_weight = sum(.__weight_col__, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Calculate difference from national average
    plot_data <- group_averages %>%
      mutate(
        diff_from_national = weighted_average - national_average,
        # Convert to percentage points if the variable is between 0-1
        diff_from_national_pct = diff_from_national * 100
      )
    
    # Store the original x values before any transformations for color mapping
    plot_data$original_x <- plot_data[[x_variable]]
    
    # Apply custom ordering for x variable if provided - do this BEFORE any transformations
    if (!is.null(x_order)) {
      # First, factor the original values to maintain order
      plot_data$original_x <- factor(plot_data$original_x, levels = x_order)
      # Then convert back to character to avoid issues with later transformations
      plot_data$original_x <- as.character(plot_data$original_x)
      # Also order the x variable itself
      plot_data[[x_variable]] <- factor(plot_data[[x_variable]], levels = x_order)
      # Convert back to character for party mapping
      plot_data[[x_variable]] <- as.character(plot_data[[x_variable]])
    }
    
    # If x_variable is party choice, handle party mapping
    if (is_party_graph) {
      # Replace party abbreviations
      plot_data <- plot_data %>%
        mutate(!!sym(x_variable) := case_when(
          !!sym(x_variable) %in% names(party_mapping[[language]]) ~ 
            party_mapping[[language]][as.character(!!sym(x_variable))],
          TRUE ~ as.character(!!sym(x_variable))
        ))
      
      # Set default order for party names (if x_order not specified)
      if (is.null(x_order)) {
        plot_data[[x_variable]] <- factor(plot_data[[x_variable]], 
                                          levels = party_order[[language]])
      } else {
        # If x_order is provided, we need to map it to the display values
        mapped_order <- sapply(x_order, function(x) {
          if (x %in% names(party_mapping[[language]])) {
            return(party_mapping[[language]][x])
          } else {
            return(x)
          }
        })
        plot_data[[x_variable]] <- factor(plot_data[[x_variable]], levels = mapped_order)
      }
    } else {
      # For non-party graphs, make sure x is properly factored if x_order was provided
      if (!is.null(x_order)) {
        plot_data[[x_variable]] <- factor(plot_data[[x_variable]], levels = x_order)
      }
    }
    
    # Create the base plot with position for the bars
    p <- ggplot(plot_data, aes(x = !!sym(x_variable), 
                               y = diff_from_national_pct)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black")
    
    # Apply colors mapping to the original x values
    if (!is.null(colors)) {
      # Map the colors based on the original x values (before transformation)
      # --- MODIFIED HERE ---
      p <- p + geom_bar(stat = "identity", 
                        position = "dodge",
                        aes(fill = original_x),
                        width = 0.6) + # Added width argument
            scale_fill_manual(values = colors, guide = "none")
    } else {
      # Default blue color if no colors provided
      # --- MODIFIED HERE ---
      p <- p + geom_bar(stat = "identity", 
                        position = "dodge",
                        fill = "#1A4782",
                        width = 0.6) # Added width argument
    }
    
    # Default subtitle if not provided
    if (is.null(subtitle)) {
      subtitle <- if(language == "fr") {
        paste0("Écart par rapport à la moyenne canadienne (", names(data)[names(data) == y_variable], ")")
      } else {
        paste0("Difference from Canadian average (", names(data)[names(data) == y_variable], ")")
      }
    }
    
  } else if (graph_type == "difference") {
    # Difference from national average for each group
    
    # Calculate national averages
    df_national <- df_full %>%
      filter(!is.na(!!sym(fill_variable)))
    
    total_national_weight <- sum(df_national$.__weight_col__, na.rm = TRUE)
    
    national_averages <- df_national %>%
      group_by(!!sym(fill_variable)) %>%
      summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE)) %>%
      mutate(national_pct = weighted_count / total_national_weight * 100)
    
    # Calculate group-specific percentages
    df_groups <- df_full %>%
      filter(!is.na(!!sym(x_variable))) %>%
      filter(!is.na(!!sym(fill_variable)))
    
    # Add party-specific filter if relevant (and no custom x filter provided)
    if(is_party_graph && is.null(x_filter_values)) {
      df_groups <- df_groups %>% filter(!!sym(x_variable) != "other")
    }
    
    # Calculate total weight for each x_variable group first
    group_totals <- df_groups %>%
      group_by(!!sym(x_variable)) %>%
      summarize(total_group_weight = sum(.__weight_col__, na.rm = TRUE))
    
    group_stats <- df_groups %>%
      group_by(!!sym(x_variable), !!sym(fill_variable)) %>%
      summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE), .groups = "drop") %>%
      left_join(group_totals, by = x_variable) %>%
      mutate(group_pct = weighted_count / total_group_weight * 100)
    
    # Create plot data with difference from national
    plot_data <- group_stats %>%
      left_join(national_averages, by = fill_variable) %>%
      mutate(pct_diff_from_national = group_pct - national_pct)
    
    # If x_variable is party choice, handle party mapping
    if (is_party_graph) {
      # Replace party abbreviations
      plot_data <- plot_data %>%
        mutate(!!sym(x_variable) := case_when(
          !!sym(x_variable) %in% names(party_mapping[[language]]) ~ 
            party_mapping[[language]][as.character(!!sym(x_variable))],
          TRUE ~ as.character(!!sym(x_variable))
        ))
      
      # Set default order for party names (if x_order not specified)
      if (is.null(x_order)) {
        plot_data[[x_variable]] <- factor(plot_data[[x_variable]], 
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
    p <- ggplot(plot_data, aes(x = !!sym(x_variable), 
                               y = pct_diff_from_national, 
                               fill = !!sym(fill_variable))) +
      geom_bar(stat = "identity", position = "dodge") + # No width change here
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
    
    # First filter out NAs
    df_groups <- df_full %>%
      filter(!is.na(!!sym(x_variable))) %>%
      filter(!is.na(!!sym(fill_variable)))
    
    # Add party-specific filter if relevant (and no custom x filter provided)
    if(is_party_graph && is.null(x_filter_values)) {
      df_groups <- df_groups %>% filter(!!sym(x_variable) != "other")
    }
    
    # Calculate total weight for each x_variable group
    group_totals <- df_groups %>%
      group_by(!!sym(x_variable)) %>%
      summarize(total_group_weight = sum(.__weight_col__, na.rm = TRUE))
    
    # Calculate weighted percentages for each x_variable/fill_variable combination
    plot_data <- df_groups %>%
      group_by(!!sym(x_variable), !!sym(fill_variable)) %>%
      summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE), .groups = "drop") %>%
      left_join(group_totals, by = x_variable) %>%
      mutate(group_pct = weighted_count / total_group_weight * 100)
    
    # If x_variable is party choice, handle party mapping
    if (is_party_graph) {
      # Replace party abbreviations
      plot_data <- plot_data %>%
        mutate(!!sym(x_variable) := case_when(
          !!sym(x_variable) %in% names(party_mapping[[language]]) ~ 
            party_mapping[[language]][as.character(!!sym(x_variable))],
          TRUE ~ as.character(!!sym(x_variable))
        ))
      
      # Set default order for party names (if x_order not specified)
      if (is.null(x_order)) {
        plot_data[[x_variable]] <- factor(plot_data[[x_variable]], 
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
    p <- ggplot(plot_data, aes(x = !!sym(x_variable), 
                               y = group_pct, 
                               fill = !!sym(fill_variable))) +
      geom_bar(stat = "identity", position = "dodge") # No width change here
    
    # Default subtitle if not provided
    if (is.null(subtitle)) {
      subtitle <- if(language == "fr") {
        "Pourcentage au sein de chaque groupe"
      } else {
        "Percentage within each group"
      }
    }
  } else if (graph_type == "percentage_by_fill") {
    # Percentages by fill variable (distribution of x variables within each fill group)
    
    # First filter out NAs
    df_groups <- df_full %>%
      filter(!is.na(!!sym(x_variable))) %>%
      filter(!is.na(!!sym(fill_variable)))
    
    # Add party-specific filter if relevant (and no custom x filter provided)
    if(is_party_graph && is.null(x_filter_values)) {
      df_groups <- df_groups %>% filter(!!sym(x_variable) != "other")
    }
    
    # Calculate total weight for each fill_variable group
    fill_totals <- df_groups %>%
      group_by(!!sym(fill_variable)) %>%
      summarize(total_fill_weight = sum(.__weight_col__, na.rm = TRUE))
    
    # Calculate weighted percentages for each x_variable within each fill_variable
    plot_data <- df_groups %>%
      group_by(!!sym(x_variable), !!sym(fill_variable)) %>%
      summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE), .groups = "drop") %>%
      left_join(fill_totals, by = fill_variable) %>%
      mutate(fill_pct = weighted_count / total_fill_weight * 100)
    
    # If x_variable is party choice, handle party mapping
    if (is_party_graph) {
      # Replace party abbreviations
      plot_data <- plot_data %>%
        mutate(!!sym(x_variable) := case_when(
          !!sym(x_variable) %in% names(party_mapping[[language]]) ~ 
            party_mapping[[language]][as.character(!!sym(x_variable))],
          TRUE ~ as.character(!!sym(x_variable))
        ))
      
      # Set default order for party names (if x_order not specified)
      if (is.null(x_order)) {
        plot_data[[x_variable]] <- factor(plot_data[[x_variable]], 
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
    p <- ggplot(plot_data, aes(x = !!sym(x_variable), 
                               y = fill_pct, 
                               fill = !!sym(fill_variable))) +
      geom_bar(stat = "identity", position = "dodge") # No width change here
    
    # Default subtitle if not provided
    if (is.null(subtitle)) {
      subtitle <- if(language == "fr") {
        "Pourcentage au sein de chaque catégorie de remplissage"
      } else {
        "Percentage within each fill category"
      }
    }
    
    # Default y-axis label if not provided
    if (is.null(y_title)) {
      y_title <- if(language == "fr") {
        "Pourcentage (%)"
      } else {
        "Percentage (%)"
      }
    }
  }
  
  # Apply color scale and labels
  if (!is.null(colors)) {
    # For difference_by_x, the colors are already applied in the graph_type block
    if (graph_type != "difference_by_x") {
      # Use fill_labels if provided, otherwise use names from colors
      if(is.null(fill_labels)) {
        fill_labels <- names(colors)
      }
      p <- p + scale_fill_manual(values = colors, labels = fill_labels)
    }
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
    theme_datagotchi_light() + # Assuming theme_datagotchi_light exists
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12), # Adjusted hjust and size
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, margin = margin(t = 10)), # Adjusted size/margin
      axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)), # Adjusted size/margin
      plot.title = element_text(size = 18, face = "bold", margin = margin(b = 10)), # Adjusted size/margin
      plot.subtitle = element_text(size = 12, margin = margin(b = 10), hjust = 0.5), # Adjusted size/margin
      plot.caption = element_text(size = 10, hjust = 0, lineheight = 1.2), # Adjusted size/lineheight
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.key.size = unit(0.8, "cm") # Adjusted size
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
    
    # Add logos if requested and function exists
    if (add_logo && !is.null(logos_list) && exists("add_multiple_pngs")) {
      add_multiple_pngs(
        base_png_path = output_path,
        output_path = gsub("\\.png$", "_final.png", output_path),
        png_list = logos_list
      )
    } else if (add_logo && !exists("add_multiple_pngs")) {
        warning("add_multiple_pngs function not found. Logos will not be added.")
    }
  }
  
  return(p)
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# Placeholder for the theme function if it's not defined elsewhere
# theme_datagotchi_light <- function(...) { theme_minimal(...) + theme(...) }
