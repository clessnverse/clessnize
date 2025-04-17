library(dplyr)
library(ggplot2)
library(scales)

#' Create standardized data visualization graphs
#'
#' @param graph_type String indicating the type of graph: "percentage" or "difference"
#'   - "percentage": Shows percentages of values within each group
#'   - "difference": Shows difference from national average for each group
#' @param data Dataframe containing the data
#' @param x_variable String with the name of the variable for the x-axis (default: "dv_voteChoice")
#' @param fill_variable String with the name of the variable to use for fill colors
#' @param weights_variable String with the name of the column containing weights (default: NULL for no weighting)
#' @param filter_values Optional vector of values to include from fill_variable
#' @param x_filter_values Optional vector of values to include from x_variable
#' @param colors Named vector with colors for each value in fill_variable
#' @param fill_labels Named vector with display labels for each value in fill_variable
#' @param x_labels Named vector with display labels for each value in x_variable
#' @param x_order Vector specifying the order of values on the x-axis
#' @param fill_order Vector specifying the order of values in the fill variable
#' @param title Graph title
#' @param subtitle Graph subtitle
#' @param y_title Y-axis title
#' @param caption Custom caption text (NULL for default caption)
#' @param caption_weight_text Custom weighting text for caption (NULL for default text)
#' @param source_text Source text for caption (default: "Léger-Datagotchi 2025")
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
  weights_variable = NULL,
  filter_values = NULL,
  x_filter_values = NULL,
  colors = NULL,
  fill_labels = NULL,
  x_labels = NULL,
  x_order = NULL,
  fill_order = NULL,
  title = NULL,
  subtitle = NULL,
  y_title = NULL,
  caption = NULL,
  caption_weight_text = NULL,
  source_text = "Léger-Datagotchi 2025",
  output_path = NULL,
  add_logo = TRUE,
  logos_list = NULL
) {
  # Match the graph_type argument
  graph_type <- match.arg(graph_type)
  
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
  
  # Generate default caption weight text if not provided but weights are used
  if (is.null(caption_weight_text) && !is.null(weights_variable)) {
    caption_weight_text <- "\nData weighted by gender, age, province, language, education level, income, immigration status, and housing type"
  } else if (is.null(caption_weight_text)) {
    caption_weight_text <- ""  # No weighting text
  }
  
  # Generate default caption if not provided
  if (is.null(caption)) {
    caption <- paste0("Source: ", source_text, " | n = ", nrow(data), caption_weight_text)
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
  if (graph_type == "difference") {
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
      geom_bar(stat = "identity", position = "dodge") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black")
    
    # Default subtitle if not provided
    if (is.null(subtitle)) {
      subtitle <- "Difference from Canadian average (percentage points)"
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
      geom_bar(stat = "identity", position = "dodge")
    
    # Default subtitle if not provided
    if (is.null(subtitle)) {
      subtitle <- "Percentage within each group"
    }
  }
  
  # Set default y-title if not provided
  y_title <- y_title %||% "Canadian average"
  
  # Apply color scale and labels
  if (!is.null(colors)) {
    # Use fill_labels if provided, otherwise use names from colors
    if(is.null(fill_labels)) {
      fill_labels <- names(colors)
    }
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
      caption = caption
    ) +
    theme_datagotchi_light() +
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
      add_multiple_pngs(
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
