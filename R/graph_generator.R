
#' ---
#' title: Modified Standardized Graph Function
#' description: An R function to create standardized data visualization graphs with customizable captions and removed hardcoded elements, using the original theme.
#' libraries: [dplyr, ggplot2, scales]
#' ---
#' 
#' This script defines an R function `create_standardized_graph` for generating standardized graphs.
#' It allows for creating percentage or difference plots, handling weighted data, filtering,
#' and customization of appearance (colors, labels, order, titles, caption).
#' 
#' Key changes from the initial user-provided version:
#' - Added a `caption` argument for custom captions, with the original logic as default.
#' - Removed hardcoded `party_colors`, `party_mapping`, and `party_order`. Users must provide these externally if needed (e.g., via `colors`, `x_labels`, `x_order`).
#' - Removed the `language` argument and related logic. Language-specific aspects should be handled before calling the function.
#' - Restored the original `theme_datagotchi_light()` and specific `theme()` overrides.

# Required libraries
library(dplyr)
library(ggplot2)
library(scales)

# Null coalescing operator (if not already defined elsewhere)
# Assigns the right-hand side value if the left-hand side is NULL.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Create standardized data visualization graphs
#'
#' @param graph_type String indicating the type of graph: "percentage" or "difference".
#'   - "percentage": Shows percentages of values within each group.
#'   - "difference": Shows difference from overall average for each group.
#' @param data Dataframe containing the data.
#' @param x_variable String with the name of the variable for the x-axis.
#' @param fill_variable String with the name of the variable to use for fill colors.
#' @param weights_variable String with the name of the column containing weights (default: NULL for no weighting).
#' @param filter_values Optional vector of values to include from `fill_variable`.
#' @param x_filter_values Optional vector of values to include from `x_variable`.
#' @param colors Named vector with colors for each value in `fill_variable`. Required if custom colors are desired.
#' @param fill_labels Named vector with display labels for each value in `fill_variable`. Used for the legend.
#' @param x_labels Named vector with display labels for each value in `x_variable`. Used for the x-axis ticks.
#' @param x_order Vector specifying the order of values on the x-axis.
#' @param fill_order Vector specifying the order of values in the fill variable (legend).
#' @param title Graph title (default: NULL).
#' @param subtitle Graph subtitle (default: NULL). A default will be generated based on `graph_type` if NULL.
#' @param y_title Y-axis title (default: NULL). A default will be generated based on `graph_type` if NULL.
#' @param caption Custom caption text (default: NULL). If NULL, a default caption including source and weighting info will be generated.
#' @param output_path Path where to save the graph (default: NULL, graph is not saved).
#' @param add_logo Logical indicating whether to add the Datagotchi logo (requires `logos_list` and saving the plot). Assumes `theme_datagotchi_light` and `add_multiple_pngs` are available.
#' @param logos_list Optional list of PNG logos to add (see add_png.R). Only used if `add_logo` is TRUE and `output_path` is provided.
#'
#' @return A ggplot object representing the created graph.
#' @export
#'
#' @examples
#' # Assuming 'survey_data' is your dataframe
#' # Assuming 'theme_datagotchi_light' and 'add_multiple_pngs' are defined
#' 
#' # Example 1: Percentage graph (unweighted)
#' # create_standardized_graph(
#' #   graph_type = "percentage",
#' #   data = survey_data,
#' #   x_variable = "region",
#' #   fill_variable = "education_level",
#' #   colors = c("High School" = "blue", "College" = "green", "University" = "red"),
#' #   fill_labels = c("High School" = "HS", "College" = "Col", "University" = "Uni"),
#' #   x_order = c("Atlantic", "Quebec", "Ontario", "Prairies", "BC"),
#' #   title = "Education Level by Region",
#' #   y_title = "Percentage (%)"
#' # )
#' 
#' # Example 2: Difference graph (weighted) with custom caption
#' # create_standardized_graph(
#' #   graph_type = "difference",
#' #   data = survey_data,
#' #   x_variable = "dv_voteChoice", # Assuming this is pre-processed
#' #   fill_variable = "age_group",
#' #   weights_variable = "survey_weight",
#' #   colors = c("18-34" = "#FFC300", "35-54" = "#FF5733", "55+" = "#C70039"),
#' #   x_labels = c("PartyA" = "Party A", "PartyB" = "Party B"), # Provide labels
#' #   x_order = c("PartyA", "PartyB", "PartyC"),             # Provide order
#' #   title = "Age Group Representation by Party Support",
#' #   y_title = "Difference from Overall Avg. (pp)",
#' #   caption = "Custom Source Info | Weighted Data | n = calculated_n"
#' # )
create_standardized_graph <- function(
  graph_type = c("percentage", "difference"),
  data,
  x_variable, # Removed default "dv_voteChoice"
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
  caption = NULL, # Added caption argument
  output_path = NULL,
  add_logo = TRUE,
  logos_list = NULL
) {
  # --- Input Validation and Setup ---
  
  # Match the graph_type argument
  graph_type <- match.arg(graph_type)
  
  # Check if essential variables exist in data
  required_vars <- c(x_variable, fill_variable)
  if (!is.null(weights_variable)) {
    required_vars <- c(required_vars, weights_variable)
  }
  missing_vars <- setdiff(required_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("The following required columns are missing from the data: ", 
         paste(missing_vars, collapse = ", "))
  }
  
  # Add weights column (either actual weights or 1s)
  if (!is.null(weights_variable)) {
    # Ensure the weight column is numeric and doesn't have negative values
    if (!is.numeric(data[[weights_variable]])) {
      stop("Weight column '", weights_variable, "' must be numeric.")
    }
    if (any(data[[weights_variable]] < 0, na.rm = TRUE)) {
      warning("Weights column '", weights_variable, "' contains negative values. Ensure weights are valid.")
    }
    # Handle potential NAs in weights - replace with 0? Or filter rows? Here, we keep them but they'll be removed by sum(na.rm=TRUE) later.
    # A common approach is to filter out rows with NA weights if they shouldn't contribute.
    # data <- data %>% filter(!is.na(!!sym(weights_variable))) 
    data$.__weight_col__ <- data[[weights_variable]]
    if (any(is.na(data$.__weight_col__))) {
        warning("Weights column '", weights_variable, "' contains NA values. These rows will contribute 0 to weighted sums where na.rm=TRUE is used.")
    }
  } else {
    # Create a column of 1s for unweighted analysis
    data$.__weight_col__ <- 1
  }
  
  # --- Data Preparation ---
  
  # Select relevant columns and filter NAs in key variables early
  # Filtering NAs in x_variable and fill_variable is crucial for percentage calculations
  # Also filter NAs in weights if they exist and should be excluded (depends on desired handling)
  df_processed <- data %>%
    select(all_of(c(x_variable, fill_variable, ".__weight_col__"))) %>%
    # Filter rows where the grouping/filling variable is NA
    filter(!is.na(!!sym(x_variable)), !is.na(!!sym(fill_variable)))
    # Optional: Filter rows where weight is NA if they should be excluded entirely
    # if (!is.null(weights_variable)) {
    #   df_processed <- df_processed %>% filter(!is.na(.__weight_col__))
    # }

  # Store initial N before filtering (for potential use in default caption)
  # Note: This N might differ from the analysis N if filters are applied
  initial_n <- nrow(data) 
  
  # Apply filter to fill variable if provided
  if (!is.null(filter_values)) {
    df_processed <- df_processed %>%
      filter(!!sym(fill_variable) %in% filter_values)
  }
  
  # Apply filter to x variable if provided
  if (!is.null(x_filter_values)) {
    df_processed <- df_processed %>%
      filter(!!sym(x_variable) %in% x_filter_values)
  }
  
  # Check if data remains after filtering NAs and applying user filters
  if (nrow(df_processed) == 0) {
      stop("No data remaining after applying filters and removing NAs in x/fill variables.")
  }
  
  # --- Calculation Logic ---
  
  # Calculate total weight *within each group* defined by x_variable
  # This is essential for calculating percentages correctly within each x-group
  group_totals <- df_processed %>%
    group_by(!!sym(x_variable)) %>%
    # Use na.rm = TRUE for sum in case weights have NAs
    summarize(total_group_weight = sum(.__weight_col__, na.rm = TRUE), .groups = "drop") %>%
    # Filter out groups with zero or negative total weight (can happen with NAs or odd weights)
    filter(total_group_weight > 0) 

  # Check if any groups remain after calculating totals
  if (nrow(group_totals) == 0) {
      stop("No groups with positive total weight found after filtering.")
  }

  # Calculate weighted counts for each x/fill combination
  group_stats <- df_processed %>%
    # Ensure we only use data corresponding to groups with positive total weight
    inner_join(select(group_totals, !!sym(x_variable)), by = x_variable) %>% 
    group_by(!!sym(x_variable), !!sym(fill_variable)) %>%
    summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE), .groups = "drop")

  # Calculate percentages within each x_variable group
  plot_data_pct <- group_stats %>%
    left_join(group_totals, by = x_variable) %>%
    # Check for division by zero explicitly, though filter(total_group_weight > 0) should prevent it
    mutate(group_pct = if_else(total_group_weight > 0, 
                               (weighted_count / total_group_weight) * 100, 
                               NA_real_)) # Assign NA if total weight is somehow zero

  # --- Plot Specific Calculations & Setup ---
  
  if (graph_type == "difference") {
    # Calculate overall averages (based on the *filtered* data, df_processed)
    # Total weight across all included groups/filters
    total_overall_weight <- sum(df_processed$.__weight_col__, na.rm = TRUE)
    
    if (total_overall_weight <= 0) {
        stop("Total overall weight for calculating averages is zero or negative after filtering.")
    }

    overall_averages <- df_processed %>%
      group_by(!!sym(fill_variable)) %>%
      summarize(total_weighted_count = sum(.__weight_col__, na.rm = TRUE), .groups = "drop") %>%
      mutate(overall_pct = total_weighted_count / total_overall_weight * 100)
      
    # Join overall averages and calculate difference
    plot_data <- plot_data_pct %>%
      left_join(select(overall_averages, !!sym(fill_variable), overall_pct), by = fill_variable) %>%
      mutate(pct_diff_from_overall = group_pct - overall_pct) %>%
      # Select relevant columns for plotting
      select(!!sym(x_variable), !!sym(fill_variable), pct_diff_from_overall)
      
    # Determine plot aesthetics
    y_aes <- "pct_diff_from_overall"
    default_subtitle <- "Difference from overall average (percentage points)" # Generic default
    default_y_title <- "Difference from Average (pp)" # Generic default
    
  } else { # graph_type == "percentage"
    # Use the already calculated group percentages
    plot_data <- plot_data_pct %>%
        select(!!sym(x_variable), !!sym(fill_variable), group_pct)

    # Determine plot aesthetics
    y_aes <- "group_pct"
    default_subtitle <- "Percentage within each group" # Generic default
    default_y_title <- "Percentage (%)" # Generic default
  }

  # --- Factor Ordering ---
  
  # Apply custom ordering for x variable if provided
  if (!is.null(x_order)) {
    # Ensure all values in x_order exist in the data to avoid issues
    present_levels <- intersect(x_order, unique(as.character(plot_data[[x_variable]]))) # Use as.character for comparison robustness
    original_levels <- unique(as.character(plot_data[[x_variable]]))
    levels_not_in_data <- setdiff(x_order, present_levels)
    levels_not_in_order <- setdiff(original_levels, present_levels)

    if (length(levels_not_in_data) > 0){
        warning("Some levels specified in `x_order` are not present in the filtered data for `x_variable`: ", 
                paste(levels_not_in_data, collapse=", "))
    }
    if (length(levels_not_in_order) > 0){
        warning("Some levels present in the data for `x_variable` are not included in `x_order` and will be dropped by factor conversion: ", 
                paste(levels_not_in_order, collapse=", "))
        # Decide if dropping is okay, or if they should be appended to x_order
        # present_levels <- c(present_levels, levels_not_in_order) # Example: append missing levels
    }
    
    if (length(present_levels) > 0) {
       plot_data[[x_variable]] <- factor(plot_data[[x_variable]], levels = present_levels)
    } else {
       warning("None of the levels specified in `x_order` are present in the filtered data for `x_variable`. Ordering ignored.")
       plot_data[[x_variable]] <- factor(plot_data[[x_variable]]) # Default factor conversion
    }
  } else {
      # Default: Convert to factor to ensure consistent plotting order (alphabetical)
      plot_data[[x_variable]] <- factor(plot_data[[x_variable]])
  }
  
  # Apply custom ordering for fill variable if provided
  if (!is.null(fill_order)) {
    present_levels <- intersect(fill_order, unique(as.character(plot_data[[fill_variable]])))
    original_levels <- unique(as.character(plot_data[[fill_variable]]))
    levels_not_in_data <- setdiff(fill_order, present_levels)
    levels_not_in_order <- setdiff(original_levels, present_levels)

     if (length(levels_not_in_data) > 0){
        warning("Some levels specified in `fill_order` are not present in the filtered data for `fill_variable`: ", 
                paste(levels_not_in_data, collapse=", "))
    }
     if (length(levels_not_in_order) > 0){
        warning("Some levels present in the data for `fill_variable` are not included in `fill_order` and will be dropped by factor conversion: ", 
                paste(levels_not_in_order, collapse=", "))
        # present_levels <- c(present_levels, levels_not_in_order) # Example: append missing levels
    }

    if (length(present_levels) > 0) {
        plot_data[[fill_variable]] <- factor(plot_data[[fill_variable]], levels = present_levels)
    } else {
        warning("None of the levels specified in `fill_order` are present in the filtered data for `fill_variable`. Ordering ignored.")
        plot_data[[fill_variable]] <- factor(plot_data[[fill_variable]]) # Default factor conversion
    }
  } else {
      # Default: Convert to factor
      plot_data[[fill_variable]] <- factor(plot_data[[fill_variable]])
  }

  # --- Plot Creation ---
  
  p <- ggplot(plot_data, aes(x = !!sym(x_variable), 
                             y = !!sym(y_aes), 
                             fill = !!sym(fill_variable))) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single")) # preserve="single" keeps bar width consistent
  
  # Add horizontal line for difference plots
  if (graph_type == "difference") {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.5)
  }
  
  # --- Plot Styling and Labels ---
  
  # Apply color scale and labels
  # Use fill_labels for the legend if provided, otherwise use the factor levels from the data
  active_fill_levels <- levels(plot_data[[fill_variable]]) # Get levels *after* factor conversion

  # Prepare legend labels: Use provided fill_labels if available, matching the active levels
  if (!is.null(fill_labels)) {
      # Ensure fill_labels is a named vector/list
      if (is.null(names(fill_labels))) {
          warning("`fill_labels` provided but is not named. Using factor levels instead.")
          legend_labels_to_use <- waiver() # Use default labels
      } else {
          # Create labels only for levels present in the data, maintaining the order of active_fill_levels
          legend_labels_to_use <- setNames(fill_labels[active_fill_levels], active_fill_levels)
          # Check if any labels were missing for active levels
          missing_labels <- active_fill_levels[!active_fill_levels %in% names(fill_labels)]
          if(length(missing_labels) > 0) {
              warning("No label provided in `fill_labels` for the following levels present in the data: ", 
                      paste(missing_labels, collapse=", "), ". Using original values as labels for these.")
              # Optionally, fill missing labels with the original level names
              names(missing_labels) <- missing_labels
              legend_labels_to_use[missing_labels] <- missing_labels 
          }
      }
  } else {
      legend_labels_to_use <- waiver() # Use default labels (factor levels)
  }

  # Apply colors if provided
  if (!is.null(colors)) {
      # Ensure colors is a named vector/list
      if (is.null(names(colors))) {
          warning("`colors` provided but is not named. Using default ggplot colors.")
          p <- p + scale_fill_discrete(labels = legend_labels_to_use, 
                                     name = "", # Legend title often kept blank
                                     drop = FALSE) # Keep all levels in legend even if data is missing for some combination
      } else {
          # Ensure colors match the levels of the fill factor
          colors_to_use <- colors[active_fill_levels] # Select colors for active levels in the correct order
          names(colors_to_use) <- active_fill_levels # Ensure names match factor levels
          
          missing_colors <- active_fill_levels[!active_fill_levels %in% names(colors)]
           if(length(missing_colors) > 0) {
              warning("No color provided in `colors` for the following levels present in the data: ", 
                      paste(missing_colors, collapse=", "), ". Default colors will be used for these.")
              # ggplot will handle missing colors by cycling defaults, which might be acceptable.
              # Or, you could assign a default color like grey:
              # colors_to_use[is.na(colors_to_use)] <- "grey80" 
          }
          
          p <- p + scale_fill_manual(values = colors_to_use, 
                                     labels = legend_labels_to_use, 
                                     name = "", 
                                     drop = FALSE, # Keep all defined levels in legend
                                     na.value = "grey80") # Color for any unexpected NA values in fill variable
      }
  } else {
      # Use default ggplot colors if no colors provided
      p <- p + scale_fill_discrete(labels = legend_labels_to_use, 
                                   name = "",
                                   drop = FALSE)
  }
  
  # Apply x-axis labels if provided
  # Prepare x-axis labels similar to legend labels
  active_x_levels <- levels(plot_data[[x_variable]])
  if (!is.null(x_labels)) {
      if (is.null(names(x_labels))) {
          warning("`x_labels` provided but is not named. Using factor levels instead.")
          x_labels_to_use <- waiver()
      } else {
          x_labels_to_use <- setNames(x_labels[active_x_levels], active_x_levels)
          missing_x_labels <- active_x_levels[!active_x_levels %in% names(x_labels)]
           if(length(missing_x_labels) > 0) {
              warning("No label provided in `x_labels` for the following levels present on the x-axis: ", 
                      paste(missing_x_labels, collapse=", "), ". Using original values as labels for these.")
              names(missing_x_labels) <- missing_x_labels
              x_labels_to_use[missing_x_labels] <- missing_x_labels 
          }
      }
      p <- p + scale_x_discrete(labels = x_labels_to_use, drop = FALSE) # Use factor levels for axis
  } else {
      p <- p + scale_x_discrete(drop = FALSE) # Ensure all factor levels are shown, use default labels
  }

  # Apply y-axis formatting (percentage or percentage points)
  if (graph_type == "percentage") {
      # Ensure y-axis starts at 0 for percentage plots
      p <- p + scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, NA)) # y-values are already 0-100
  } else { # difference graph
      p <- p + scale_y_continuous(labels = scales::label_number(suffix = " pp")) # Add " pp" suffix
  }

  # Generate default caption if none provided
  final_caption <- caption %||% {
      caption_weight_text <- if (is.null(weights_variable)) {
          " | Unweighted data" 
      } else {
          "\nData weighted" # Keep it simple, details can be added in report text
      }
      # Calculate N based on the *processed* dataframe for accuracy regarding filters/NAs removed
      # This N represents the sample size used in the analysis shown.
      analysis_n <- nrow(df_processed) 
      paste0("Source: Léger-Datagotchi 2025 | n = ", analysis_n, caption_weight_text) 
  }

  # Add common styling and labels using the original theme
  p <- p + 
    labs(
      title = title,
      subtitle = subtitle %||% default_subtitle, # Use default if NULL
      x = "", # Typically remove x-axis title for categorical data
      y = y_title %||% default_y_title, # Use default if NULL
      fill = "", # Legend title - often blank
      caption = final_caption
    ) +
    theme_datagotchi_light() + # Use the specified theme function
    theme( # Apply specific overrides from the original function
      axis.text.x = element_text(angle = 0, hjust = 1, size = 52), # Original: hjust=1
      axis.text.y = element_text(size = 52),
      axis.title.x = element_text(size = 56, margin = margin(t = 30)),
      axis.title.y = element_text(size = 72, face = "bold", margin = margin(r = 30)),
      plot.title = element_text(size = 102, face = "bold", margin = margin(b = 15), hjust = 0.5), # Keep centered title? Original didn't specify hjust.
      plot.subtitle = element_text(size = 52, margin = margin(b = 15), hjust = 0.5),
      plot.caption = element_text(size = 44, hjust = 0, lineheight = 0.3), # Original lineheight
      legend.title = element_text(size = 56),
      legend.text = element_text(size = 52),
      legend.key.size = unit(0.5, "in") # Original size unit
      # Note: theme_datagotchi_light() might already set some of these. Overrides here take precedence.
    )
  
  # --- Save Plot (Optional) ---
  
  # Save with high resolution if path is provided
  if (!is.null(output_path)) {
    # Ensure directory exists if specified in path
    dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
    
    ggsave(plot = p, # Explicitly pass the plot object
           filename = output_path,
           width = 16, 
           height = 9,
           dpi = 300,
           units = "in",
           scale = 1,
           bg = "white") # Add background for non-transparent save
           
    message("Plot saved to: ", output_path)
           
    # Add logos if requested (assuming add_multiple_pngs function exists)
    if (add_logo && !is.null(logos_list)) {
        if (exists("add_multiple_pngs") && is.function(add_multiple_pngs)) {
            output_final_path <- gsub("\\.png$", "_final.png", output_path) # Basic final name
            # Ensure the function call matches the definition of add_multiple_pngs
            tryCatch({
                 add_multiple_pngs( 
                     base_png_path = output_path, 
                     output_path = output_final_path, 
                     png_list = logos_list
                 )
                 message("Logos added to: ", output_final_path)
            }, error = function(e) {
                 warning("Failed to add logos. Error in add_multiple_pngs: ", e$message)
            })
        } else {
             warning("`add_logo` is TRUE, but the function `add_multiple_pngs` was not found or is not a function. Logos not added.")
        }
    } else if (add_logo && is.null(logos_list)) {
        warning("`add_logo` is TRUE, but `logos_list` is NULL. Logos not added.")
    }
  }
  
  # --- Return Plot Object ---
  
  return(p)
}

