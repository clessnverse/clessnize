# Load necessary packages
library(dplyr)
library(ggplot2)
library(scales)
library(rlang)
library(ggtext)
library(magick)

#' Create standardized data visualization graphs
#'
#' @param graph_type String indicating the type of graph: "percentage",
#'   "difference", "percentage_by_fill", or "difference_by_x".
#' @param data Dataframe containing the data.
#' @param x_variable String with the name of the variable for the x-axis
#'   (default: "dv_voteChoice").
#' @param fill_variable String with the name of the variable to use for fill
#'   colors or the numeric variable for "difference_by_x".
#' @param weights_variable String with the name of the column containing weights
#'   (default: NULL).
#' @param filter_values Optional vector of values to include from fill_variable.
#' @param x_filter_values Optional vector of values to include from x_variable.
#' @param language "fr" or "en" for output language.
#' @param colors Named vector with colors for each value in fill_variable
#'   (or x_variable for "difference_by_x").
#' @param fill_labels Named vector with display labels for each value in
#'   fill_variable.
#' @param x_labels Named vector with display labels for each value in x_variable.
#'   **Can contain paths to PNG files (ending in .png) instead of text labels.**
#' @param x_order Vector specifying the order of values on the x-axis.
#' @param fill_order Vector specifying the order of values in the fill variable.
#' @param title Graph title.
#' @param subtitle Graph subtitle.
#' @param y_title Y-axis title.
#' @param custom_caption String to completely override the default caption.
#' @param add_caption_line String to add as an additional line to the default
#'   caption.
#' @param output_path Path where to save the graph.
#' @param add_logo Logical indicating whether to add the Datagotchi logo.
#' @param logos_list Optional list of PNG logos to add (see add_png.R).
#' @param png_label_scale Numeric scaling factor for PNG labels on the x-axis
#'   (default: 1.0 for original size). Applied to image height, width adjusts
#'   automatically.
#'
#' @return A ggplot object.
#' @export
#'
#' @importFrom dplyr filter group_by summarize mutate left_join case_when select
#'   all_of
#' @importFrom ggplot2 ggplot aes geom_bar geom_hline scale_fill_manual
#'   scale_x_discrete labs theme element_text margin unit ggsave waiver
#' @importFrom scales percent
#' @importFrom stats weighted.mean
#' @importFrom rlang sym !! := `%||%`
#' @importFrom ggtext element_markdown
#' @importFrom magick image_read image_info
create_standardized_graph <- function(graph_type = c("percentage",
                                                    "difference",
                                                    "percentage_by_fill",
                                                    "difference_by_x"),
                                      data,
                                      x_variable = "dv_voteChoice",
                                      fill_variable,
                                      weights_variable = NULL,
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
                                      custom_caption = NULL,
                                      add_caption_line = NULL,
                                      output_path = NULL,
                                      add_logo = TRUE,
                                      logos_list = NULL,
                                      png_label_scale = 1.0) {
  # Match the graph_type argument
  graph_type <- match.arg(graph_type)

  # --- Define party mappings and order ---
  party_colors <- c(
    "PCC" = "#1A4782",
    "PLC" = "#D71920",
    "BQ" = "#33B2CC",
    "NPD" = "#F58220",
    "PVC" = "#3D9B35"
  )
  party_mapping <- list(
    "fr" = c(
      "lpc" = "PLC", "cpc" = "PCC", "ndp" = "NPD",
      "bq" = "BQ", "gpc" = "PVC"
    ),
    "en" = c(
      "lpc" = "LPC", "cpc" = "CPC", "ndp" = "NDP",
      "bq" = "BQ", "gpc" = "GPC"
    )
  )
  party_order <- list(
    "fr" = c("PLC", "PCC", "BQ", "NPD", "PVC"),
    "en" = c("LPC", "CPC", "BQ", "NDP", "GPC")
  )

  # --- Reverse mapping for label lookup ---
  reverse_party_mapping <- NULL
  if (x_variable == "dv_voteChoice" && !is.null(party_mapping[[language]])) {
    reverse_party_mapping <- setNames(
      names(party_mapping[[language]]),
      party_mapping[[language]]
    )
  }

  # --- Initial setup (weights, captions, y_title) ---
  default_y_title <- if (language == "fr") {
    "Moyenne canadienne"
  } else {
    "Canadian average"
  }
  y_title <- y_title %||% default_y_title

  caption_weight_text <- if (is.null(weights_variable)) {
    ""
  } else {
    if (language == "fr") {
      paste(
        "\nDonnées pondérées selon le genre, l'âge, la province,",
        "la langue, le niveau d'éducation, le revenu,",
        "le status d'immigrant et le type d'habitation"
      )
    } else {
      paste(
        "\nData weighted by gender, age, province, language,",
        "education level, income, immigration status,",
        "and housing type"
      )
    }
  }

  current_year <- format(Sys.Date(), "%Y") # Use current year dynamically
  default_caption <- if (language == "fr") {
    paste0(
      "Source : Léger-Datagotchi ", current_year,
      " | n = ", nrow(data), caption_weight_text
    )
  } else {
    paste0(
      "Source: Léger-Datagotchi ", current_year,
      " | n = ", nrow(data), caption_weight_text
    )
  }

  caption_text <- custom_caption %||% {
    if (!is.null(add_caption_line)) {
      paste0(default_caption, "\n", add_caption_line)
    } else {
      default_caption
    }
  }

  if (!is.null(weights_variable)) {
    if (!weights_variable %in% colnames(data)) {
      stop(paste0("Weight column '", weights_variable, "' not found"))
    }
    data$.__weight_col__ <- data[[weights_variable]]
  } else {
    data$.__weight_col__ <- 1
  }

  # --- Filter and select data ---
  req_vars <- unique(c(
    x_variable,
    fill_variable,
    if (!is.null(weights_variable)) {
      c(weights_variable, ".__weight_col__")
    } else {
      ".__weight_col__"
    }
  ))

  df_full <- data %>%
    select(all_of(req_vars))

  if (!is.null(filter_values)) {
    df_full <- df_full %>%
      filter(!!sym(fill_variable) %in% filter_values)
  }
  if (!is.null(x_filter_values)) {
    df_full <- df_full %>%
      filter(!!sym(x_variable) %in% x_filter_values)
  }

  is_party_graph <- x_variable == "dv_voteChoice"

  # --- Data processing based on graph type ---
  if (graph_type == "difference_by_x") {
    y_variable <- fill_variable
    df_national <- df_full %>%
      filter(!is.na(!!sym(y_variable)), !is.na(!!sym(x_variable)))

    if (!is.numeric(df_national[[y_variable]])) {
      warning(paste0("Y variable (", y_variable, ") not numeric"))
    }

    national_average <- weighted.mean(
      df_national[[y_variable]],
      df_national$.__weight_col__,
      na.rm = TRUE
    )

    df_groups <- df_full %>%
      filter(!is.na(!!sym(x_variable)), !is.na(!!sym(y_variable)))

    if (is_party_graph && is.null(x_filter_values)) {
      df_groups <- df_groups %>%
        filter(!!sym(x_variable) != "other")
    }

    group_averages <- df_groups %>%
      group_by(!!sym(x_variable)) %>%
      summarize(
        weighted_average = weighted.mean(
          !!sym(y_variable), .__weight_col__,
          na.rm = TRUE
        ),
        .groups = "drop"
      )

    plot_data <- group_averages %>%
      mutate(
        diff_from_national = weighted_average - national_average,
        diff_from_national_pct = diff_from_national * 100
      )

    plot_data$original_x <- plot_data[[x_variable]]

    if (is.null(subtitle)) {
      y_var_name <- fill_variable
      subtitle <- if (language == "fr") {
        paste0("Écart par rapport à la moyenne canadienne (", y_var_name, ")")
      } else {
        paste0("Difference from Canadian average (", y_var_name, ")")
      }
    }
  } else if (graph_type == "difference") {
    df_national <- df_full %>%
      filter(!is.na(!!sym(fill_variable)))

    total_national_weight <- sum(df_national$.__weight_col__, na.rm = TRUE)

    national_averages <- df_national %>%
      group_by(!!sym(fill_variable)) %>%
      summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE)) %>%
      mutate(national_pct = weighted_count / total_national_weight * 100)

    df_groups <- df_full %>%
      filter(!is.na(!!sym(x_variable)), !is.na(!!sym(fill_variable)))

    if (is_party_graph && is.null(x_filter_values)) {
      df_groups <- df_groups %>%
        filter(!!sym(x_variable) != "other")
    }

    group_totals <- df_groups %>%
      group_by(!!sym(x_variable)) %>%
      summarize(total_group_weight = sum(.__weight_col__, na.rm = TRUE))

    group_stats <- df_groups %>%
      group_by(!!sym(x_variable), !!sym(fill_variable)) %>%
      summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE),
                .groups = "drop") %>%
      left_join(group_totals, by = x_variable) %>%
      mutate(group_pct = weighted_count / total_group_weight * 100)

    plot_data <- group_stats %>%
      left_join(national_averages, by = fill_variable) %>%
      mutate(pct_diff_from_national = group_pct - national_pct)

    if (is.null(subtitle)) {
      subtitle <- if (language == "fr") {
        "Écart par rapport à la moyenne canadienne (points de %)"
      } else {
        "Difference from Canadian average (percentage points)"
      }
    }
  } else if (graph_type == "percentage") {
    df_groups <- df_full %>%
      filter(!is.na(!!sym(x_variable)), !is.na(!!sym(fill_variable)))

    if (is_party_graph && is.null(x_filter_values)) {
      df_groups <- df_groups %>%
        filter(!!sym(x_variable) != "other")
    }

    group_totals <- df_groups %>%
      group_by(!!sym(x_variable)) %>%
      summarize(total_group_weight = sum(.__weight_col__, na.rm = TRUE))

    plot_data <- df_groups %>%
      group_by(!!sym(x_variable), !!sym(fill_variable)) %>%
      summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE),
                .groups = "drop") %>%
      left_join(group_totals, by = x_variable) %>%
      mutate(group_pct = weighted_count / total_group_weight * 100)

    if (is.null(subtitle)) {
      subtitle <- if (language == "fr") {
        "Pourcentage au sein de chaque groupe"
      } else {
        "Percentage within each group"
      }
    }
  } else if (graph_type == "percentage_by_fill") {
    df_groups <- df_full %>%
      filter(!is.na(!!sym(x_variable)), !is.na(!!sym(fill_variable)))

    if (is_party_graph && is.null(x_filter_values)) {
      df_groups <- df_groups %>%
        filter(!!sym(x_variable) != "other")
    }

    fill_totals <- df_groups %>%
      group_by(!!sym(fill_variable)) %>%
      summarize(total_fill_weight = sum(.__weight_col__, na.rm = TRUE))

    plot_data <- df_groups %>%
      group_by(!!sym(x_variable), !!sym(fill_variable)) %>%
      summarize(weighted_count = sum(.__weight_col__, na.rm = TRUE),
                .groups = "drop") %>%
      left_join(fill_totals, by = fill_variable) %>%
      mutate(fill_pct = weighted_count / total_fill_weight * 100)

    if (is.null(subtitle)) {
      subtitle <- if (language == "fr") {
        "Pourcentage au sein de chaque catégorie de remplissage"
      } else {
        "Percentage within each fill category"
      }
    }
    if (is.null(y_title)) {
      y_title <- if (language == "fr") {
        "Pourcentage (%)"
      } else {
        "Percentage (%)"
      }
    }
  }

  # --- Factor Ordering and Party Mapping ---
  if (!is.null(x_order)) {
    valid_x_order <- intersect(x_order, unique(plot_data[[x_variable]]))
    if (length(valid_x_order) > 0) {
      plot_data[[x_variable]] <- factor(
        plot_data[[x_variable]],
        levels = valid_x_order
      )
    } else {
      warning(
        "None of the values specified in x_order exist in the data for x_variable."
      )
      x_order <- NULL # Reset x_order so party order can apply if needed
    }
  }

  # Handle party mapping and default order (if x_order wasn't provided/failed)
  if (is_party_graph) {
    plot_data <- plot_data %>%
      mutate(!!sym(x_variable) := case_when(
        as.character(!!sym(x_variable)) %in% names(party_mapping[[language]]) ~
          party_mapping[[language]][as.character(!!sym(x_variable))],
        TRUE ~ as.character(!!sym(x_variable))
      ))

    # Apply default party order only if x_order was NOT specified
    if (is.null(x_order)) {
      valid_party_order <- intersect(
        party_order[[language]],
        unique(plot_data[[x_variable]])
      )
      if (length(valid_party_order) > 0) {
        plot_data[[x_variable]] <- factor(
          plot_data[[x_variable]],
          levels = valid_party_order
        )
      }
    }
  }

  # Ensure x_variable is a factor for consistent scale handling
  if (!inherits(plot_data[[x_variable]], "factor")) {
    plot_data[[x_variable]] <- factor(plot_data[[x_variable]])
  }

  # Apply custom fill ordering if provided (relevant for non-difference_by_x types)
  if (graph_type != "difference_by_x" && !is.null(fill_order)) {
    valid_fill_order <- intersect(
      fill_order,
      unique(plot_data[[fill_variable]])
    )
    if (length(valid_fill_order) > 0) {
      plot_data[[fill_variable]] <- factor(
        plot_data[[fill_variable]],
        levels = valid_fill_order
      )
    } else {
      warning(
        "None of the values specified in fill_order exist in the data for fill_variable."
      )
    }
  }

  # Ensure fill_variable is a factor if used for fill aesthetic
  if (graph_type != "difference_by_x" &&
      !inherits(plot_data[[fill_variable]], "factor")) {
    plot_data[[fill_variable]] <- factor(plot_data[[fill_variable]])
  }


  # --- Prepare X-axis Labels (Text or PNG with Scaling) ---
  use_png_labels <- FALSE
  final_x_axis_labels <- NULL # Default ggplot labels unless overridden
  original_x_labels <- x_labels # Keep the user-provided arg

  if (!is.null(original_x_labels)) {
    is_png <- grepl("\\.png$", original_x_labels, ignore.case = TRUE)

    if (any(is_png)) {
      use_png_labels <- TRUE
      x_levels <- levels(plot_data[[x_variable]]) # Get levels in current order

      markdown_labels <- sapply(x_levels, function(level) {
        # Find original key corresponding to current level
        original_key <- level
        if (!is.null(reverse_party_mapping) &&
            level %in% names(reverse_party_mapping)) {
          original_key <- reverse_party_mapping[[level]]
        }

        # Look up original key in user-provided x_labels
        label_value <- original_x_labels[original_key]

        if (!is.na(label_value) && !is.null(label_value)) {
          if (grepl("\\.png$", label_value, ignore.case = TRUE)) {
            # --- Get image height and apply scale ---
            img_tag <- tryCatch({
              if (!file.exists(label_value)) {
                warning(
                  "PNG file not found: ", label_value,
                  ". Using level text '", level, "' instead.",
                  call. = FALSE
                )
                return(as.character(level)) # Fallback to text
              }

              # Use magick to get image info
              img_info <- image_info(image_read(label_value))
              original_height <- img_info$height[1] # Use 1st frame

              if (is.na(original_height) || original_height <= 0) {
                warning(
                  "Could not read valid height for: ", label_value,
                  ". Using level text '", level, "' instead.",
                  call. = FALSE
                )
                return(as.character(level)) # Fallback
              }

              # Calculate new height based on scale factor
              new_height <- round(original_height * png_label_scale)
              if (new_height <= 0) new_height <- 1 # Ensure positive height

              # Create Markdown img tag using calculated height only
              sprintf("<img src='%s' height='%d' />", label_value, new_height)
            }, error = function(e) {
              warning(
                "Error processing image ", label_value, ": ", e$message,
                ". Using level text '", level, "' instead.",
                call. = FALSE
              )
              return(as.character(level)) # Fallback to text on error
            })
            return(img_tag)
            # --- End image processing ---
          } else {
            # Use text label directly
            as.character(label_value)
          }
        } else {
          # Default: use the level itself if no label found for this key
          as.character(level)
        }
      }, USE.NAMES = FALSE) # Get labels in order of levels

      final_x_axis_labels <- setNames(markdown_labels, x_levels)

    } else {
      # Use provided text labels directly
      final_x_axis_labels <- original_x_labels
    }
  }

  # --- Create the ggplot object ---
  y_aes_var <- switch(
    graph_type,
    "percentage" = "group_pct",
    "difference" = "pct_diff_from_national",
    "percentage_by_fill" = "fill_pct",
    "difference_by_x" = "diff_from_national_pct"
  )

  p <- ggplot(plot_data, aes(x = !!sym(x_variable), y = !!sym(y_aes_var)))

  if (graph_type %in% c("difference", "difference_by_x")) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  }

  bar_position <- "dodge"
  bar_width <- if (graph_type == "difference_by_x") 0.6 else 0.9

  if (graph_type == "difference_by_x") {
    if (!is.null(colors)) {
      p <- p + geom_bar(
        stat = "identity", position = bar_position,
        aes(fill = original_x), width = bar_width
      ) +
        scale_fill_manual(values = colors, guide = "none") # No legend
    } else {
      p <- p + geom_bar(
        stat = "identity", position = bar_position,
        fill = "#1A4782", width = bar_width
      ) # Default color
    }
  } else {
    # Other graph types use fill_variable for fill aesthetic
    p <- p + geom_bar(
      stat = "identity", position = bar_position,
      aes(fill = !!sym(fill_variable)), width = bar_width
    )
    # Apply fill colors and labels later
  }


  # --- Apply Scales ---

  # Fill scale (only if fill aesthetic was used)
  if (graph_type != "difference_by_x" && !is.null(colors)) {
    fill_labels_to_use <- fill_labels %||% names(colors)

    # Match labels to factor levels if fill variable is factored
    if (inherits(plot_data[[fill_variable]], "factor")) {
      current_levels <- levels(plot_data[[fill_variable]])
      if (!is.null(names(fill_labels_to_use)) &&
          all(current_levels %in% names(fill_labels_to_use))) {
        # Reorder/filter labels using names
        fill_labels_to_use <- fill_labels_to_use[current_levels]
      } else if (length(fill_labels_to_use) == length(current_levels)) {
        # Assume order is correct if unnamed and length matches
        names(fill_labels_to_use) <- current_levels
      } else {
        warning(
          "Mismatch between fill factor levels and fill_labels. Using default legend labels."
        )
        fill_labels_to_use <- waiver() # Use ggplot default
      }
    }
    p <- p + scale_fill_manual(values = colors, labels = fill_labels_to_use)
  }

  # X scale labels (apply the prepared text or markdown labels)
  if (!is.null(final_x_axis_labels)) {
    p <- p + scale_x_discrete(labels = final_x_axis_labels)
  } # else: ggplot uses factor levels by default


  # --- Apply Theme ---
  p <- p +
    labs(
      title = title,
      subtitle = subtitle,
      x = "", # Explicitly set x-axis title to blank
      y = y_title,
      fill = "", # Legend title for fill
      caption = caption_text
    ) +
    theme_datagotchi_light() # Apply base theme first

  # Conditionally modify axis.text.x AFTER base theme
  if (use_png_labels) {
    # Use element_markdown for PNG labels
    p <- p + theme(
      # Use element_markdown, size setting here is less critical for image size
      axis.text.x = element_markdown(
        size = 11, # Base size for spacing/lineheight context
        lineheight = 1.5
      ), # Increased lineheight for potentially tall images
      # --- Reapply other theme elements to ensure consistency ---
      axis.text.y = element_text(size = 52),
      # Increased top margin for axis title to avoid overlap
      axis.title.x = element_text(size = 56, margin = margin(t = 40)),
      axis.title.y = element_text(
        size = 72, face = "bold", margin = margin(r = 30)
      ),
      plot.title = element_text(
        size = 102, face = "bold", margin = margin(b = 15)
      ),
      plot.subtitle = element_text(
        size = 52, margin = margin(b = 15), hjust = 0.5
      ),
      plot.caption = element_text(
        size = 44, hjust = 0, lineheight = 0.3
      ),
      legend.title = element_text(size = 56),
      legend.text = element_text(size = 52),
      legend.key.size = unit(0.5, "in")
    )
  } else {
    # Use standard element_text for text labels
    p <- p + theme(
      axis.text.x = element_text(angle = 0, hjust = 1, size = 52), # Original
      axis.text.y = element_text(size = 52),
      axis.title.x = element_text(size = 56, margin = margin(t = 30)),
      axis.title.y = element_text(
        size = 72, face = "bold", margin = margin(r = 30)
      ),
      plot.title = element_text(
        size = 102, face = "bold", margin = margin(b = 15)
      ),
      plot.subtitle = element_text(
        size = 52, margin = margin(b = 15), hjust = 0.5
      ),
      plot.caption = element_text(
        size = 44, hjust = 0, lineheight = 0.3
      ),
      legend.title = element_text(size = 56),
      legend.text = element_text(size = 52),
      legend.key.size = unit(0.5, "in")
    )
  }

  # --- Save and Add Logos ---
  if (!is.null(output_path)) {
    ggsave(
      plot = p, # Explicitly pass plot object
      filename = output_path,
      width = 16, height = 9, dpi = 300, units = "in", scale = 1
    )

    if (add_logo && !is.null(logos_list) && exists("add_multiple_pngs")) {
      # Ensure add_multiple_pngs function is available before calling
      add_multiple_pngs(
        base_png_path = output_path,
        output_path = gsub("\\.png$", "_final.png", output_path),
        png_list = logos_list
      )
    } else if (add_logo && !exists("add_multiple_pngs")) {
      warning(
        "Function 'add_multiple_pngs' not found. Logos will not be added.",
        call. = FALSE
      )
    }
  }

  return(p)
}

# Null coalescing operator (ensure it's available)
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
