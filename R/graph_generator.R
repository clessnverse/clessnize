# Load necessary packages (ensure these are imported in NAMESPACE if building pkg)
library(dplyr)
library(ggplot2)
library(rlang)
library(ggtext)
library(magick)
library(utils) # For download.file

#' Create Standardized Data Visualization Graphs
#'
#' Generates various types of standardized graphs (percentages, differences from
#' average) with options for weighting, filtering, custom labels (including
#' PNG images for axis labels), colors, ordering, and logo overlays.
#'
#' @param graph_type String indicating the type of graph:
#'   \itemize{
#'     \item{"percentage"}: Shows percentages of `fill_variable` values within
#'       each group on the x-axis (bars within an x-group sum to 100%).
#'     \item{"difference"}: Shows the difference (in percentage points) between
#'       the percentage of each `fill_variable` value within an x-axis group
#'       and the national average percentage for that `fill_variable` value.
#'     \item{"percentage_by_fill"}: Shows the percentage distribution of
#'       `x_variable` values within each group defined by `fill_variable`
#'       (bars for a specific fill color across all x-categories sum to 100%).
#'     \item{"difference_by_x"}: Shows the difference between the weighted mean
#'       of a numeric `fill_variable` for each x-axis group and the overall
#'       weighted mean of that variable across all included data. The
#'       difference is typically shown in percentage points if the numeric
#'       variable is scaled 0-1.
#'   }
#' @param data Dataframe containing the survey data.
#' @param x_variable String with the name of the column in `data` to use for the
#'   x-axis (usually a categorical variable, default: "dv_voteChoice").
#' @param fill_variable String with the name of the column in `data` to use for
#'   fill colors in "percentage", "difference", and "percentage_by_fill" modes.
#'   In "difference_by_x" mode, this parameter specifies the numeric column
#'   to analyze.
#' @param weights_variable String with the name of the column in `data`
#'   containing respondent weights (default: `NULL` for unweighted analysis).
#' @param filter_values Optional vector of values. Only rows where the
#'   `fill_variable` column matches one of these values will be included in
#'   calculations (applied AFTER initial data selection).
#' @param x_filter_values Optional vector of values. Only rows where the
#'   `x_variable` column matches one of these values will be included in
#'   calculations (applied AFTER initial data selection).
#' @param language String, either "fr" (French) or "en" (English), controlling
#'   default labels and party name mappings (default: "fr").
#' @param colors Named vector where names correspond to the values in
#'   `fill_variable` (for most graph types) or `x_variable` (for
#'   "difference_by_x") and values are valid color specifications (e.g., hex
#'   codes, color names). Names should match values *before* party mapping for
#'   difference_by_x.
#' @param fill_labels Named vector providing display labels for the values in
#'   `fill_variable`. Names should match the data values, values are the labels
#'   for the legend. If `NULL`, the original values or names from `colors`
#'   are used.
#' @param x_labels Named vector providing display labels for the values on the
#'   x-axis. Names should match the data values *before* potential party
#'   mapping (e.g., "lpc", "cpc"). Values are the desired labels.
#'   **Values can be paths to local PNG files or URLs pointing to PNG files
#'   (ending in .png) to display images instead of text.**
#' @param x_order Optional vector specifying the desired order of categories
#'   on the x-axis. Values should match the data values *before* potential
#'   party mapping.
#' @param fill_order Optional vector specifying the desired order of categories
#'   in the legend (for applicable graph types). Values should match the data
#'   values in `fill_variable`.
#' @param title String for the main graph title.
#' @param subtitle String for the graph subtitle. Default subtitles are
#'   provided based on `graph_type` and `language` if `NULL`.
#' @param y_title String for the y-axis title. Default titles are provided
#'   based on `graph_type` and `language` if `NULL`.
#' @param custom_caption String to completely override the default caption.
#'   The default caption includes source, sample size (n), and weighting info.
#' @param add_caption_line String to add as an additional line *below* the
#'   default caption (ignored if `custom_caption` is provided).
#' @param output_path String specifying the full path (including filename and
#'   .png extension) where the generated graph should be saved. If `NULL`, the
#'   graph is not saved automatically.
#' @param add_logo Logical indicating whether to add logos specified in
#'   `logos_list` to the saved graph (default: `TRUE`). Requires the existence
#'   of an `add_multiple_pngs` function (assumed available in the package).
#' @param logos_list Optional list used by `add_multiple_pngs` function to
#'   specify logo files and their positions. See `add_png.R` documentation.
#' @param png_label_scale Numeric scaling factor for PNG labels used on the
#'   x-axis (when `x_labels` provides image paths/URLs). Default is `1.0`
#'   (100% of original image size). The factor is applied to the image height,
#'   and the width adjusts automatically to maintain aspect ratio.
#'
#' @return A `ggplot` object representing the generated graph.
#'
#' @export
#'
#' @importFrom dplyr filter group_by summarize mutate left_join case_when select
#'   all_of vars one_of .data
#' @importFrom ggplot2 ggplot aes geom_bar geom_hline scale_fill_manual
#'   scale_x_discrete labs theme element_text margin unit ggsave waiver
#'   element_line element_rect element_blank scale_fill_discrete
#' @importFrom stats weighted.mean
#' @importFrom rlang sym !! := `%||%` is_character is_logical is_vector
#'   is_list is_numeric is_null
#' @importFrom ggtext element_markdown
#' @importFrom magick image_read image_info
#' @importFrom utils download.file
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
  # --- Validate Arguments ---
  graph_type <- match.arg(graph_type)
  stopifnot(
    is.data.frame(data),
    rlang::is_character(x_variable), length(x_variable) == 1,
    rlang::is_character(fill_variable), length(fill_variable) == 1,
    rlang::is_null(weights_variable) || (rlang::is_character(weights_variable) && length(weights_variable) == 1),
    rlang::is_null(filter_values) || rlang::is_vector(filter_values),
    rlang::is_null(x_filter_values) || rlang::is_vector(x_filter_values),
    language %in% c("fr", "en"),
    rlang::is_null(colors) || (rlang::is_vector(colors) && !rlang::is_null(names(colors))),
    rlang::is_null(fill_labels) || (rlang::is_vector(fill_labels) && !rlang::is_null(names(fill_labels))),
    rlang::is_null(x_labels) || (rlang::is_vector(x_labels) && !rlang::is_null(names(x_labels))),
    rlang::is_null(x_order) || rlang::is_vector(x_order),
    rlang::is_null(fill_order) || rlang::is_vector(fill_order),
    rlang::is_null(title) || (rlang::is_character(title) && length(title) == 1),
    rlang::is_null(subtitle) || (rlang::is_character(subtitle) && length(subtitle) == 1),
    rlang::is_null(y_title) || (rlang::is_character(y_title) && length(y_title) == 1),
    rlang::is_null(custom_caption) || (rlang::is_character(custom_caption) && length(custom_caption) == 1),
    rlang::is_null(add_caption_line) || (rlang::is_character(add_caption_line) && length(add_caption_line) == 1),
    rlang::is_null(output_path) || (rlang::is_character(output_path) && length(output_path) == 1),
    rlang::is_logical(add_logo), length(add_logo) == 1,
    rlang::is_null(logos_list) || rlang::is_list(logos_list),
    rlang::is_numeric(png_label_scale), length(png_label_scale) == 1, png_label_scale > 0
  )

  # --- Define party mappings and order ---
  party_mapping <- list(
    "fr" = c("lpc" = "PLC", "cpc" = "PCC", "ndp" = "NPD", "bq" = "BQ", "gpc" = "PVC"),
    "en" = c("lpc" = "LPC", "cpc" = "CPC", "ndp" = "NDP", "bq" = "BQ", "gpc" = "GPC")
  )
  default_party_order <- list(
    "fr" = c("PLC", "PCC", "BQ", "NPD", "PVC"),
    "en" = c("LPC", "CPC", "BQ", "NDP", "GPC")
  )

  # --- Reverse mapping for label lookup ---
  reverse_party_mapping <- NULL
  if (x_variable == "dv_voteChoice" && !rlang::is_null(party_mapping[[language]])) {
    reverse_party_mapping <- setNames(names(party_mapping[[language]]), party_mapping[[language]])
  }

  # --- Initial setup (weights, captions, y_title) ---
  default_y_title <- if (language == "fr") "Moyenne canadienne" else "Canadian average"
  y_title_set <- y_title # Store user preference

  caption_weight_text <- if (rlang::is_null(weights_variable)) "" else {
    if (language == "fr") "\nDonnées pondérées selon le genre, l'âge, la province, la langue, le niveau d'éducation, le revenu, le status d'immigrant et le type d'habitation"
    else "\nData weighted by gender, age, province, language, education level, income, immigration status, and housing type"
  }
  # Use current date for the default caption
  current_year <- format(Sys.Date(), "%Y") # Consistent with previous request
  default_caption <- if (language == "fr") { paste0("Source : Léger-Datagotchi ", current_year, " | n = ", nrow(data), caption_weight_text) }
  else { paste0("Source: Léger-Datagotchi ", current_year, " | n = ", nrow(data), caption_weight_text) }
  caption_text <- custom_caption %||% { if (!rlang::is_null(add_caption_line)) paste0(default_caption, "\n", add_caption_line) else default_caption }

  # Add weight column or dummy weights using internal name
  weight_col_internal <- ".WEIGHT_INTERNAL_TEMP"
  if (!rlang::is_null(weights_variable)) {
    if (!weights_variable %in% colnames(data)) stop(paste0("Weight column '", weights_variable, "' not found"))
    data[[weight_col_internal]] <- data[[weights_variable]]
  } else { data[[weight_col_internal]] <- 1 }

  # --- Select necessary columns ---
  req_vars <- unique(c(x_variable, fill_variable, weight_col_internal))
  missing_vars <- setdiff(req_vars, colnames(data))
  if (length(missing_vars) > 0) stop("Missing required columns in data: ", paste(missing_vars, collapse = ", "))
  df_full <- data %>% select(all_of(req_vars))

  # --- Apply Filters ---
  if (!rlang::is_null(filter_values)) { df_full <- df_full %>% filter(!!sym(fill_variable) %in% filter_values) }
  if (!rlang::is_null(x_filter_values)) { df_full <- df_full %>% filter(!!sym(x_variable) %in% x_filter_values) }
  if (nrow(df_full) == 0) stop("No data remaining after applying filters.")

  is_party_graph <- x_variable == "dv_voteChoice"

  # --- Data processing based on graph type ---
  if (graph_type == "difference_by_x") {
    y_variable <- fill_variable
    if (!is.numeric(df_full[[y_variable]])) stop("For graph_type='difference_by_x', fill_variable ('", y_variable, "') must be numeric.")
    df_national <- df_full %>% filter(!is.na(!!sym(y_variable)), !is.na(!!sym(x_variable)))
    if (nrow(df_national) == 0) stop("No valid data for national average calculation.")
    national_average <- stats::weighted.mean(df_national[[y_variable]], df_national[[weight_col_internal]], na.rm = TRUE)
    df_groups <- df_full %>% filter(!is.na(!!sym(x_variable)), !is.na(!!sym(y_variable)))
    if (is_party_graph && rlang::is_null(x_filter_values)) { df_groups <- df_groups %>% filter(!!sym(x_variable) != "other") }
    if (nrow(df_groups) == 0) stop("No valid data remaining for group average calculations.")
    group_averages <- df_groups %>% group_by(!!sym(x_variable)) %>% summarize(weighted_average = stats::weighted.mean(!!sym(y_variable), !!sym(weight_col_internal), na.rm = TRUE), .groups = "drop")
    plot_data <- group_averages %>% mutate(diff_from_national = .data$weighted_average - national_average, diff_from_national_pct = .data$diff_from_national * 100)
    plot_data$original_x <- plot_data[[x_variable]]
    if (rlang::is_null(subtitle)) { subtitle <- if (language == "fr") paste0("Écart par rapport à la moyenne canadienne (", y_variable, ")") else paste0("Difference from Canadian average (", y_variable, ")") }
    y_title <- y_title_set %||% (if (language == "fr") "Écart par rapport à la moyenne (points de %)" else "Difference from average (percentage points)")

  } else if (graph_type == "difference") {
    df_national <- df_full %>% filter(!is.na(!!sym(fill_variable)))
    if (nrow(df_national) == 0) stop("No valid data for national average calculation.")
    total_national_weight <- sum(df_national[[weight_col_internal]], na.rm = TRUE)
    if (total_national_weight == 0) stop("Total national weight is zero, cannot calculate percentages.")
    national_averages <- df_national %>% group_by(!!sym(fill_variable)) %>% summarize(weighted_count = sum(!!sym(weight_col_internal), na.rm = TRUE), .groups = "drop") %>% mutate(national_pct = .data$weighted_count / total_national_weight * 100)
    df_groups <- df_full %>% filter(!is.na(!!sym(x_variable)), !is.na(!!sym(fill_variable)))
    if (is_party_graph && rlang::is_null(x_filter_values)) { df_groups <- df_groups %>% filter(!!sym(x_variable) != "other") }
    if (nrow(df_groups) == 0) stop("No valid data remaining for group calculations.")
    group_totals <- df_groups %>% group_by(!!sym(x_variable)) %>% summarize(total_group_weight = sum(!!sym(weight_col_internal), na.rm = TRUE), .groups = "drop")
    group_stats <- df_groups %>% group_by(!!sym(x_variable), !!sym(fill_variable)) %>% summarize(weighted_count = sum(!!sym(weight_col_internal), na.rm = TRUE), .groups = "drop") %>% left_join(group_totals, by = x_variable) %>% mutate(group_pct = ifelse(.data$total_group_weight == 0, 0, .data$weighted_count / .data$total_group_weight * 100))
    plot_data <- group_stats %>% left_join(national_averages, by = fill_variable) %>% mutate(pct_diff_from_national = .data$group_pct - .data$national_pct)
    if (rlang::is_null(subtitle)) { subtitle <- if (language == "fr") "Écart par rapport à la moyenne canadienne (points de %)" else "Difference from Canadian average (percentage points)" }
    y_title <- y_title_set %||% (if (language == "fr") "Écart par rapport à la moyenne (points de %)" else "Difference from average (percentage points)")

  } else if (graph_type == "percentage") {
     df_groups <- df_full %>% filter(!is.na(!!sym(x_variable)), !is.na(!!sym(fill_variable)))
     if (is_party_graph && rlang::is_null(x_filter_values)) { df_groups <- df_groups %>% filter(!!sym(x_variable) != "other") }
     if (nrow(df_groups) == 0) stop("No valid data remaining for group calculations.")
     group_totals <- df_groups %>% group_by(!!sym(x_variable)) %>% summarize(total_group_weight = sum(!!sym(weight_col_internal), na.rm = TRUE), .groups = "drop")
     plot_data <- df_groups %>% group_by(!!sym(x_variable), !!sym(fill_variable)) %>% summarize(weighted_count = sum(!!sym(weight_col_internal), na.rm = TRUE), .groups = "drop") %>% left_join(group_totals, by = x_variable) %>% mutate(group_pct = ifelse(.data$total_group_weight == 0, 0, .data$weighted_count / .data$total_group_weight * 100))
     if (rlang::is_null(subtitle)) { subtitle <- if (language == "fr") "Pourcentage au sein de chaque groupe" else "Percentage within each group" }
     y_title <- y_title_set %||% (if (language == "fr") "Pourcentage (%)" else "Percentage (%)")

  } else if (graph_type == "percentage_by_fill") {
     df_groups <- df_full %>% filter(!is.na(!!sym(x_variable)), !is.na(!!sym(fill_variable)))
     if (is_party_graph && rlang::is_null(x_filter_values)) { df_groups <- df_groups %>% filter(!!sym(x_variable) != "other") }
     if (nrow(df_groups) == 0) stop("No valid data remaining for group calculations.")
     fill_totals <- df_groups %>% group_by(!!sym(fill_variable)) %>% summarize(total_fill_weight = sum(!!sym(weight_col_internal), na.rm = TRUE), .groups = "drop")
     plot_data <- df_groups %>% group_by(!!sym(x_variable), !!sym(fill_variable)) %>% summarize(weighted_count = sum(!!sym(weight_col_internal), na.rm = TRUE), .groups = "drop") %>% left_join(fill_totals, by = fill_variable) %>% mutate(fill_pct = ifelse(.data$total_fill_weight == 0, 0, .data$weighted_count / .data$total_fill_weight * 100))
     if (rlang::is_null(subtitle)) { subtitle <- if (language == "fr") "Pourcentage au sein de chaque catégorie de remplissage" else "Percentage within each fill category" }
     y_title <- y_title_set %||% (if (language == "fr") "Pourcentage (%)" else "Percentage (%)")
  }

  # --- Factor Ordering and Party Mapping ---
  if (!rlang::is_null(x_order)) {
    valid_x_order <- intersect(x_order, unique(plot_data[[x_variable]]))
    if (length(valid_x_order) > 0) { plot_data[[x_variable]] <- factor(plot_data[[x_variable]], levels = valid_x_order) }
    else { warning("None of the values specified in x_order exist in the data for x_variable."); x_order <- NULL }
  }
  if (is_party_graph) {
    plot_data <- plot_data %>% mutate(!!sym(x_variable) := case_when(as.character(!!sym(x_variable)) %in% names(party_mapping[[language]]) ~ party_mapping[[language]][as.character(!!sym(x_variable))], TRUE ~ as.character(!!sym(x_variable))))
    if (rlang::is_null(x_order)) {
      valid_party_order <- intersect(default_party_order[[language]], unique(plot_data[[x_variable]]))
      if (length(valid_party_order) > 0) { plot_data[[x_variable]] <- factor(plot_data[[x_variable]], levels = valid_party_order) }
    }
  }
  if (!inherits(plot_data[[x_variable]], "factor")) { plot_data[[x_variable]] <- factor(plot_data[[x_variable]]) }
  if (graph_type != "difference_by_x") {
    if (!rlang::is_null(fill_order)) {
      valid_fill_order <- intersect(fill_order, unique(plot_data[[fill_variable]]))
      if (length(valid_fill_order) > 0) { plot_data[[fill_variable]] <- factor(plot_data[[fill_variable]], levels = valid_fill_order) }
      else { warning("None of the values specified in fill_order exist in the data for fill_variable.") }
    }
    if (!inherits(plot_data[[fill_variable]], "factor")) { plot_data[[fill_variable]] <- factor(plot_data[[fill_variable]]) }
  }

  # --- Prepare X-axis Labels ---
  use_png_labels <- FALSE
  final_x_axis_labels <- waiver()
  original_x_labels_arg <- x_labels

  if (!rlang::is_null(original_x_labels_arg)) {
    if (rlang::is_null(names(original_x_labels_arg))) {
      warning("x_labels must be a named vector. Ignoring.", call. = FALSE)
    } else {
      # Check which entries in x_labels are PNG paths
      is_png <- grepl("\\.png$", original_x_labels_arg, ignore.case = TRUE)
      # Ensure is_png has the same names as original_x_labels_arg
      names(is_png) <- names(original_x_labels_arg)

      if (any(is_png, na.rm = TRUE)) { # Check if any are TRUE png paths
        use_png_labels <- TRUE
        x_levels <- levels(plot_data[[x_variable]])
        markdown_labels <- sapply(x_levels, function(level) {
          original_key <- level
          if (!rlang::is_null(reverse_party_mapping) && level %in% names(reverse_party_mapping)) {
            original_key <- reverse_party_mapping[[level]]
          }
          label_value <- original_x_labels_arg[original_key]

          if (!is.na(label_value) && !rlang::is_null(label_value)) {
            # Use isTRUE() to safely handle potential NA when checking is_png
            if (isTRUE(is_png[original_key])) {
              # --- Handle PNG processing ---
              img_tag <- tryCatch({
                is_url <- grepl("^https?://", label_value, ignore.case = TRUE)
                local_path <- label_value
                if (is_url) {
                  tmp_file <- tempfile(fileext = ".png")
                  download_result <- tryCatch({ utils::download.file(label_value, tmp_file, mode = "wb", quiet = TRUE); TRUE }, error = function(e) { warning("Failed to download PNG: ", label_value, " (", e$message, "). Using text '", level, "'.", call. = FALSE); FALSE })
                  if (!download_result || !file.exists(tmp_file) || file.info(tmp_file)$size == 0) { return(as.character(level)) }
                  local_path <- tmp_file
                } else { if (!file.exists(local_path)) { warning("PNG file not found: ", local_path, ". Using text '", level, "'.", call. = FALSE); return(as.character(level)) } }
                img_info <- magick::image_info(magick::image_read(local_path))
                original_height <- img_info$height[1]
                if (is.na(original_height) || original_height <= 0) { warning("Invalid height for image: ", label_value, ". Using text '", level, "'.", call. = FALSE); return(as.character(level)) }
                new_height <- max(1, round(original_height * png_label_scale))
                sprintf("<img src='%s' height='%d' />", label_value, new_height)
              }, error = function(e) { warning("Error processing image ", label_value, ": ", e$message, ". Using text '", level, "'.", call. = FALSE); return(as.character(level)) })
              return(img_tag)
              # --- End image processing ---
            } else {
              # Use text label directly (key didn't exist or wasn't a PNG)
              as.character(label_value)
            }
          } else {
            # Default: use the factor level itself if no label value found
            as.character(level)
          }
        }, USE.NAMES = FALSE)
        final_x_axis_labels <- setNames(markdown_labels, x_levels)
      } else {
        # If x_labels provided but no PNGs detected, use them as text labels
        # Ensure names match levels
        x_levels <- levels(plot_data[[x_variable]])
        if(all(x_levels %in% names(original_x_labels_arg))) {
           final_x_axis_labels <- original_x_labels_arg[x_levels] # Reorder to match levels
        } else {
           warning("Names in x_labels do not match all levels on x-axis. Using default axis labels.", call. = FALSE)
           final_x_axis_labels <- waiver() # Fallback to default
        }
      }
    }
  }

  # --- Create the ggplot object ---
  y_aes_var <- switch(graph_type, "percentage" = "group_pct", "difference" = "pct_diff_from_national", "percentage_by_fill" = "fill_pct", "difference_by_x" = "diff_from_national_pct")
  p <- ggplot(plot_data, aes(x = !!sym(x_variable), y = !!sym(y_aes_var))) +
    (if (graph_type %in% c("difference", "difference_by_x")) { geom_hline(yintercept = 0, linetype = "dashed", color = "black") })

  # Add bars
  bar_position <- "dodge"
  bar_width <- if (graph_type == "difference_by_x") 0.6 else 0.9

  if (graph_type == "difference_by_x") {
    aes_fill <- if (!rlang::is_null(colors)) aes(fill = .data$original_x) else aes()
    p <- p + geom_bar(stat = "identity", position = bar_position, mapping = aes_fill, width = bar_width)
    if (!rlang::is_null(colors)) {
       valid_color_keys <- intersect(names(colors), unique(plot_data$original_x))
       if (length(valid_color_keys) == 0) {
         warning("Names in 'colors' do not match values in 'x_variable' for difference_by_x. Using default fill.", call. = FALSE)
         p <- p + scale_fill_manual(values = c("dummy" = "#1A4782"), guide = "none", name = "") # Hack for default color
       } else {
         p <- p + scale_fill_manual(values = colors[valid_color_keys], guide = "none", name = "")
       }
    } else {
       p <- p + scale_fill_manual(values = c("dummy"="#1A4782"), guide = "none", name = "") # Apply default fill color properly
    }
  } else {
    p <- p + geom_bar(stat = "identity", position = bar_position, aes(fill = !!sym(fill_variable)), width = bar_width)
    # Apply fill scale later
  }

  # --- Apply Scales ---
  # Fill scale (only if fill aesthetic was used by geom_bar)
  if (graph_type != "difference_by_x") {
    fill_scale_args <- list(name = "") # Blank legend title default
    if (!rlang::is_null(colors)) {
      fill_scale_args$values <- colors
      fill_scale_args$labels <- fill_labels %||% waiver()
      # Reorder colors/labels to match factor levels if possible
      if (inherits(plot_data[[fill_variable]], "factor")) {
        current_levels <- levels(plot_data[[fill_variable]])
        if(!rlang::is_null(names(colors)) && all(current_levels %in% names(colors))) { fill_scale_args$values <- colors[current_levels] }
        else { warning("Names/length in 'colors' may not match levels of 'fill_variable'. Legend colors might be incorrect.", call. = FALSE) }
        if(!rlang::is_null(fill_labels)) {
           if(!rlang::is_null(names(fill_labels)) && all(current_levels %in% names(fill_labels))) { fill_scale_args$labels <- fill_labels[current_levels] }
           else if (length(fill_labels) == length(current_levels) && rlang::is_null(names(fill_labels))) { names(fill_scale_args$labels) <- current_levels} # Apply names if length matches
           else { warning("Names/length in 'fill_labels' do not match levels of 'fill_variable'. Using default labels.", call. = FALSE); fill_scale_args$labels <- waiver() }
        }
      }
      p <- p + do.call(scale_fill_manual, fill_scale_args)
    } else { # No colors specified, use default scale but apply labels if provided
      fill_scale_args$labels <- fill_labels %||% waiver()
      p <- p + do.call(scale_fill_discrete, fill_scale_args)
    }
  }

  # X scale labels (apply prepared text or markdown labels)
  if (!identical(final_x_axis_labels, waiver())) {
    p <- p + scale_x_discrete(labels = final_x_axis_labels)
  }

  # --- Apply Theme ---
  p <- p +
    labs(title = title, subtitle = subtitle, x = "", y = y_title, caption = caption_text) +
    # Call the actual theme function from the package environment
    theme_datagotchi_light()

  # Conditionally modify theme elements AFTER applying the base theme
  if (use_png_labels) {
    p <- p + theme(
      axis.text.x = ggtext::element_markdown(size = 11, lineheight = 1.5, hjust = 0.5), # Center align markdown
      axis.text.y = element_text(size = 52),
      axis.title.x = element_text(size = 56, margin = margin(t = 40)), # Increased margin
      axis.title.y = element_text(size = 72, face = "bold", margin = margin(r = 30)),
      plot.title = element_text(size = 102, face = "bold", margin = margin(b = 15)),
      plot.subtitle = element_text(size = 52, margin = margin(b = 15), hjust = 0.5),
      plot.caption = element_text(size = 44, hjust = 0, lineheight = 0.3),
      legend.title = element_text(size = 56),
      legend.text = element_text(size = 52),
      legend.key.size = unit(0.5, "in")
    )
  } else {
    p <- p + theme(
      axis.text.x = element_text(angle = 0, hjust = 1, size = 52), # Original text settings
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
  }

  # --- Save and Add Logos ---
  if (!rlang::is_null(output_path)) {
    output_dir <- dirname(output_path)
    if (!dir.exists(output_dir)) { dir.create(output_dir, recursive = TRUE) }
    ggsave(plot = p, filename = output_path, width = 16, height = 9, dpi = 300, units = "in", scale = 1)
    # Assumes add_multiple_pngs is available in the package environment
    if (add_logo && !rlang::is_null(logos_list) && exists("add_multiple_pngs")) {
      add_multiple_pngs(base_png_path = output_path, output_path = gsub("\\.png$", "_final.png", output_path), png_list = logos_list)
    } else if (add_logo && rlang::is_null(logos_list) && exists("add_multiple_pngs")) { warning("add_logo=TRUE but logos_list is NULL. No logos added.", call. = FALSE) }
    else if (add_logo && !exists("add_multiple_pngs")) { warning("Function 'add_multiple_pngs' not found. Logos will not be added.", call. = FALSE) }
  }

  # No need to clean up internal weight column as df_full is local to function

  return(p)
}

# --- Helper Functions ---

# Null coalescing operator (ensure it's available in the package or defined)
`%||%` <- function(x, y) { if (rlang::is_null(x)) y else x }

# No placeholder for theme_datagotchi_light() needed here
