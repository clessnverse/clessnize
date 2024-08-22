#' Draw Datagotchi Node
#'
#' This function creates a graphical representation of a decision tree node, illustrating the proportion of predicted outcomes for Democrat, Independent, and Republican categories, along with the proportion of the total sample in that node.
#'
#' @param pred_democrat Numeric. The proportion of predictions classified as Democrat in this node (between 0 and 1).
#' @param pred_independant Numeric. The proportion of predictions classified as Independent in this node (between 0 and 1).
#' @param pred_republican Numeric. The proportion of predictions classified as Republican in this node (between 0 and 1).
#' @param node_proportion Numeric. The proportion of the total sample represented by this node (between 0 and 1).
#' @param label Character. The label for the node, typically representing the decision criterion.
#' @param left_lab Character. The label to be displayed on the left side of the node (e.g., corresponding to the "Yes" branch).
#' @param right_lab Character. The label to be displayed on the right side of the node (e.g., corresponding to the "No" branch).
#'
#' @return A ggplot object representing the graphical node.
#'
#' @examples
#' draw_datagotchi_node(
#'   pred_democrat = 0.5,
#'   pred_independant = 0.2,
#'   pred_republican = 0.3,
#'   node_proportion = 0.7,
#'   label = "Prius or Pickup?",
#'   left_lab = "Prius",
#'   right_lab = "Pickup"
#' )
#' 
#' @import ggplot2
#' @export
draw_datagotchi_node <- function(
  pred_democrat,
  pred_independant,
  pred_republican,
  node_proportion,
  label,
  left_lab,
  right_lab,
  base_size = 20
) {
  # Calcul des pourcentages
  pct_democrat <- pred_democrat * 100
  pct_independant <- pred_independant * 100
  pct_republican <- pred_republican * 100
  # Déterminer le groupe le plus nombreux
  pcts <- c("DEMOCRAT" = pct_democrat, "INDEPENDANT" = pct_independant, "REPUBLICAN" = pct_republican)
  max_group <- ifelse(
    pct_democrat == max(pcts),
    "DEMOCRAT",
    ifelse(
      pct_republican == max(pcts),
      "REPUBLICAN",
      "INDEPENDANT")
    )
  # Déterminer la couleur du groupe avec le pourcentage le plus élevé
  max_color <- ifelse(max_group == "DEMOCRAT", "#00AEF3",
                      ifelse(max_group == "REPUBLICAN", "#E81B23", "darkgray"))
  # Créer un dataframe pour ggplot
  data <- data.frame(
    x = c(1, 1, 1),
    category = c("DEMOCRAT", "INDEPENDANT", "REPUBLICAN"),
    start = c(0, pct_democrat, pct_democrat + pct_independant),
    end = c(pct_democrat, pct_democrat + pct_independant, 100)
  )
  # Créer le plot avec ggplot2
  plot <- ggplot(data) +
    geom_rect(aes(xmin = 0, xmax = 1,
                  ymin = start, ymax = end, fill = category)) +
    scale_fill_manual(values = c("DEMOCRAT" = "#00AEF3", "INDEPENDANT" = "lightgray", "REPUBLICAN" = "#E81B23")) +
    coord_flip() +
    labs(
      title = label
    ) +
    theme_datagotchi_light(base_size = base_size) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = NA, color = NA),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"
    ) +
    annotate(
      "text", x = 0.5, y = -10,
      label = left_lab, hjust = 1,
      size = 5, angle = 90, family = "PixelOperatorSC"
    ) +
    annotate(
      "text", x = 0.5, y = 110, 
      label = right_lab, hjust = 1,
      size = 5, angle = 90, family = "PixelOperatorSC"
    ) +
    annotate(
      geom = "rect",
      xmin = -0.122, xmax = -0.053, ymin = 0, ymax = 100,
      fill = NA, color = "grey", linetype = "dashed",
      stroke = 0.5
    ) +
    annotate(
      geom = "rect",
      xmin = -0.125, xmax = -0.05, ymin = 0, ymax = node_proportion * 100, fill = "black"
    ) +
    annotate(
      "text",
      x = -0.0875, y = node_proportion * 100 + 3, hjust = 0.5,
      label = paste0(round(node_proportion * 100), "\n% of sample"),
      family = "PixelOperatorSC"
    ) +
    annotate(
      "text", x = -0.2, y = 50, label = paste(max_group, round(pcts[max_group]), "%"),
      hjust = 0.5, size = 5, color = max_color, fontface = "bold",
      family = "PixelOperatorSC"
    )  
  return(plot)
}
