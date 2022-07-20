#' Creates a grid of plots that compare pairs of distributions, with one being the reference.
#' @description
#' This function generates a grid of plots, each with two distributions, one representing the subgroup and another representing the reference.
#' It uses \code{ggplot2} to produce the plot objects.
#' @param rules A data frame of rules composed by \code{feature_conditions} and \code{distribution_values}.
#' @param reference_conditions A list of strings that defines the feature conditions of the reference.
#' @param reference_distribution A numeric vector composed of the distribution values of the reference.
#' @param page A numeric value indicating the page, i.e. the subset of input rules, that are to be displayed.
#' @param label A string that represents the label used on the distribution axis.
#' @param type A string that contains the type of plot to be produced. It can be "boxplot", "density", "dotplot", "histogram", "scatterplot" or "violinplot".
#' @param items_per_page a numeric value that indicates how many plots should be displayed in a single grid.
#' @return A grid of \code{ggplot2} objects, each portraying a subgroup and the reference.
#' @examples
#' plot_grid_single_subgroup(t, reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), type = "boxplot", label = "error", items_per_page = 5)
#' plot_grid_single_subgroup(t, reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), type = "density", label = "error", items_per_page = 9)
#' plot_grid_single_subgroup(t, reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), type = "dotplot", label = "error", items_per_page = 4)
#' plot_grid_single_subgroup(t, reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), type = "histogram")
#' plot_grid_single_subgroup(t, reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), type = "scatterplot", label = "error", items_per_page = 4)
#' plot_grid_single_subgroup(t, reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), type = "violinplot", label = "error", items_per_page = 4)
#' @import grid
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @export
plot_grid_single_subgroup <- function(rules, reference_conditions, reference_distribution, page = 1, label = "error", type = "Boxplot", items_per_page = 4) {
  if (identical(type %>% tolower(), "boxplot") == FALSE && identical(type %>% tolower(), "density") == FALSE && identical(type %>% tolower(), "dotplot") == FALSE && identical(type %>% tolower(), "histogram") == FALSE && identical(type %>% tolower(), "scatterplot") == FALSE && identical(type %>% tolower(), "violinplot") == FALSE) {
    stop("Invalid plot type: Boxplot, Density, Dotplot, Histogram, Scatterplot or Violinplot.")
  }
  # Number of items
  items <- 1:nrow(rules)
  # Generate sets of indexes per page
  indexes <- unname(split(items, ceiling(items / items_per_page)))
  # Index bigger than number of pages
  if (page > length(indexes) || page <= 0) {
    stop("Page out of bounds.")
  }
  else {
    plots <- list()
    for (i in seq_len(length(indexes[[page]]))) {
      j <- indexes[[page]][i]
      plots[[i]] <- plot_single_subgroup(subgroup_conditions = rules[j, ]$feature_conditions, subgroup_distribution = rules[j, ]$distribution_values %>% unlist(use.names = FALSE), reference_conditions = reference_conditions, reference_distribution = reference_distribution, type = type, label = label)
    }
    rows_cols <- items_per_page %>% sqrt() %>% ceiling()
    grid.arrange(grobs = plots, nrow = rows_cols, ncol = rows_cols) %>% return()
  }
}
