#' Plots the distributions of the input subgroup and reference.
#' @description
#' This function generates one canvas with two distributions, one representing the subgroup and another representing the reference.
#' It uses \code{ggplot2} to produce the plot objects.
#' @param subgroup_conditions A list of strings that defines the feature conditions of the subgroup.
#' @param subgroup_distribution A numeric vector composed of the distribution values of the subgroup.
#' @param reference_conditions A list of strings that defines the feature conditions of the reference.
#' @param reference_distribution A numeric vector composed of the distribution values of the reference.
#' @param label A string that represents the label used on the distribution axis.
#' @param type A string that contains the type of plot to be produced. It can be "boxplot", "density", "dotplot", "histogram", "scatterplot" or "violinplot".
#' @return A \code{ggplot2} object composed of a canvas portraying the subgroup and the reference.
#' @examples
#' plot_single_subgroup(subgroup_conditions = t[1,]$feature_conditions, subgroup_distribution = t[1,]$distribution_values %>% unlist(use.names = FALSE), reference_conditions = t[nrow(t),]$feature_conditions, reference_distribution = t[nrow(t),]$distribution_values %>% unlist(use.names = FALSE), type = "Boxplot", label = "error")
#' plot_single_subgroup(subgroup_conditions = t[1,]$feature_conditions, subgroup_distribution = t[1,]$distribution_values %>% unlist(use.names = FALSE), reference_conditions = t[nrow(t),]$feature_conditions, reference_distribution = t[nrow(t),]$distribution_values %>% unlist(use.names = FALSE), type = "density", label = "error")
#' plot_single_subgroup(subgroup_conditions = t[1,]$feature_conditions, subgroup_distribution = t[1,]$distribution_values %>% unlist(use.names = FALSE), reference_conditions = t[nrow(t),]$feature_conditions, reference_distribution = t[nrow(t),]$distribution_values %>% unlist(use.names = FALSE), type = "dotplot", label = "error")
#' plot_single_subgroup(subgroup_conditions = t[1,]$feature_conditions, subgroup_distribution = t[1,]$distribution_values %>% unlist(use.names = FALSE), reference_conditions = t[nrow(t),]$feature_conditions, reference_distribution = t[nrow(t),]$distribution_values %>% unlist(use.names = FALSE), type = "histogram", label = "error")
#' plot_single_subgroup(subgroup_conditions = t[1,]$feature_conditions, subgroup_distribution = t[1,]$distribution_values %>% unlist(use.names = FALSE), reference_conditions = t[nrow(t),]$feature_conditions, reference_distribution = t[nrow(t),]$distribution_values %>% unlist(use.names = FALSE), type = "scatterplot", label = "error")
#' plot_single_subgroup(subgroup_conditions = t[1,]$feature_conditions, subgroup_distribution = t[1,]$distribution_values %>% unlist(use.names = FALSE), reference_conditions = t[nrow(t),]$feature_conditions, reference_distribution = t[nrow(t),]$distribution_values %>% unlist(use.names = FALSE), type = "violinplot", label = "error")
#' @import dplyr
#' @import ggplot2
#' @export
plot_single_subgroup <- function(subgroup_conditions, subgroup_distribution, reference_conditions, reference_distribution, type = "Boxplot", label = "error") {
  # Subgroup information
  subgroup_text <- paste(subgroup_conditions %>% unlist() %>% sort(), collapse = ",\n")
  subgroup_size <- length(subgroup_distribution)
  # Reference information
  reference_text <- paste(reference_conditions %>% unlist() %>% sort(), collapse = ",\n")
  reference_size <- length(reference_distribution)
  # All distribution values
  distribution_values <- c(subgroup_distribution, reference_distribution)

  # Data frame containing all data
  subgroup_labels <- rep(c(subgroup_text), each = subgroup_size)
  reference_labels <- rep(c(reference_text), each = reference_size)
  label_values <- c(subgroup_labels, reference_labels)
  df <- data.frame(label_values, distribution_values)

  # Legend for each plot
  legend <- list()
  legend[subgroup_text] <- paste0(subgroup_text, "\n", subgroup_size, " instances\n(", format(round(((subgroup_size / reference_size) * 100), 3), nsmall = 3), "%)")
  legend[reference_text] <- paste0(reference_text, "\n", reference_size, " instances\n(", 100, "%)")

  # Plot
  switch(
    type %>% tolower(),
    "boxplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, fill = label_values)) +
      theme(legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label) +
      geom_boxplot() +
      scale_x_discrete("label_values", labels = legend) +
      geom_hline(yintercept = median(reference_distribution), linetype = "dashed", colour = "#333233"),
    "density" = ggplot(data = df, aes(x = distribution_values, fill = label_values)) +
      theme(legend.title = element_blank(), legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold")) +
      labs(x = label) +
      geom_density(alpha = 0.5) +
      scale_fill_discrete("label_values", labels = legend),
    "dotplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, fill = label_values)) +
      theme(legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label) +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.8, binwidth = 0.05, position = "dodge") +
      scale_x_discrete("label_values", labels = legend),
    "histogram" = ggplot(data = df, aes(x = distribution_values, fill = label_values)) +
      theme(legend.title = element_blank(), legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold")) +
      labs(x = label, y = "Frequency") +
      geom_histogram(aes(y = stat(density * width)), binwidth = 0.1, alpha = 0.5, position = "identity") +
      scale_fill_discrete("label_values", labels = legend),
    "scatterplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, colour = label_values)) +
      theme(legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label) +
      geom_point(position = position_dodge(width = 0)) +
      scale_x_discrete("label_values", labels = legend),
    "violinplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, fill = label_values)) +
      theme(legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label) +
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) +
      scale_x_discrete("label_values", labels = legend),
    stop("Invalid error type. Type must be Boxplot, Density, Dotplot, Histogram, Scatterplot or Violinplot.")
  ) %>% return()
}
