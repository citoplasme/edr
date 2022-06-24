#' Plots the distributions of the input subgroups and the reference.
#' @description
#' This function generates one canvas with multiple distributions representing the input subgroups and the reference.
#' It uses \code{ggplot2} to produce the plot objects.
#' @param rules A data frame of rules composed by \code{feature_conditions} and \code{distribution_values}.
#' @param reference_conditions A list of strings that defines the feature conditions of the reference.
#' @param reference_distribution A numeric vector composed of the distribution values of the reference.
#' @param title A string used as the title of the canvas.
#' @param label A string that represents the label used on the distribution axis.
#' @param type A string that contains the type of plot to be produced. It can be "boxplot", "density", "dotplot", "histogram", "scatterplot" or "violinplot".
#' @return A \code{ggplot2} object composed of a canvas portraying the input subgroups and the reference.
#' @examples
#' plot_multiple_subgroups(rules = t %>% head(5), reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), title = "First 5 Subgroups", label = "error", type = "BOXPLOT")
#' plot_multiple_subgroups(rules = t %>% head(5), reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), title = "First 5 Subgroups", label = "error", type = "density")
#' plot_multiple_subgroups(rules = t %>% head(5), reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), title = "First 5 Subgroups", label = "error", type = "dotplot")
#' plot_multiple_subgroups(rules = t %>% head(5), reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), title = "First 5 Subgroups", label = "error", type = "Histogram")
#' plot_multiple_subgroups(rules = t %>% head(5), reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), title = "First 5 Subgroups", label = "error", type = "Scatterplot")
#' plot_multiple_subgroups(rules = t %>% head(5), reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), title = "First 5 Subgroups", label = "error", type = "ViolinPlot")
#' @import dplyr
#' @import ggplot2
#' @export
plot_multiple_subgroups <- function(rules, reference_conditions, reference_distribution, title = "", label = "error", type = "Boxplot") {
  # Named list for each subgroup to keep track of number of instances
  instance_count <- list()
  # Generate a legend for each plot
  legend <- list()
  # Reference information
  reference_text <- paste(reference_conditions %>% unlist() %>% sort(), collapse = ",\n")
  reference_size <- length(reference_distribution)
  # All distribution values
  distribution_values <- c(reference_distribution)
  # Data frame containing all data
  reference_labels <- rep(c(reference_text), each = reference_size)
  label_values <- c(reference_labels)
  df <- data.frame(label_values, distribution_values)
  colnames(df) <- c("label_values", "distribution_values")
  # Update instance count
  legend[reference_text] <- paste0(reference_text, "\n", reference_size, " instances\n(", 100, "%)")

  # Rules
  for (i in seq_len(nrow(rules))) {
    # Subgroup information
    subgroup_text <- paste(rules[i, ]$feature_conditions %>% unlist() %>% sort(), collapse = ",\n")
    subgroup_distribution <- rules[i, ]$distribution_values %>% unlist(use.names = FALSE)
    subgroup_size <- length(subgroup_distribution)
    subgroup_labels <- rep(c(subgroup_text), each = subgroup_size)
    # Update data frame
    tmp <- data.frame(subgroup_labels, subgroup_distribution)
    colnames(tmp) <- c("label_values", "distribution_values")
    df <- rbind(df, tmp)
    # Update legend for plot
    legend[subgroup_text] <- paste0(subgroup_text, "\n", subgroup_size, " instances\n(", format(round(((as.integer(subgroup_size) / reference_size) * 100), 3), nsmall = 3), "%)")
  }

  # Plot
  switch(
    type %>% tolower(),
    "boxplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, fill = label_values)) +
      theme(legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label, title = title) +
      scale_x_discrete("label_values", labels = legend) +
      geom_boxplot() +
      geom_hline(yintercept = median(reference_distribution), linetype = "dashed", colour = "#333233"),
    "density" = ggplot(data = df, aes(x = distribution_values, fill = label_values)) +
      theme(legend.title = element_blank(), legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold")) +
      labs(x = label, title = title) +
      geom_density(alpha = 0.5) +
      scale_fill_discrete("label_values", labels = legend),
    "dotplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, fill = label_values)) +
      theme(legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label) +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.8, binwidth = 0.05, position = "dodge") +
      scale_x_discrete("label_values", labels = legend),
    "histogram" = ggplot(data = df, aes(x = distribution_values, fill = label_values)) +
      theme(legend.title = element_blank(), legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold")) +
      labs(x = label, y = "Frequency",  title = title) +
      geom_histogram(aes(y = stat(density * width)), binwidth = 0.1, alpha = 0.5, position = "identity") +
      scale_fill_discrete("label_values", labels = legend),
    "scatterplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, colour = label_values)) +
      theme(legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label) +
      geom_point(position = position_dodge(width = 0)) +
      scale_x_discrete("label_values", labels = legend),
    "violinplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, fill = label_values)) +
      theme(legend.position = "none", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label, title = title) +
      scale_x_discrete("label_values", labels = legend) +
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)),
    stop("Invalid error type. Type must be Boxplot, Density, Dotplot, Histogram, Scatterplot or Violinplot.")
  ) %>% return()
}
