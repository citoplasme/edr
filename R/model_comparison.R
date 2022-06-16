#' Plots the distributions of input rules grouped by and colored by their respective models.
#' @description
#' This function generates one or more plots of the rules divided by subgroup and colored by model.
#' It uses \code{ggplot2} to produce the plot objects.
#' @param rules A data frame of rules composed by \code{feature_conditions}, \code{antecedent_support}, \code{p_value}, \code{kurtosis}, \code{skewness}, \code{mean}, \code{mode}, \code{median} and \code{standard_deviation}.
#' @param title A string used as the title of the canvas.
#' @param label A string that represents the label used on the distribution axis.
#' @param type A string that contains the type of plot to be produced. It can be "boxplot", "density", "dotplot", "histogram", "scatterplot" or "violinplot".
#' @return A \code{ggplot2} object composed of the plots of the rules divided by subgroup and colored by model.
#' @examples
#' rule_1 <- t %>% head(1) %>% mutate(model = "SVM")
#' rule_2 <- rule_1 %>% mutate(distribution_values = (distribution_values %>% unlist(use.names = FALSE) + runif(distribution_values %>% unlist(use.names = FALSE) %>% length(), -1, 1)) %>% list(), model = "ANN")
#' reference_1 <- t %>% tail(1) %>% mutate(model = "SVM")
#' reference_2 <- reference_1 %>% mutate(distribution_values = (distribution_values %>% unlist(use.names = FALSE) + runif(distribution_values %>% unlist(use.names = FALSE) %>% length(), -1, 1)) %>% list(), model = "ANN")
#'
#' plot_multiple_models(rules = rbind(rule_1, rule_2), title = "Example of a title", label = "errors", type = "boxPlot")
#' plot_multiple_models(rules = rbind(rule_1, rule_2), title = "Example of a title", label = "errors", type = "DENSITY")
#' plot_multiple_models(rules = rbind(rule_1, rule_2), title = "Example of a title", label = "errors", type = "dotplot")
#' plot_multiple_models(rules = rbind(rule_1, rule_2), title = "Example of a title", label = "errors", type = "hisTOgRAM")
#' plot_multiple_models(rules = rbind(rule_1, rule_2), title = "Example of a title", label = "errors", type = "SCATTERplot")
#' plot_multiple_models(rules = rbind(rule_1, rule_2), title = "Example of a title", label = "errors", type = "violinplot")
#'
#' plot_multiple_models(rules = rbind(rule_1, rule_2, reference_1, reference_2), title = "Example of a title", label = "errors", type = "boxPlot")
#' plot_multiple_models(rules = rbind(rule_1, rule_2, reference_1, reference_2), title = "Example of a title", label = "errors", type = "DENSITY")
#' plot_multiple_models(rules = rbind(rule_1, rule_2, reference_1, reference_2), title = "Example of a title", label = "errors", type = "dotplot")
#' plot_multiple_models(rules = rbind(rule_1, rule_2, reference_1, reference_2), title = "Example of a title", label = "errors", type = "hisTOgRAM")
#' plot_multiple_models(rules = rbind(rule_1, rule_2, reference_1, reference_2), title = "Example of a title", label = "errors", type = "Scatterplot")
#' plot_multiple_models(rules = rbind(rule_1, rule_2, reference_1, reference_2), title = "Example of a title", label = "errors", type = "Violinplot")

plot_multiple_models <- function(rules, title = "", label = "error", type = "Boxplot") {
  # Generate a legend for each plot
  legend <- list()
  # initialize data frame
  df <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(df) <- c("label_values", "distribution_values", "model_values")

  for (i in seq_len(nrow(rules))) {
    # Subgroup information
    subgroup_text <- paste(rules[i, ]$feature_conditions %>% unlist() %>% sort(), collapse = ",\n")
    subgroup_distribution <- rules[i, ]$distribution_values %>% unlist(use.names = FALSE)
    subgroup_size <- length(subgroup_distribution)
    subgroup_labels <- rep(c(subgroup_text), each = subgroup_size)
    subgroup_model <- rep(c(rules[i, ]$model), each = subgroup_size)
    # Update data frame
    tmp <- data.frame(subgroup_labels, subgroup_distribution, subgroup_model)
    colnames(tmp) <- c("label_values", "distribution_values", "model_values")
    df <- rbind(df, tmp)
    # Update legend for plot
    legend[subgroup_text] <- paste0(subgroup_text, "\n", subgroup_size, " instances")
  }

  # Plot
  switch(
    type %>% tolower(),
    "boxplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, fill = model_values)) +
      theme(legend.position = "bottom", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label, title = title, fill = "Model") +
      scale_x_discrete("label_values", labels = legend) +
      geom_boxplot(),
    "density" = ggplot(data = df, aes(x = distribution_values, fill = model_values)) +
      theme(legend.title = element_blank(), legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold")) +
      labs(x = label, title = title) +
      geom_density(alpha = 0.5, color = 1) +
      scale_fill_discrete("model_values", labels = legend) +
      facet_grid(. ~ label_values),
    "dotplot" = ggplot(data = df, aes(x = model_values, y = distribution_values, fill = model_values)) +
      theme(legend.position = "bottom", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label, title = title, fill = "Model") +
      geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.8, binwidth = 0.05, position = "dodge") +
      scale_x_discrete("model_values", labels = legend) +
      facet_grid(. ~ label_values),
    "histogram" = ggplot(data = df, aes(x = distribution_values, fill = model_values)) +
      theme(legend.title = element_blank(), legend.position = "bottom", legend.box = "horizontal", plot.title = element_text(face = "bold")) +
      labs(x = label, y = "Frequency",  title = title) +
      geom_histogram(aes(y = stat(density * width)), binwidth = 0.1, alpha = 0.5, position = "identity", ) +
      facet_grid(. ~ label_values) +
      scale_color_discrete("model_values", labels = legend),
    "scatterplot" = ggplot(data = df, aes(x = model_values, y = distribution_values, colour = model_values)) +
      theme(legend.position = "bottom", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label, title = title, colour = "Model") +
      geom_point(position = position_dodge(width = 0)) +
      facet_grid(. ~ label_values) +
      scale_x_discrete("model_values", labels = legend),
    "violinplot" = ggplot(data = df, aes(x = label_values, y = distribution_values, fill = model_values)) +
      theme(legend.position = "bottom", axis.title.x = element_blank(), plot.title = element_text(face = "bold")) +
      labs(y = label, title = title, fill = "Model") +
      scale_x_discrete("label_values", labels = legend) +
      geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)),
    stop("Invalid error type. Type must be Boxplot, Density, Dotplot, Histogram, Scatterplot or Violinplot.")
  ) %>% return()
}

