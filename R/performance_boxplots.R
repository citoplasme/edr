#' Plots the distributions of input rules using boxplots and colors their quantile values based on how these compare to a reference distribution.
#' @description
#' This function generates one canvas with multiple boxplots of the rules, each with three possible colors for each quantile: red, blue or green.
#' The first color means that the distribution of the subgroup for that quantile has a higher value than the reference for the same quantile, blue the same and green a lower value.
#' It uses \code{ggplot2} to produce the plot objects.
#' @param rules A data frame of rules composed by \code{feature_conditions}, \code{antecedent_support}, \code{p_value}, \code{kurtosis}, \code{skewness}, \code{mean}, \code{mode}, \code{median} and \code{standard_deviation}.
#' @param reference_conditions A list of strings that defines the feature conditions of the reference.
#' @param reference_distribution A numeric vector composed of the distribution values of the reference.
#' @param label A string that represents the label used on the distribution axis.
#' @return A \code{ggplot2} object composed of a canvas of boxplots from the rules.
#' @examples
#' performance_boxplots(rules = t %>% head(-1), reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), label = "error")
#' performance_boxplots(rules = t %>% head(40), reference_conditions = t %>% tail(1) %>% pull(feature_conditions), reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE), label = "error")
#' @import dplyr
#' @import ggplot2
#' @export
performance_boxplots <- function(rules, reference_conditions, reference_distribution, label = "") {
  # Initialize data frame
  df <- data.frame(matrix(nrow = 0, ncol = 2))
  colnames(df) <- c("label_values", "distribution_values")
  # Reference information
  reference_text <- paste(reference_conditions %>% unlist() %>% sort(), collapse = ",")
  reference_size <- length(reference_distribution)
  reference_labels <- rep(c(reference_text), each = reference_size)
  # Update data frame
  tmp <- data.frame(reference_labels, reference_distribution)
  colnames(tmp) <- c("label_values", "distribution_values")
  df <- rbind(df, tmp)
  # Initialize vector that contains all text information to factorize
  subgroup_levels <- c()

  for (i in seq_len(nrow(rules))) {
    # Subgroup information
    subgroup_text <- paste(rules[i, ]$feature_conditions %>% unlist() %>% sort(), collapse = ",")
    subgroup_distribution <- rules[i, ]$distribution_values %>% unlist(use.names = FALSE)
    subgroup_size <- length(subgroup_distribution)
    subgroup_labels <- rep(c(subgroup_text), each = subgroup_size)
    # Update data frame
    tmp <- data.frame(subgroup_labels, subgroup_distribution)
    colnames(tmp) <- c("label_values", "distribution_values")
    df <- rbind(df, tmp)
    # Update vector
    subgroup_levels <- c(subgroup_levels, subgroup_text)
  }

  subgroup_levels <- subgroup_levels %>% sort()
  df$label_values <- df$label_values %>% factor(ordered = TRUE, levels = c(subgroup_levels, reference_text))

  quartile_values <- boxplot.stats(reference_distribution)$stats

  # Plot
  plot <- ggplot(df, aes(x = distribution_values, y = label_values)) +
    geom_boxplot() +
    xlab(label) +
    ylab ("")

  plot_build <- ggplot_build(plot)$data[[1]] %>% head(-1)

  return(plot +
     # LW
     geom_segment(data = plot_build, aes(x = xmin, xend = xmin, y = ymin, yend = ymax, colour = ifelse(plot_build$xmin > quartile_values[[1]], "red", ifelse(plot_build$xmin == quartile_values[[1]], "blue", "darkgreen"))), size = 1) +
     # Q1
     geom_segment(data = plot_build, aes(x = xlower, xend = xlower, y = ymin, yend = ymax, colour = ifelse(plot_build$xlower > quartile_values[[2]], "red", ifelse(plot_build$xlower == quartile_values[[2]], "blue", "darkgreen"))), size = 1) +
     # Median
     geom_segment(data = plot_build, aes(x = xmiddle, xend = xmiddle, y = ymin, yend = ymax, colour = ifelse(plot_build$xmiddle > quartile_values[[3]], "red", ifelse(plot_build$xmiddle == quartile_values[[3]], "blue", "darkgreen"))), size = 1) +
     # Q3
     geom_segment(data = plot_build, aes(x = xupper, xend = xupper, y = ymin, yend = ymax, colour = ifelse(plot_build$xupper > quartile_values[[4]], "red", ifelse(plot_build$xupper == quartile_values[[4]], "blue", "darkgreen"))), size = 1) +
     # UW
     geom_segment(data = plot_build, aes(x = xmax, xend = xmax, y = ymin, yend = ymax, colour = ifelse(plot_build$xmax > quartile_values[[5]], "red", ifelse(plot_build$xmax == quartile_values[[5]], "blue", "darkgreen"))), size = 1) +
     # Legend
     scale_color_manual(breaks = c("darkgreen", "blue", "red"), values = c("darkgreen", "blue", "red"), labels = c("Lower", "Equal", "Higher")) +
     theme(legend.position = "bottom", legend.box = "horizontal") +
     labs(colour = "Value comparison to reference: ")
  )
}
