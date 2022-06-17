# antecedent_support, p_value, kurtosis, skewness, mean, median, mode, standard_deviation, distribution_values, feature_conditions
# Number, Number, Number, Number, Number, Number, Number, Number, List(Number), List(String)

###############################################################################
############################ Graphical Analysis ###############################
###############################################################################


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
#' @import gridExtra
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
