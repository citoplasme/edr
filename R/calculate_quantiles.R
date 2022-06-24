#' Calculates the quantile values of every numeric distribution in a rule data frame.
#' @description
#' This function calculates the quantile values (lower-whisker, first-quartile, median, third-quartile and upper-whisker) of every numeric distribution in the rule data frame.
#' It outputs a data frame composed by the feature conditions and the quantile values, all in separate columns.
#' @param rules A data frame that contains a list column named \code{feature_conditions} and \code{distribution_values}.
#' @return A data frame that contains the feature conditions in \code{rules} and their respective quantile values based on \code{distribution_values}.
#' @examples
#' calculate_quantiles(rules)
#' @import dplyr
#' @export
calculate_quantiles <- function(rules) {
  rules %>% select(feature_conditions, distribution_values) %>% mutate(
    lw = boxplot.stats(distribution_values %>% unlist(use.names = FALSE))$stats[1],
    q1 = boxplot.stats(distribution_values %>% unlist(use.names = FALSE))$stats[2],
    median = boxplot.stats(distribution_values %>% unlist(use.names = FALSE))$stats[3],
    q3 = boxplot.stats(distribution_values %>% unlist(use.names = FALSE))$stats[4],
    uw = boxplot.stats(distribution_values %>% unlist(use.names = FALSE))$stats[5],
    distribution_values = NULL
  ) %>% return()
}
