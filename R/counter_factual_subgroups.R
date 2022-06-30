#' Calculates the counter factual subgroups of every subgroup in a rule data frame.
#' @description
#' This function calculates the counter factual subgroups of every subgroup in the rule data frame.
#' Taking an initial subgroup, a counter factual subgroup is one with a different set of feature conditions,
#' sharing at least one condition with the initial subgroup, but has an extreme opposite behavior.
#' It outputs a object list, in which each object is composed of its feature conditions in \code{feature_conditions},
#' performance metrics in comparison to the reference values in \code{performance} and a data frame of variation subgroups
#' that have contrasting performance while sharing at least one feature condition.
#' @param rules A data frame that contains a list column named \code{feature_conditions} and quantile values in the the following
#' columns: (\code{lw}, \code{q1}, \code{median}, \code{q3} and \code{uw}). This data frame can be obtained from the application of function
#' \code{edr::calculate_quantiles}.
#' @param reference_quantiles A vector that contains five float values that characterize the reference quantiles
#' (lower-whisker, first-quartile, median, third-quartile and upper-whisker).
#' @return A object list, in which each object is composed of its feature conditions in \code{feature_conditions},
#' performance metrics in comparison to the reference values in \code{performance} and a data frame of counter factual subgroups.
#' @examples
#' counter_factual_subgroups(example_rules %>% calculate_quantiles() %>% head(50), c(0.00742, 0.547, 0.926, 1.33, 2.42))
#' @import dplyr
#' @export
counter_factual_subgroups <- function(rules, reference_quantiles) {
  # New list to store counter factual subgroups of each rule (data frames)
  counter_factuals <- list()
  # Loop over every rule
  for (i in seq_len(nrow(rules))) {
    # Initialize an auxiliary data frame to store variations of the i-th rule
    aux <- data.frame(matrix(ncol = 7, nrow = 0))
    colnames(aux) <- c("feature_conditions", "number_of_similarities", "lw", "q1", "median", "q3", "uw")
    # Loop over the rules
    for (j in seq_len(nrow(rules))) {
      # If the rule being compared is the same (i-th), the loop skips it
      if (i == j) {
        next
      }
      # Otherwise the rule is to be analyzed
      else {
        # Check if elements share some condition
        mutual <- intersect(rules[i, ] %>% pull(feature_conditions) %>% unlist(), rules[j, ] %>% pull(feature_conditions) %>% unlist())
        # Calculate the length of the shared conditions
        mutual_length <- length(mutual)
        # In case the rules shared 1 or more characteristics
        if (mutual_length > 0) {
          # Store the name of the subgroup, number of similarities and its quantile value comparison to reference (-1 Lower, 0 Equal, 1 Higher)
          aux[nrow(aux) + 1, ] <- c(
            rules[j, ] %>% pull(feature_conditions) %>% list(),
            mutual_length,
            ifelse(rules[j, ]$lw > reference_quantiles[1], 1, ifelse(rules[j, ]$lw == reference_quantiles[1], 0, -1)),
            ifelse(rules[j, ]$q1 > reference_quantiles[2], 1, ifelse(rules[j, ]$q1 == reference_quantiles[2], 0, -1)),
            ifelse(rules[j, ]$median > reference_quantiles[3], 1, ifelse(rules[j, ]$median == reference_quantiles[3], 0, -1)),
            ifelse(rules[j, ]$q3 > reference_quantiles[4], 1, ifelse(rules[j, ]$q3 == reference_quantiles[4], 0, -1)),
            ifelse(rules[j, ]$uw > reference_quantiles[5], 1, ifelse(rules[j, ]$uw == reference_quantiles[5], 0, -1))
          )
        }
      }
    }
    counter_factuals[[i]] <- list(
      # Store feature conditions
      feature_conditons = rules[i, ]$feature_conditions %>% unlist(use.names = FALSE) %>% sort(),
      # Store how the rule performs in comparison to reference
      performance = list(
        lw = ifelse(rules[i, ]$lw > reference_quantiles[1], 1, ifelse(rules[i, ]$lw == reference_quantiles[1], 0, -1)),
        q1 = ifelse(rules[i, ]$q1 > reference_quantiles[2], 1, ifelse(rules[i, ]$q1 == reference_quantiles[2], 0, -1)),
        median = ifelse(rules[i, ]$median > reference_quantiles[3], 1, ifelse(rules[i, ]$median == reference_quantiles[3], 0, -1)),
        q3 = ifelse(rules[i, ]$q3 > reference_quantiles[4], 1, ifelse(rules[i, ]$q3 == reference_quantiles[4], 0, -1)),
        uw = ifelse(rules[i, ]$uw > reference_quantiles[5], 1, ifelse(rules[i, ]$uw == reference_quantiles[5], 0, -1))
      ),
      # Sort auxiliary data frame by number of similarities (conditions) and add it to the list of variations
      variations = aux %>% arrange(desc(number_of_similarities))
    )

    # Filter all variations that do not have a different performance from the base subgroup
    counter_factuals[[i]]$variations <- counter_factuals[[i]]$variations %>% filter(
      lw != counter_factuals[[i]]$performance$lw,
      q1 != counter_factuals[[i]]$performance$q1,
      median != counter_factuals[[i]]$performance$median,
      q3 != counter_factuals[[i]]$performance$q3,
      uw != counter_factuals[[i]]$performance$uw
    )
    # If the variation data frame is empty, the item is removed from the counter factual list
    if (counter_factuals[[i]]$variations %>% nrow() == 0) {
      counter_factuals <- counter_factuals[-i]
    }
  }
  # Remove all NULL values from the list
  counter_factuals[-which(sapply(counter_factuals, is.null))] %>% return()
}
