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


#load("/Users/joaopimentel/Documents/GitHub/edr_package/edr/test_data_v2.RData")

#t %>% calculate_quantiles()

# id, conditions => expand = id, condition => id, shared_ids

#t %>% head(10) %>% select(feature_conditions) %>% tibble::rowid_to_column("id") %>%
#  tidyr::unnest(feature_conditions) %>% group_by(feature_conditions) %>%
#  summarize(ids = id %>% unique() %>% sort() %>% paste(collapse = ","))
#

# crossing()

# id, quantiles

# 1 - quantiles -> comparison to reference
# 2 - rules -> similarities
# 3 -

# -1 = smaller than reference
# 0 = equal to reference
# 1 = higher than reference

#t %>% head(10) %>% calculate_quantiles() %>% counter_factual_subgroups(reference_quantiles = boxplot.stats(t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE))$stats)

# import tidyr

# counter_factual_subgroups <- function(rules, reference_quantiles) {
#   # New list to store variations of each rule (data frames)
#   variations <- list()
#   # Loop over every rule
#   for (i in seq_len(nrow(rules))) {
#     # Initialize an auxiliary data frame to store variations of the i-th rule
#     aux <- data.frame(matrix(ncol = 2, nrow = 0))
#     colnames(aux) <- c("feature_conditions", "number_of_similarities")
#     # Loop over the rules
#     for (j in seq_len(nrow(rules))) {
#       # If the rule being compared is the same (i-th), the loop skips it
#       if (i == j) {
#         next
#       }
#       # Otherwise the rule is to be analyzed
#       else {
#         # Check if elements share some condition
#         mutual <- intersect(rules[i, ] %>% pull(feature_conditions) %>% unlist(), rules[j, ] %>% pull(feature_conditions) %>% unlist())
#         # Calculate the length of the shared conditions
#         mutual_length <- length(mutual)
#         # In case the rules shared 1 or more characteristics
#         if (mutual_length > 0) {
#           # Store the name of the subgroup, number of similarities and its quantile values
#           aux[nrow(aux) + 1, ] <- c(rules[j, ] %>% pull(feature_conditions) %>% list(), mutual_length)
#         }
#       }
#     }
#     # Sort auxiliary data frame by number of similarities (conditions) and add it to the list of variations
#     variations[[i]] <- aux %>% arrange(desc(number_of_similarities))
#     # Update how each quantile compares to the reference
#     rules[i, ]$lw <- ifelse(rules[i, ]$lw > reference_quantiles[1], 1, ifelse(rules[i, ]$lw == reference_quantiles[1], 0, -1))
#     rules[i, ]$q1 <- ifelse(rules[i, ]$q1 > reference_quantiles[2], 1, ifelse(rules[i, ]$q1 == reference_quantiles[2], 0, -1))
#     rules[i, ]$median <- ifelse(rules[i, ]$median > reference_quantiles[3], 1, ifelse(rules[i, ]$median == reference_quantiles[3], 0, -1))
#     rules[i, ]$q3 <- ifelse(rules[i, ]$q3 > reference_quantiles[4], 1, ifelse(rules[i, ]$q3 == reference_quantiles[4], 0, -1))
#     rules[i, ]$uw <- ifelse(rules[i, ]$uw > reference_quantiles[5], 1, ifelse(rules[i, ]$uw == reference_quantiles[5], 0, -1))
#   }
#
#   return(list(variations = variations, rules = rules))
#   # # Initialize vector of instances to drop
#   # instances_to_drop <- c()
#   # # Initialize list of performance of rules
#   # performance_rules <- list()
#   # # Loop over every rule
#   # for (i in seq_len(nrow(rules))) {
#   #   # initialize vector of rule performance
#   #   rule_performance <- c()
#   #   # Calculate performance of rule based on reference distribution
#   #   for (l in seq_len(length(df[i, ][-1]))) {
#   #     rule_performance <- c(rule_performance, cell_colors(
#   #       as.numeric(as.character(df[i, ][l + 1])),
#   #       as.numeric(as.character(reference_distribution[l + 1]))))
#   #   }
#   #   # Append rule performance to list
#   #   performance_rules[[i]] <- rule_performance
#   #   # Initialize vector of rows to drop (Not different from parent node)
#   #   uninteresting_indexes <- c()
#   #   # Loop over every variation associated with the rule
#   #   for (j in seq_len(nrow(variations[[i]]))) {
#   #     # Loop over every quantile of the respective variation
#   #     for (k in seq_len(length(variations[[i]][j, ][-1]))) {
#   #       # Values are compared with the reference distribution
#   #       variations[[i]][j, ][k + 1] <- cell_colors(
#   #         as.numeric(as.character(variations[[i]][j, ][k + 1])),
#   #         as.numeric(as.character(reference_distribution[k + 1])))
#   #     }
#   #     # Check if performance is exactly the same as parent node and add to vec
#   #     uninteresting_indexes <- c(uninteresting_indexes,
#   #                                all(rule_performance == variations[[i]][j, ][-1]))
#   #   }
#   #   # Check if there are rows to delete
#   #   if (length(uninteresting_indexes > 0)) {
#   #     # Drop uninteresting indexes
#   #     variations[[i]] <- variations[[i]][!uninteresting_indexes, ]
#   #   }
#   #   # Check if instance has more at least one interesting variation
#   #   instances_to_drop <- c(instances_to_drop, nrow(variations[[i]]) == 0)
#   # }
#   # # Associate names of parent rules to each dataframe in list
#   # names(variations) <- get_subgroups(df)
#   # # Associate names of subgroups to their performance
#   # names(performance_rules) <- names(variations)
#   #
#   # # Drop instances with no variations
#   # variations <- variations[!instances_to_drop]
#   # # Return the list with the interesting instances
#   # return(list(subgroups = performance_rules,
#   #             interesting_instances = variations))
# }
#
# TEST
#t %>% head(10) %>% calculate_quantiles() %>% counter_factual_subgroups(reference_distribution = t %>% tail(1) %>% pull(distribution_values) %>% unlist(use.names = FALSE))
#
# intersect(t[1, ]$Subgroup %>% unlist(), t[2, ]$Subgroup %>% unlist())
