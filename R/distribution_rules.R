#' List all subgroups in an orderly manner.
#' @description
#' This function returns a vector of strings, each representing the feature conditions that define a subgroup.
#' @param rules A data frame that contains a list column named \code{feature_conditions}.
#' @return A vector of strings representing the feature conditions that define the subgroups in the input data frame.
#' @examples
#' get_subgroups(rules)
#' @import dplyr
#' @export
get_subgroups <- function(rules) {
  rules %>% select(feature_conditions) %>%
    mutate(feature_conditions = paste(feature_conditions %>% unlist() %>% sort(), collapse = ", ")) %>%
    pull(feature_conditions) %>% sort() %>% return()
}

#' Converts the distribution outputted from Caren to a numeric vector.
#' @description
#' This function converts the distribution outputted from Caren, which is a string, to a numeric vector.
#' Note that this functions is a variation of function from \href{https://www.dcc.fc.up.pt/~amjorge/software/carenR/R/caren_integrated.R}{CarenR}.
#' @param caren_distribution A string that follows the pattern: "error={value/count, ...}".
#' @return A numeric vector that replicates each value by its count.
#' @examples
#' caren_distribution_as_vector("error={1/2,3/1}")
#' @import dplyr
#' @import stringr
#' @import tibble
#' @export
caren_distribution_as_vector <- function(caren_distribution) {
  caren_distribution %>% str_replace_all(pattern = "[a-zA-Z\\=\\{\\}\\ ]", replacement = "") %>%
    str_split(pattern = ",") %>% unlist() %>% as_tibble_col(column_name = "data") %>%
    mutate(value = str_split_fixed(string = data, pattern = "/", n = 2)) %>% select(value) %>%
    mutate(count = value[,2] %>% as.numeric(digits = 15), value = value[,1] %>% as.numeric(digits = 15)) %>%
    rep(.$value, .$count) %>%
    return()
}

#' Filters data frame based on a regular expression applied to the feature conditions that define a subgroup.
#' @description
#' This function filters out all data items that do not match the input regular expression.
#' @param rules A data frame that contains a list column named \code{feature_conditions}.
#' @param regex A regular expression to filter the feature conditions in the input data frame.
#' @return A vector of strings representing the feature conditions that define the subgroups in the input data frame.
#' @examples
#' filter_subgroups(rules, regex = "lstat=]")
#' @import dplyr
#' @export
filter_subgroups <- function(rules, regex = "") {
  rules %>% select(feature_conditions) %>%
    mutate(feature_conditions = paste(feature_conditions %>% unlist() %>% sort(), collapse = ", ")) %>%
    filter(grepl(feature_conditions, pattern = regex, fixed = TRUE)) %>% return()
}
