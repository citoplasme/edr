#' Calculates error values based on the differences between two vectors containing real and predicted values
#' @description
#' This function calculates the error of a model based on receiving a vector with the predicted values and the real values.
#' It also considers the type of error that is being calculated, with three possible options: absolute, residual and logarithmic.
#' Note that this functions is a variation of function from \href{https://github.com/inesareosa/MScThesis}{Error Dependence Plots}.
#' @seealso [single_model_data()] to understand how this function is used in the context of altering a data frame.
#' @param real A vector that contains the real values of the property of interest.
#' @param predicted A vector that contains the predicted values by the model.
#' @param type A string that contains the type of error to be calculated. It can be "absolute", "residual" or "logarithmic".
#' @return A vector composed of the errors calculated from the differences between real and predicted values.
#' @examples
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2))
#' # returns c(1,2,1,1,1,3)
#'
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type="Absolute")
#' # returns c(1,2,1,1,1,3)
#'
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type="absolute")
#' # returns c(1,2,1,1,1,3)
#'
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type="AbSoLuTe")
#' # returns c(1,2,1,1,1,3)
#'
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type="Logarithmic")
#' # returns c(0.6931472,1.0986123,0.6931472,0.6931472,0.6931472,1.3862944)
#'
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type="logarithmic")
#' # returns c(0.6931472,1.0986123,0.6931472,0.6931472,0.6931472,1.3862944)
#'
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type="lOgARITHmic")
#' # returns c(0.6931472,1.0986123,0.6931472,0.6931472,0.6931472,1.3862944)
#'
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type="Residual")
#' # returns c(1,2,1,1,-1,3)
#'
#' calculate_error(c(1,2,3,4,5,5), c(0,0,2,3,6,2), type="ResiDual")
#' # returns c(1,2,1,1,-1,3)
#' @import dplyr
#' @export
calculate_error <- function(real, predicted, type = "Absolute") {
  switch(
    type %>% tolower(),
    "absolute" = abs(real - predicted),
    "logarithmic" = log(1 + abs(real - predicted)),
    "residual" = real - predicted,
    stop("Invalid error type. Type must be Absolute, Residual or Logarithmic.")
  ) %>% return()
}

#' Calculates the prediction errors of a model and adds it to the input data frame
#' @description
#' This function creates a data frame with feature values, the model's errors and predictions.
#' Note that this functions is a variation of function from \href{https://github.com/inesareosa/MScThesis}{Error Dependence Plots}.
#' @seealso [calculate_error()] to understand how error values are calculated.
#' @param df A data frame that contains feature values and a target column.
#' @param predicted A vector that contains the predicted values by the model.
#' @param feature_y A string that contains the name of the column that represents the target variable.
#' @param type A string that contains the type of error to be calculated. It can be "absolute", "residual" or "logarithmic".
#' @param output_predictions A boolean that defines if the output data frame will contain the predictions in a new column.
#' @param output_target A boolean that defines if the output data frame will contain the target values.
#' @return A data frame that contains the at least one extra column in the form of prediction errors based on the type of error selected.
#' @examples
#' single_model_data(df = iris, iris$Sepal.Length + runif(length(iris$Sepal.Length), -1, 1), feature_y = "Sepal.Length", type = "Logarithmic")
#'
#' single_model_data(df = iris, iris$Sepal.Length + runif(length(iris$Sepal.Length), -1, 1), feature_y = "Sepal.Length", type = "Logarithmic", output_predictions = FALSE, output_target = FALSE)
#'
#' single_model_data(df = iris, iris$Sepal.Length + runif(length(iris$Sepal.Length), -1, 1), feature_y = "Sepal.Length", type = "Absolute", output_predictions = FALSE, output_target = TRUE)
#'
#' single_model_data(df = iris, iris$Sepal.Length + runif(length(iris$Sepal.Length), -1, 1), feature_y = "Sepal.Length", type = "Residual", output_predictions = TRUE, output_target = FALSE)
#'
#' single_model_data(df = iris, iris$Sepal.Length + runif(length(iris$Sepal.Length), -1, 1), feature_y = "Sepal.Length", type = "Residual", output_predictions = TRUE, output_target = TRUE)
#' @import dplyr
#' @export
single_model_data <- function(df, predicted, feature_y, type = "Absolute", output_predictions = FALSE, output_target = FALSE) {
  y <- df %>% pull(feature_y)
  df %>% select(c(!matches(feature_y), if(isTRUE(output_target)) matches(feature_y) else NULL)) %>%
    mutate(
      predicted = if(isTRUE(output_predictions)) predicted else NULL,
      error = calculate_error(y, predicted, type)
    ) %>%
    return()
}
