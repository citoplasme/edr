#' Creates two data frames, one composed of edges and other of vertices that form a graph based on a set of rules.
#' @description
#' This function creates two data frames. One is composed of all the edges that define the graph, in the form \code{from}-\code{to}.
#' The vertex data frame is more detailed, containing identifiers, labels, shapes, colors, etc..
#' The color of each vertex is calculated based on its median compares to the input \code{reference_value}.
#' @seealso [edr::plot_graph()] to understand how the output of this function is used graphically.
#' @param rules A data frame of rules composed by \code{feature_conditions}, \code{antecedent_support}, \code{p_value}, \code{kurtosis}, \code{skewness}, \code{mean}, \code{mode}, \code{median} and \code{standard_deviation}.
#' @param reference_value A numeric value used to color the subgroup vertices based on their medians.
#' @return A named list of two data frames: \code{vertices}, being composed of all graph vertices, and \code{edges}, containing all connections between vertices.
#' @examples
#' create_graph(example_rules, 0.5)
#' @import dplyr
#' @export
create_graph <- function(rules, reference_value) {
  # Initialize edge data frame with column names
  edges <- data.frame(data.frame(matrix(ncol = 2, nrow = 0)))
  colnames(edges) <- c("from", "to")
  # Initialize vertex data frame with column names
  vertices <- data.frame(matrix(ncol = 8, nrow = 0))
  colnames(vertices) <- c("id", "label", "group", "value", "shape", "title", "color", "shadow")

  # Populate both data frames using the rules
  for (i in seq_len(nrow(rules))) {
    # Subgroup
    subgroup_text <- paste(rules[i, ]$feature_conditions %>% unlist() %>% sort(), collapse = ", ")
    if (subgroup_text %in% vertices$label == FALSE) {
      title <- paste0("<p><b>", subgroup_text,
                      "</b><br></p><hr>",
                      "<p style='font-size: 14px;'><b>Antecedent Support:</b> ",
                      as.character(rules[i, ]$antecedent_support), "</p>",
                      "<p style='font-size: 14px;'><b>P-value:</b> ",
                      as.character(rules[i, ]$p_value), "</p>",
                      "<p style='font-size: 14px;'><b>Kurtosis:</b> ",
                      as.character(rules[i, ]$kurtosis), "</p>",
                      "<p style='font-size: 14px;'><b>Skewness:</b> ",
                      as.character(rules[i, ]$skewness), "</p>",
                      "<p style='font-size: 14px;'><b>Mean:</b> ",
                      as.character(rules[i, ]$mean), "</p>",
                      "<p style='font-size: 14px;'><b>Mode:</b> ",
                      as.character(rules[i, ]$mode), "</p>",
                      "<p style='font-size: 14px;'><b>Median:</b> ",
                      as.character(rules[i, ]$median), "</p>",
                      "<p style='font-size: 14px;'><b>Standard Deviation:</b> ",
                      as.character(rules[i, ]$standard_deviation), "</p>"
      )
      # Node color is based on the reference value
      color <- if (reference_value >= rules[i, ]$median) {"blue"} else {"red"}
      # Add vertex to data frame
      vertices[nrow(vertices) + 1, ] <- c(nrow(vertices) + 1, subgroup_text, "Subgroup", 1, "diamond", title, color, TRUE)
    }

    # Individual conditions
    feature_conditions <- rules[i, ]$feature_conditions %>% unlist(use.names = FALSE)
    for (j in seq_len(length(feature_conditions))) {
      feature_condition <- feature_conditions[j]
      # If condition not in data frame
      if (feature_condition %in% vertices$label == FALSE) {
        title <- paste0("<p><b>", feature_condition, "</b><br></p>")
        # Add vertex to data frame
        vertices[nrow(vertices) + 1, ] <- c(nrow(vertices) + 1, feature_condition, "Condition", 1, "ellipse", title, "orange", TRUE)
      }
      # Add edge to data frame
      edges[nrow(edges) + 1, ] <- c(vertices[vertices$label == feature_condition, ]$id, vertices[vertices$label == subgroup_text, ]$id)
    }
  }
  return(list(vertices = vertices, edges = edges))
}

#' Plots an interactive graph based on a data frame of vertices and a data frame of edges.
#' @description
#' This function creates an interactive visualization of the rules as a graph.
#' It uses the library \code{visNetwork} for the graphical component.
#' @seealso [edr::create_graph()] to understand how the inputs of this function are calculated.
#' @param vertices A data frame the contains \code{from} and \code{to} columns.
#' @param edges A data frame the contains \code{id}, \code{label}, \code{group}, \code{value}, \code{shape}, \code{title}, \code{color} and \code{shadow} columns.
#' @return An interactive \code{visNetwork} graph of input rules.
#' @examples
#' graph_data <- example_rules %>% create_graph(0.5)
#' plot_graph(graph_data$vertices, graph_data$edges)
#' @import dplyr
#' @import igraph
#' @import visNetwork
#' @export
plot_graph <- function(vertices, edges) {
  visNetwork(vertices, edges) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, algorithm = "hierarchical"), nodesIdSelection = TRUE) %>%
    visEdges(arrows = "to", color = "gray") %>%
    visIgraphLayout() %>% return()
}
