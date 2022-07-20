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
#' @import visNetwork
#' @export
plot_graph <- function(vertices, edges) {
  visNetwork(vertices, edges) %>%
    visOptions(highlightNearest = list(enabled = TRUE, hover = TRUE, algorithm = "hierarchical"), nodesIdSelection = TRUE) %>%
    visEdges(arrows = "to", color = "gray") %>%
    visIgraphLayout() %>% return()
}
