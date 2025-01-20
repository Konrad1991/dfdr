# Adjacent matrix representation of a graph is missing
# TODO: Graph
# - Create empty graph
# - Create adjacent matrix
# - Add node
# - add edge
# - remove edge
# - Traverse
#   - Depth-first search
#   - Breadth-first search
# - Plot

create_empty_graph <- function() {
  list()
}

# Traverse the graph
# Depth-first search
dfs <- function(graph, node, visited = character(0)) {
  if (node %in% visited) {
    return(visited)
  }
  visited <- c(visited, node)
  for (neighbor in graph[[node]]) {
    visited <- dfs(graph, neighbor, visited)
  }
  visited
}

# Breadth-first search
bfs <- function(graph, start) {
  queue <- c(start)
  visited <- character(0)
  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    if (!(node %in% visited)) {
      visited <- c(visited, node)
      queue <- c(queue, graph[[node]])
    }
  }
  visited
}

add_node <- function(graph, node_name, connected_nodes) {
  stopifnot(is.character(node_name))
  graph[[node_name]] <- connected_nodes
  return(graph)
}

remove_node <- function(graph, node_name, connected_nodes) {
  stopifnot(is.character(node_name))
  graph[[node_name]] <- NULL
  graph <- Filter(Negate(is.null), graph)
  # TODO: remove the edges to the removed node
  return(graph)
}

add_edge <- function(graph, node1, node2) {
  graph[[node1]] <- c(graph[[node1]], node2)
  return(graph)
}

remove_edge <- function(graph, node1, node2) {
  graph[[node1]] <- setdiff(graph[[node1]], node2)
  return(graph)
}

# Plot
print_graph <- function(graph) {
  for (node in names(graph)) {
    cat(node, "->", paste(graph[[node]], collapse = ", "), "\n")
  }
}

# Example
# a <- x1
# b <- x1 + x2
# y <- a * b
graph <- create_empty_graph()
graph <- add_node(graph, "x1", c("a", "b"))
graph <- add_node(graph, "x2", "b")
graph <- add_node(graph, "a", "y")
graph <- add_node(graph, "b", "y")

invert_graph <- function(graph) {
  inverted_graph <- create_empty_graph()
  for (node in names(graph)) {
    for (neighbor in graph[[node]]) {
      inverted_graph[[neighbor]] <- c(inverted_graph[[neighbor]], node)
    }
  }
  inverted_graph
}

inverted_graph <- invert_graph(graph)
print_graph(inverted_graph)
dfs(inverted_graph, "y")

dfs_all_paths <- function(graph, node, current_path = character(0L), all_paths = list()) {
  current_path <- c(current_path, node)
  if (is.null(graph[[node]])) {
    # INFO: Found a leaf --> Node at the end of a path
    all_paths <- append(all_paths, list(current_path))
  } else {
    # Visit all neighbours
    for (neighbour in graph[[node]]) {
      all_paths <- dfs_all_paths(graph, neighbour, current_path, all_paths)
    }
  }
  return(all_paths)
}

dfs_all_paths(inverted_graph, "y")
dfs_all_paths(graph, "x1")
