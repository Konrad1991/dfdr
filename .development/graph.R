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
# - Print

setClass(
  "Node",
  slots = c(
    name = "character",
    connected_nodes = "ANY",
    value = "numeric",
    deriv = "numeric",
    value_call = "function",
    deriv_call = "function"
  )
)

create_empty_graph <- function() {
  list()
}

# Traverse the graph
# Depth-first search
dfs <- function(graph, node, visited = character(0)) {
  if (inherits(node, "Node")) {
    node <- node@name
  }
  if (node %in% visited) {
    return(visited)
  }
  visited <- c(visited, node)
  for (neighbor in names(graph)) {
    visited <- dfs(graph, neighbor, visited)
  }
  visited
}

add_node <- function(graph, node_name, connected_nodes, value, deriv) {
  stopifnot(is.character(node_name))
  node <- new("Node",
    name = node_name,
    connected_nodes = connected_nodes,
    value = as.numeric(value), deriv = as.numeric(deriv)
  )
  graph[[node_name]] <- node
  return(graph)
}

# Plot
print_graph <- function(graph) {
  for (node in names(graph)) {
    if (is.null(graph[[node]])) {
      next
    }
    cat(node, "->", paste(graph[[node]]@connected_nodes, collapse = ", "), "\n")
  }
}

# Example
# a <- x1
# b <- x1 + x2
# y <- a * b
graph <- create_empty_graph()
graph <- add_node(graph, "x1", c("a", "b"), 4, 1)
graph <- add_node(graph, "x2", "b", 2, 1)
graph <- add_node(graph, "a", "y", NA, 0)
graph <- add_node(graph, "b", "y", NA, 0)
graph <- add_node(graph, "y", "END", NA, 0)

# Forward pass
graph[["x1"]]@value_call <- function(gr) {
  return(4)
}
graph[["x2"]]@value_call <- function(gr) {
  return(2)
}
graph[["a"]]@value_call <- function(gr) {
  return(gr[["x1"]]@value)
}
graph[["b"]]@value_call <- function(gr) {
  return(gr[["x1"]]@value + gr[["x2"]]@value)
}
graph[["y"]]@value_call <- function(gr) {
  return(gr[["a"]]@value * gr[["b"]]@value)
}

print_graph(graph)

# Perform the forward pass
compute_values <- function(graph) {
  topological_sort <- function(graph) {
    visited <- character(0L)
    stack <- character(0L)

    visit <- function(node_name) {
      if (node_name %in% visited) {
        return()
      }
      visited <<- c(visited, node_name)
      if (!is.null(graph[[node_name]])) {
        for (connected_node in graph[[node_name]]@connected_nodes) {
          visit(connected_node)
        }
      }
      stack <<- c(stack, node_name)
    }

    for (node_name in names(graph)) {
      visit(node_name)
    }

    rev(stack) # Reverse the stack for topological order
  }

  sorted_nodes <- topological_sort(graph)
  # Compute values in topological order
  for (node_name in sorted_nodes) {
    if (!is.null(graph[[node_name]])) {
      graph[[node_name]]@value <- graph[[node_name]]@value_call(graph)
    }
  }
  return(graph)
}

graph <- compute_values(graph)
# Check computed values
for (node_name in names(graph)) {
  cat(node_name, "=", graph[[node_name]]@value, "\n")
}

# Invert graph
invert_graph <- function(graph) {
  inverted_graph <- create_empty_graph()
  for (node in names(graph)) {
    for (neighbor in graph[[node]]@connected_nodes) {
      node_temp <- new("Node",
        name = neighbor,
        connected_nodes = graph[[node]]@name,
        value = graph[[node]]@value, deriv = graph[[node]]@deriv
      )
      if (is.null(inverted_graph[[neighbor]])) {
        inverted_graph[[neighbor]] <- node_temp
      } else {
        inverted_graph[[neighbor]]@connected_nodes <- c(
          inverted_graph[[neighbor]]@connected_nodes,
          node_temp@connected_nodes
        )
      }
    }
  }
  inverted_graph
}

inverted_graph <- invert_graph(graph)
print_graph(graph)
print_graph(inverted_graph)
str(inverted_graph)
dfs(inverted_graph, "y")

# Backward pass
graph[["x1"]]@deriv_call <- function(gr) {
  return(1)
}
graph[["x2"]]@deriv_call <- function(gr) {
  return(1)
}
graph[["a"]]@deriv_call <- function(gr) {
  return(1)
}
graph[["b"]]@deriv_call <- function(gr) {
  return(gr[["x1"]]@value + gr[["x2"]]@value)
}
graph[["y"]]@deriv_call <- function(gr) {
  return(gr[["a"]]@value * gr[["b"]]@value)
}

# Find the paths for x1
# dy/da = 1
#


dfs_all_paths <- function(graph, node, current_path = character(0L), all_paths = list()) {
  current_path <- c(current_path, node)
  if (is.null(graph[[node]]) || length(graph[[node]]@connected_nodes) == 0) {
    # INFO: Found a leaf --> Node at the end of a path
    all_paths <- append(all_paths, list(current_path))
  } else {
    # Visit all neighbours
    for (neighbour in graph[[node]]@connected_nodes) {
      all_paths <- dfs_all_paths(graph, neighbour, current_path, all_paths)
    }
  }
  return(all_paths)
}

dfs_all_paths(inverted_graph, "y")
dfs_all_paths(graph, "x1")
