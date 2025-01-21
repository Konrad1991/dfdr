Node <- R6::R6Class(
  "Node",
  public = list(
    name = NULL,
    connected_nodes = NULL,
    value = NA,
    deriv = NA,
    value_call = NULL,
    deriv_call = NULL,
    initialize = function(name, connected_nodes, value) {
      self$name <- name
      self$connected_nodes <- connected_nodes
      self$value <- value
    }
  )
)

Graph <- R6::R6Class(
  "Graph",
  public = list(
    Nodes = list(),
    visited = character(0),
    # Traverse the graph
    # Depth-first search
    dfs = function(node) {
      if (inherits(node, "Node")) {
        node <- node$name
      }
      if (node %in% self$visited) {
        return()
      }
      self$visited <- c(self$visited, node)
      for (neighbor in names(Nodes)) {
        self$visited <- dfs(neighbor)
      }
    },
    add_node = function(node_name, connected_nodes, value) {
      stopifnot(is.character(node_name))
      node <- Node$new(node_name, connected_nodes, value)
      self$Nodes[[node_name]] <- node
    },
    print = function() {
      for (node in names(Nodes)) {
        cat(
          node, "->",
          paste(Nodes[[node]]@connected_nodes,
            collapse = ", "
          ), "\n"
        )
      }
    }
  )
)

# Example
# a <- x1
# b <- x1 + x2
# y <- a * b
graph <- create_empty_graph()
graph <- add_node(graph, "x1", c("a", "b"), 4, 0)
graph <- add_node(graph, "x2", "b", 2, 0)
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
    rev(stack)
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

graph_evaluated <- compute_values(graph)
# Check computed values
for (node_name in names(graph_evaluated)) {
  cat(node_name, "=", graph_evaluated[[node_name]]@value, "\n")
}
print_graph(graph_evaluated)

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

# Backward pass
graph[["y"]]@deriv_call <- function(gr) {
  gr[["a"]]@deriv <- gr[["a"]]@deriv + gr[["b"]]@value * gr[["y"]]@deriv
  gr[["b"]]@deriv <- gr[["b"]]@deriv + gr[["a"]]@value * gr[["y"]]@deriv
  return(gr)
}

graph[["b"]]@deriv_call <- function(gr) {
  gr[["x1"]]@deriv <- gr[["x1"]]@deriv + 1 * gr[["b"]]@deriv
  gr[["x2"]]@deriv <- gr[["x2"]]@deriv + 1 * gr[["b"]]@deriv
  return(gr)
}

graph[["a"]]@deriv_call <- function(gr) {
  gr[["x1"]]@deriv <- gr[["x1"]]@deriv + 1 * gr[["a"]]@deriv
  return(gr)
}

compute_derivatives <- function(graph) {
  graph[["y"]]@deriv <- 1
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

    stack <- stack[stack != "END"]
    stack <- stack[stack != "x1"]
    stack <- stack[stack != "x2"]
    stack
  }
  sorted_nodes <- topological_sort(graph)
  print(sorted_nodes)
  for (node_name in sorted_nodes) {
    if (!is.null(graph[[node_name]])) {
      temp <- graph[[node_name]]@deriv_call(graph)
      print(temp[[node_name]]@deriv)
      graph <- temp
    }
  }
  return(graph)
}

# Compute derivatives
graph <- compute_values(graph)
print_graph(graph)
graph_with_derivatives <- compute_derivatives(graph)

# Print results
for (node_name in names(graph_with_derivatives)) {
  cat(
    node_name, ": value =", graph_with_derivatives[[node_name]]@value,
    ", derivative =", graph_with_derivatives[[node_name]]@deriv, "\n"
  )
}
# Example
# a <- x1
# b <- x1 + x2
# y <- a * b
# dy/dx1 = x1 * (x1 + x2) = x1^2 + x1*x2 = 10
# dy/dx2 = x1 = 4
