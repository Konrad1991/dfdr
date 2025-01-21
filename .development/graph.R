operations <- list(
  add = list(
    forward = function(a, b) a + b,
    backward = function(grad, inputs) list(grad, grad)
  ),
  sub = list(
    forward = function(a, b) a - b,
    backward = function(grad, inputs) list(grad, -grad)
  ),
  mul = list(
    forward = function(a, b) a * b,
    backward = function(grad, inputs) list(grad * inputs[[2]], grad * inputs[[1]])
  ),
  div = list(
    forward = function(a, b) a / b,
    backward = function(grad, inputs) list(grad / inputs[[2]], -grad * inputs[[1]] / (inputs[[2]]^2))
  )
)

setClass(
  "Node",
  slots = c(
    name = "character",
    connected_nodes = "ANY",
    value = "numeric",
    deriv = "numeric",
    operation = "character",
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

add_node <- function(graph, node_name, connected_nodes, operation, value) {
  stopifnot(is.character(node_name))
  node <- new("Node",
    name = node_name,
    connected_nodes = connected_nodes,
    value = as.numeric(value),
    operation = operation
  )
  # Generate value_call
  if (!is.null(operation)) {
    node@value_call <- function(gr) {
      inputs <- lapply(node@connected_nodes, function(n) gr[[n]]@value)
      operations[[operation]]$forward(inputs[[1]], inputs[[2]])
    }
  }
  # Generate deriv_call
  if (!is.null(operation)) {
    node@deriv_call <- function(gr) {
      inputs <- lapply(node@connected_nodes, function(n) gr[[n]])
      grads <- operations[[operation]]$backward(gr[[node_name]]@deriv, inputs)
      for (i in seq_along(node@connected_nodes)) {
        gr[[node@connected_nodes[i]]]@deriv <- gr[[node@connected_nodes[i]]]@deriv + grads[[i]]
      }
      gr
    }
  }
  graph[[node_name]] <- node
  return(graph)
}

# Print
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
graph <- add_node(graph, "x1", c("a", "b"), "", 4)
graph <- add_node(graph, "x2", "b", "", 2)
graph <- add_node(graph, "a", "y", "", NA)
graph <- add_node(graph, "b", "y", "add", NA)
graph <- add_node(graph, "y", "END", "mul", NA)

# Forward pass
# graph[["x1"]]@value_call <- function(gr) {
#   return(4)
# }
# graph[["x2"]]@value_call <- function(gr) {
#   return(2)
# }
# graph[["a"]]@value_call <- function(gr) {
#   return(gr[["x1"]]@value)
# }
# graph[["b"]]@value_call <- function(gr) {
#   return(gr[["x1"]]@value + gr[["x2"]]@value)
# }
# graph[["y"]]@value_call <- function(gr) {
#   return(gr[["a"]]@value * gr[["b"]]@value)
# }

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

# Backward pass
# graph[["y"]]@deriv_call <- function(gr) {
#   gr[["a"]]@deriv <- gr[["a"]]@deriv + gr[["b"]]@value * gr[["y"]]@deriv
#   gr[["b"]]@deriv <- gr[["b"]]@deriv + gr[["a"]]@value * gr[["y"]]@deriv
#   return(gr)
# }
#
# graph[["b"]]@deriv_call <- function(gr) {
#   gr[["x1"]]@deriv <- gr[["x1"]]@deriv + 1 * gr[["b"]]@deriv
#   gr[["x2"]]@deriv <- gr[["x2"]]@deriv + 1 * gr[["b"]]@deriv
#   return(gr)
# }
#
# graph[["a"]]@deriv_call <- function(gr) {
#   gr[["x1"]]@deriv <- gr[["x1"]]@deriv + 1 * gr[["a"]]@deriv
#   return(gr)
# }

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
  for (node_name in sorted_nodes) {
    if (!is.null(graph[[node_name]])) {
      print(graph[[node_name]])
      temp <- graph[[node_name]]@deriv_call(graph)
      graph <- temp
    }
  }
  return(graph)
}

# Compute derivatives
graph <- compute_values(graph)
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
