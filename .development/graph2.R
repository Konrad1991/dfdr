# Operation definitions
operations <- list(
  add = list(
    forward = function(l) l[1] + l[2],
    backward = function(l) {
      grad <- l[[1]]
      list(grad, grad)
    }
  ),
  sub = list( # TODO:
    forward = function(l) l[1] - l[2],
    backward = function(l) list(l[1], -l[1]) # grad, inputs
  ),
  mul = list(
    forward = function(l) {
      l[1] * l[2]
    },
    backward = function(l) {
      inputs <- l[2:3]
      grad <- l[[1]]
      list(grad * inputs[[2]]@value, grad * inputs[[1]]@value)
    }
  ),
  div = list( # TODO:
    forward = function(l) l[1] / l[2],
    backward = function(l) list(l[1] / l[2][[2]], -l[1] * l[2][[1]] / (l[2][[2]]^2))
  ),
  forward = list( # TODO:Required?
    forward = function(l) l,
    backward = function(l) list(l[[1]])
  )
)

# Node definition
setClass(
  "Node",
  slots = c(
    name = "character",
    connected_nodes = "ANY",
    value = "ANY",
    deriv = "ANY",
    operation = "ANY",
    value_call = "ANY",
    deriv_call = "ANY"
  )
)

# Create an empty graph
create_empty_graph <- function() {
  list()
}

# Add node to the graph
add_node <- function(graph, node_name, connected_nodes = character(0), operation = NULL, value = NA) {
  stopifnot(is.character(node_name))
  node <- new("Node",
    name = node_name,
    connected_nodes = connected_nodes,
    value = value,
    deriv = 0,
    operation = operation,
    value_call = if (!is.null(operation)) {
      function(gr) {
        inputs <- lapply(connected_nodes, function(n) gr[[n]]@value)
        if (operation == "forward") {
          return(inputs[[1]])
        }
        operations[[operation]]$forward(c(inputs[[1]], inputs[[2]]))
      }
    } else {
      NULL
    },
    deriv_call = if (!is.null(operation)) {
      function(gr) {
        inputs <- lapply(connected_nodes, function(n) gr[[n]])
        grads <- operations[[operation]]$backward(c(gr[[node_name]]@deriv, inputs))
        for (i in seq_along(connected_nodes)) {
          gr[[connected_nodes[i]]]@deriv <- gr[[connected_nodes[i]]]@deriv + grads[[i]]
        }
        gr
      }
    } else {
      NULL
    }
  )
  graph[[node_name]] <- node
  return(graph)
}

# Topological sort (to process nodes in the correct order)
topological_sort <- function(graph) {
  visited <- character(0)
  stack <- character(0)

  visit <- function(node_name) {
    if (node_name %in% visited) {
      return()
    }
    visited <<- c(visited, node_name)
    if (!is.null(graph[[node_name]])) {
      for (neighbor in graph[[node_name]]@connected_nodes) {
        visit(neighbor)
      }
    }
    stack <<- c(stack, node_name)
  }

  for (node_name in names(graph)) {
    visit(node_name)
  }
  rev(stack) # Reverse for forward pass
}

# Forward pass
compute_values <- function(graph) {
  sorted_nodes <- topological_sort(graph) |> rev()
  sorted_nodes <- sorted_nodes[!sorted_nodes %in% c("x1", "x2")]
  for (node_name in sorted_nodes) {
    if (!is.null(graph[[node_name]]@value_call)) {
      graph[[node_name]]@value <- graph[[node_name]]@value_call(graph)
    }
  }
  return(graph)
}

# Backward pass
compute_derivatives <- function(graph) {
  sorted_nodes <- topological_sort(graph)
  graph[["y"]]@deriv <- 1 # Initialize derivative at the output node
  for (node_name in sorted_nodes) {
    if (!is.null(graph[[node_name]]@deriv_call)) {
      graph <- graph[[node_name]]@deriv_call(graph)
    }
  }
  return(graph)
}

# Print the graph
print_graph <- function(graph) {
  for (node in names(graph)) {
    cat(
      node, "->", paste(graph[[node]]@connected_nodes, collapse = ", "), "\n",
      "Value:", graph[[node]]@value, "Derivative:", graph[[node]]@deriv, "\n"
    )
  }
}

# Example: Build the graph
# y = a * b + a
# a = x1
# b = x1 + x2

graph <- create_empty_graph()
graph <- add_node(graph, "x1", value = 4, operation = "forward")
graph <- add_node(graph, "x2", value = 2, operation = "forward")
graph <- add_node(graph, "a", c("x1"), operation = "forward") # a = x1
graph <- add_node(graph, "b", c("x1", "x2"), operation = "add") # b = x1 + x2
graph <- add_node(graph, "mul", c("a", "b"), operation = "mul") # a * b
graph <- add_node(graph, "y", c("mul", "a"), operation = "add") # y = a * b + a

# Compute values and derivatives
graph <- compute_values(graph)
graph <- compute_derivatives(graph)

# Print the graph
print_graph(graph)
graph
# PLAN:
# - 1. Move to R6
# - 2. create graph from one line from AST
#   --> recursivly traverse ast
#   --> add each function call as node
#   --> e.g. x * y ==> add_node(graph, "mul1", c("x", "y"), operation = "mul")
#   --> Increment for each occurance of an operation or variable the counter for this entity
# - 3. How to create graph across multiple lines?
#   --> loop over body and add it to the graph
# - 4. How to handle if
#   --> store for each path (if, else if etc.) a graph
#   --> chose the correct path based on the if condition
# - 5. Can I replace (at least parts)
#   of the value_call and deriv_call
#   with symbolic derivatives?
#   --> Complicated. First understand the current approach better.
# - 6. What can I calculate during preprocessing?
#   --> How can I define the graph as string?
# - 7. How to handle subsetting?
#   --> Is this basically the same as for if branches?
