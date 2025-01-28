# PLAN:
# Code --> GRAPH --> AST_DERIV --> Code_DERIV
# When parsing GRAPH to AST_DERIV the RHS is assembled to one call
# This only works in ast2ast. As here each variable holds a
# value and a derivative. Thus, variables type resembles the
# Node class in R.
# 1. Add unary operations
# 2. How to handle if
#  - each if branch is stored in a special Node class
#  - the logical; the branch stored in a graph
# 3. add Node for constants
# 4. Add possibility that variables occure several times at lhs
# 5. Subsetted lhs
operations <- list(
  add = list(
    forward = function(a, b) {
      a + b
    },
    backward = function(grad, inputs) {
      list(grad, grad)
    }
  ),
  sub = list(
    forward = function(a, b) a - b,
    backward = function(grad, inputs) {
      list(grad, -grad)
    }
  ),
  mul = list(
    forward = function(a, b) {
      a * b
    },
    backward = function(grad, inputs) {
      list(grad * inputs[[2]]$value, grad * inputs[[1]]$value)
    }
  ),
  div = list(
    forward = function(a, b) a / b,
    backward = function(grad, inputs) {
      inputs <- l[2:3]
      grad <- l[[1]]
      list(
        grad / inputs[[2]]$value,
        -grad * inputs[[1]]$value / (inputs[[2]]$value^2)
      )
    }
  ),
  subsetting = list(
    forward = function(a, b) {
      a[b]
    },
    backward = function(grad, inputs) {
      grad_temp <- rep(0, length(inputs[[1]]$value))
      grad_temp[inputs[[2]]$value] <- grad
      list(grad_temp, 0)
    }
  ),
  concatenate = list(
    forward = function(...) {
      return(unlist(...))
    },
    backward = function(grad, inputs) {
      if (identical(grad, numeric(0))) {
        return(rep(0, length(inputs)))
      }
      res <- rep(grad, length(inputs)) |> as.list()
      return(res)
    }
  ),
  forward = list(
    forward = function(l) l,
    backward = function(grad, inputs) list(grad)
  ),
  forward_subsetting = list(
    forward = function(rhs, what) {
    }
  )
)

transf_fct_name <- function(name) {
  if (name == "+") {
    return("add")
  }
  if (name == "-") {
    return("sub")
  }
  if (name == "*") {
    return("mul")
  }
  if (name == "/") {
    return("div")
  }
  if (name == "[") {
    return("subsetting")
  }
  if (name == "c") {
    return("concatenate")
  }
  name
}

Node <- R6::R6Class(
  "Node",
  public = list(
    name = NULL,
    connected_nodes = NULL,
    value = NA,
    deriv = NA,
    operation = NULL,
    value_call = NULL,
    deriv_call = NULL,
    initialize = function(name, connected_nodes, value, operation) {
      self$name <- name
      self$connected_nodes <- connected_nodes
      self$value <- value
      self$operation <- operation
      self$deriv <- 0
      self$value_call <- if (!is.null(operation)) {
        function(gr) {
          inputs <- lapply(connected_nodes, function(n) gr[[n]]$value)
          if (operation == "forward" && length(inputs) == 0) {
            return(value)
          } else if (operation == "forward" && length(inputs) == 1) {
            return(operations[[operation]]$forward(inputs[[1]]))
          } else if (operation == "concatenate") {
            return(operations[[operation]]$forward(inputs))
          } else if (operation == "forward_subsetting") {
            # subset lhs and rhs and then assign
            return(operations[[operation]]$forward(gr[[name]]$value, inputs))
          } else {
            return(operations[[operation]]$forward(inputs[[1]], inputs[[2]]))
          }
        }
      } else {
        NULL
      }
      self$deriv_call <- if (!is.null(operation)) {
        function(gr) {
          inputs <- lapply(connected_nodes, function(n) gr[[n]])
          grads <- operations[[operation]]$backward(
            gr[[name]]$deriv, inputs
          )
          for (i in seq_along(connected_nodes)) {
            gr[[connected_nodes[i]]]$deriv <-
              gr[[connected_nodes[i]]]$deriv + grads[[i]]
          }
          gr
        }
      } else {
        NULL
      }
    },
    print = function() {
      cat(
        self$name,
        "->", paste(self$connected_nodes, collapse = ", "), "\n",
        "Value:", self$value,
        "Derivative:", self$deriv, "\n",
        "operation:", self$operation, "\n\n"
      )
    }
  )
)

Graph <- R6::R6Class(
  "Graph",
  public = list(
    l = list(),
    visited = NULL,
    stack = NULL,
    initialize = function() {
      self$l <- list()
    },
    add_node = function(node_name, connected_nodes = character(0),
                        operation = NULL, value = NA) {
      node <- Node$new(
        name = node_name,
        connected_nodes = connected_nodes,
        value = value,
        operation = operation
      )
      self$l[[node_name]] <- node
    },
    # Topological sort (to process nodes in the correct order)
    topological_sort = function() {
      self$visited <- character(0)
      self$stack <- character(0)
      visit <- function(node_name) {
        if (node_name %in% self$visited) {
          return()
        }
        self$visited <- c(self$visited, node_name)
        if (!is.null(self$l[[node_name]])) {
          for (neighbor in self$l[[node_name]]$connected_nodes) {
            visit(neighbor)
          }
        }
        self$stack <- c(self$stack, node_name)
      }
      for (node_name in names(self$l)) {
        visit(node_name)
      }
      self$stack <- rev(self$stack)
    },
    forward_pass = function() { # INFO: calculate values
      sorted_nodes <- self$topological_sort() |> rev()
      for (node_name in sorted_nodes) {
        if (!is.null(self$l[[node_name]]$value_call)) {
          # PLAN: here check if node is "if" branch
          self$l[[node_name]]$value <- self$l[[node_name]]$value_call(self$l)
        }
      }
    },
    backward_pass = function(from_what) { # INFO: calculate derivatives
      sorted_nodes <- self$topological_sort()
      self$l[[from_what]]$deriv <- 1 # Initialize derivative at the output node
      for (node_name in sorted_nodes) {
        if (!is.null(self$l[[node_name]]$deriv_call)) {
          self$l <- self$l[[node_name]]$deriv_call(self$l)
        }
      }
    },
    print = function() {
      for (node in names(self$l)) {
        print(node)
        self$l[[node]]$print()
      }
    }
  )
)

is_assign <- function(line) {
  deparse(line[[1]]) %in% c("<-", "=")
}

is_concatenate <- function(line) {
  deparse(line[[1]]) == "c"
}

is_subset <- function(line) {
  deparse(line[[1]]) == "["
}

is_call <- function(line) {
  is.call(line)
}

create_name_forward <- function(line, env) {
  name <- deparse(line[[2]])
  if (!(name %in% env$variable_list)) {
    env$variable_list[[name]] <- 0
  }
  res <- paste(name, env$variable_list[[name]], sep = "")
  env$variable_list[[name]] <- env$variable_list[[name]] + 1
  res
}

add_forward <- function(line, env) {
  name <- deparse(line[[2]])
  value <- NA
  operation <- NULL
  value <- line[[3]]
  operation <- "forward"
  if (rlang::is_symbol(value)) {
    env$graph$add_node(name,
      connected_nodes = deparse(value),
      value = NA, operation = operation
    )
  } else {
    env$graph$add_node(name, value = value, operation = operation)
  }
}

elongate_connected_nodes <- function(env, connected_nodes) {
  if (length(env$graph$l) >= 1) {
    connected_nodes <- union(
      connected_nodes,
      env$graph$l[[length(env$graph$l)]]$name
    )
  }
  connected_nodes
}

create_name <- function(line, env) {
  name <- deparse(line[[1]])
  name <- transf_fct_name(name)
  res <- paste(name, env$counter_list[[name]], sep = "")
  env$counter_list[[name]] <- env$counter_list[[name]] + 1
  res
}

add_binary <- function(line, env) {
  fct <- create_name(line, env)
  connected_nodes <- NULL
  if (!is_call(line[[2]])) {
    connected_nodes <- deparse(line[[2]])
  } else {
    parse_line(line[[2]], env)
    connected_nodes <- elongate_connected_nodes(env, connected_nodes)
  }
  if (!is_call(line[[3]])) {
    connected_nodes <- union(
      connected_nodes,
      deparse(line[[3]])
    )
  } else {
    parse_line(line[[3]], env)
    connected_nodes <- elongate_connected_nodes(env, connected_nodes)
  }

  env$graph$add_node(fct,
    connected_nodes = connected_nodes,
    operation = deparse(line[[1]]) |> transf_fct_name()
  )
}

add_concatenate <- function(line, env) {
  fct <- create_name(line, env)
  connected_nodes <- NULL
  connected_nodes <- lapply(line[-1], function(x) {
    if (!is_call(x)) {
      connected_nodes <- union(connected_nodes, deparse(x))
    } else {
      parse_line(x, env)
      connected_nodes <- elongate_connected_nodes(env, connected_nodes)
    }
    return(connected_nodes)
  })
  env$graph$add_node(fct,
    connected_nodes = unlist(connected_nodes),
    operation = "concatenate"
  )
}

parse_line <- function(line, env) {
  if (!is.call(line)) {
    return(line)
  }
  line <- as.list(line)
  if (is_assign(line)) {
    if (!is_call(line[[3]])) {
      add_forward(line, env)
    } else {
      operation <- "forward"
      parse_line(line[[3]], env)
      prev_node1 <- env$graph$l[[length(env$graph$l)]]
      connected_nodes <- prev_node1$name
      name <- deparse(line[[2]])
      # name <- line |> create_name_forward(env)

      if (is_call(line[[2]])) {
        stopifnot("Only subsetted lhs is allowed" = is_subset(line[[2]]))
        parse_line(line[[2]], env)
        prev_node2 <- env$graph$l[[length(env$graph$l)]]
        connected_nodes <- union(connected_nodes, prev_node2$name)
        name <- prev_node2$connected_nodes[1] # First what is subsetted then the index
        operation <- "forward_subsetting"
      }
      env$graph$add_node(name,
        connected_nodes = connected_nodes,
        value = NA, operation = operation
      ) # TODO: handle subsetted lhs
    }
  } else if (is_concatenate(line)) {
    add_concatenate(line, env)
  } else if (length(line) == 3) {
    add_binary(line, env)
  } # TODO: add unary operations
}

# Example: Build the graph
# y = a * b + a
# a = x1
# b = x1 + x2
env <- new.env()
env$graph <- Graph$new()
env$counter_list <- list(
  add = 0, sub = 0, mul = 0, div = 0, forward = 0,
  subsetting = 0, concatenate = 0,
  forward_subsetting = 0
)
env$variable_list <- list()

# parse_line(quote(zero <- 0), env)
parse_line(quote(idx1 <- 3), env)
# parse_line(quote(y <- c(zero, zero, zero)), env)
parse_line(quote(x1 <- 4), env)
parse_line(quote(x2 <- 2), env)
parse_line(quote(x3 <- 3), env)
parse_line(quote(a <- c(x1, x2, x3)), env)
parse_line(quote(b <- x1 + x2), env)
parse_line(quote(y <- a * b + a[idx1]), env)
# parse_line(quote(y[idx1] <- a * b + a[idx1]), env)
graph <- env$graph
graph$forward_pass()
graph$backward_pass("y")
graph
