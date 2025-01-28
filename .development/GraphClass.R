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
