# set deriv to 1 for last assign node
set_1_last_assign <- function(from_what, node_list) {
  idx <- which(grepl(from_what, ls(node_list)))
  idx <- idx[length(idx)]
  if (length(idx) == 0) {
    stop("Node not found")
  } else if (length(idx) == 1) {
    node_list[[ls(node_list)[idx]]]$deriv <- 1
  } else {
    stop("test")
  }
}

Graph <- R6::R6Class(
  "Graph",
  public = list(
    l = new.env(),
    visited = NULL,
    stack = NULL,
    last_assigned = NULL,
    assignments = list(),
    initialize = function() {
      self$l <- new.env()
    },
    add_node = function(node_name, connected_nodes = character(0),
                        operation = NULL, value = NA) {
      node <- NULL
      self$last_assigned <- node_name
      self$assignments[[length(self$assignments) + 1]] <- node_name
      if (operation == "add") {
        node <- NodePlusArithmetic$new(
          name = node_name,
          connected_nodes = connected_nodes,
          value = value
        )
      } else if (operation == "sub") {
        node <- NodeSubArithmetic$new(
          name = node_name,
          connected_nodes = connected_nodes,
          value = value
        )
      } else if (operation == "mul") {
        node <- NodeTimesArithmetic$new(
          name = node_name,
          connected_nodes = connected_nodes,
          value = value
        )
      } else if (operation == "div") {
        node <- NodeDivArithmetic$new(
          name = node_name,
          connected_nodes = connected_nodes,
          value = value
        )
      } else if (operation == "forward") {
        node <- NodeForward$new(
          name = node_name,
          connected_nodes = connected_nodes,
          value = value
        )
      } else if (operation == "concatenate") {
        node <- NodeConcatenate$new(
          name = node_name,
          connected_nodes = connected_nodes,
          value = value
        )
      }
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
        # PLAN: here check if node is "if" branch
        self$l[[node_name]]$forward(self$l)
      }
    },
    backward_pass = function(from_what) { # INFO: calculate derivatives
      sorted_nodes <- self$topological_sort()
      # Initialize derivative at the output node
      set_1_last_assign(from_what, self$l)
      for (node_name in sorted_nodes) {
        if (!is.null(self$l[[node_name]]$backward)) {
          self$l[[node_name]]$backward(self$l)
        }
      }
    },
    # TODO: Create output class
    # define a class which holds the nodes
    # already sorted.
    # The idea is that the GraphClass is responsible
    # to create an efficient output
    get_value = function(from_what) {
      idx <- which(grepl(from_what, ls(self$l)))
      idx <- idx[length(idx)]
      self$l[[ls(self$l)[idx]]]$value
    },
    get_deriv = function(from_what) {
      idx <- which(grepl(from_what, ls(self$l)))
      idx <- idx[1]
      self$l[[ls(self$l)[idx]]]$deriv
    },
    print = function() {
      for (node in self$assignments) {
        print(node)
        self$l[[node]]$print()
      }
    }
  )
)
