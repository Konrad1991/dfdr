# set deriv to 1 for last assign node
set_1_last_assign <- function(from_what, node_list) {
  idx <- which(grepl(from_what, ls(node_list)))
  idx <- idx[length(idx)]
  if (length(idx) == 0) {
    stop("node not found")
  } else if (length(idx) == 1) {
    node_list[[ls(node_list)[idx]]]$deriv <- rep(
      1, length(node_list[[ls(node_list)[idx]]]$value)
    )
  } else {
    stop("test")
  }
}

create_node <- function(operation, node_name, connected_nodes, value) {
  node <- NULL
  if (operation == "add") {
    node <- node_plus$new(
      name = node_name,
      connected_nodes = connected_nodes,
      value = value
    )
  } else if (operation == "sub") {
    node <- node_sub$new(
      name = node_name,
      connected_nodes = connected_nodes,
      value = value
    )
  } else if (operation == "mul") {
    node <- node_times$new(
      name = node_name,
      connected_nodes = connected_nodes,
      value = value
    )
  } else if (operation == "div") {
    node <- node_div$new(
      name = node_name,
      connected_nodes = connected_nodes,
      value = value
    )
  } else if (operation == "forward") {
    node <- node_forward$new(
      name = node_name,
      connected_nodes = connected_nodes,
      value = value
    )
  } else if (operation == "concatenate") {
    node <- node_concatenate$new(
      name = node_name,
      connected_nodes = connected_nodes,
      value = value
    )
  }
  return(node)
}

sort <- R6::R6Class(
  "sort",
  public = list(
    visited = NULL,
    stack = NULL,
    node_list = NULL,
    # Topological sort (to process nodes in the correct order)
    topological_sort = function() {
      self$visited <- character(0)
      self$stack <- character(0)
      visit <- function(node_name) {
        if (node_name %in% self$visited) {
          return()
        }
        self$visited <- c(self$visited, node_name)
        if (!is.null(self$node_list[[node_name]])) {
          for (neighbor in self$node_list[[node_name]]$connected_nodes) {
            visit(neighbor)
          }
        }
        self$stack <- c(self$stack, node_name)
      }
      for (node_name in names(self$node_list)) {
        visit(node_name)
      }
      self$stack <- rev(self$stack)
    },
    ts = function(node_list) {
      self$node_list <- node_list
      return(self$topological_sort())
    }
  )
)

# TODO: Create output class
# define a class which holds the nodes
# already sorted.
# The idea is that the graphClass is responsible
# to create an efficient output
graph <- R6::R6Class(
  "graph",
  public = list(
    node_list = new.env(),
    last_assigned = NULL,
    assignments = list(),
    sorted_nodes = NULL,
    sort = sort$new(),
    initialize = function() {
      self$node_list <- new.env()
    },
    add_node = function(node_name, connected_nodes = character(0),
                        operation = NULL, value = NA) {
      node <- create_node(operation, node_name, connected_nodes, value)
      self$last_assigned <- node_name
      self$assignments[[length(self$assignments) + 1]] <- node_name
      self$node_list[[node_name]] <- node
    },
    forward_pass = function() { # INFO: calculate values
      self$sorted_nodes <- self$sort$ts(self$node_list)
      for (node_name in rev(self$sorted_nodes)) {
        # PLAN: here check if node is "if" branch
        self$node_list[[node_name]]$forward(self$node_list)
      }
    },
    backward_pass = function(from_what) { # INFO: calculate derivatives
      # Initialize derivative at the output node
      set_1_last_assign(from_what, self$node_list)
      for (node_name in self$sorted_nodes) {
        if (!is.null(self$node_list[[node_name]]$backward)) {
          self$node_list[[node_name]]$backward(self$node_list)
        }
      }
    },
    get_value = function(from_what) {
      idx <- which(grepl(from_what, ls(self$node_list)))
      idx <- idx[length(idx)]
      self$node_list[[ls(self$node_list)[idx]]]$value
    },
    get_deriv = function(from_what) {
      idx <- which(grepl(from_what, ls(self$node_list)))
      idx <- idx[1]
      self$node_list[[ls(self$node_list)[idx]]]$deriv
    },
    print = function() {
      for (node in self$assignments) {
        print(node)
        self$node_list[[node]]$print()
      }
    }
  )
)
