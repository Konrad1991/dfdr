node <- R6::R6Class(
  "node",
  public = list(
    name = NULL,
    connected_nodes = NULL,
    value = NA,
    deriv = NA,
    initialize = function(name, connected_nodes, value) {
      self$name <- name
      self$connected_nodes <- connected_nodes
      self$value <- value
      self$deriv <- 0
    },
    print = function() {
      cat(
        self$name,
        class(self) |> as.character(),
        "->", paste(self$connected_nodes, collapse = ", "), "\n",
        "Value:", self$value,
        "Derivative:", self$deriv, "\n\n"
      )
    }
  )
)

node_plus <- R6::R6Class(
  "node_plus",
  inherit = node,
  public = list(
    initialize = function(name, connected_nodes, value) {
      super$initialize(
        name = name,
        value = value,
        connected_nodes = connected_nodes
      )
    },
    forward = function(l) {
      inputs <- lapply(self$connected_nodes, function(n) l[[n]]$value)
      self$value <- inputs[[1]] + inputs[[2]]
    },
    backward = function(graph) {
      inputs <- lapply(self$connected_nodes, function(n) graph[[n]])
      grads <- graph[[self$name]]$backward_intern(
        self$deriv, inputs
      )
      for (i in seq_along(self$connected_nodes)) {
        graph[[self$connected_nodes[i]]]$deriv <-
          graph[[self$connected_nodes[i]]]$deriv + grads[[i]]
      }
    },
    backward_intern = function(grad, inputs) {
      list(grad, grad)
    }
  )
)

node_sub <- R6::R6Class(
  "node_sub",
  inherit = node,
  public = list(
    initialize = function(name, connected_nodes, value) {
      super$initialize(
        name = name,
        value = value,
        connected_nodes = connected_nodes
      )
    },
    forward = function(l) {
      inputs <- lapply(self$connected_nodes, function(n) l[[n]]$value)
      self$value <- inputs[[1]] - inputs[[2]]
    },
    backward = function(graph) {
      inputs <- lapply(self$connected_nodes, function(n) graph[[n]])
      grads <- graph[[self$name]]$backward_intern(
        self$deriv, inputs
      )
      for (i in seq_along(self$connected_nodes)) {
        graph[[self$connected_nodes[i]]]$deriv <-
          graph[[self$connected_nodes[i]]]$deriv + grads[[i]]
      }
    },
    backward_intern = function(grad, inputs) {
      list(grad, -grad)
    }
  )
)

node_times <- R6::R6Class(
  "node_times",
  inherit = node,
  public = list(
    initialize = function(name, connected_nodes, value) {
      super$initialize(
        name = name,
        value = value,
        connected_nodes = connected_nodes
      )
    },
    forward = function(l) {
      inputs <- lapply(self$connected_nodes, function(n) l[[n]]$value)
      self$value <- inputs[[1]] * inputs[[2]]
    },
    backward = function(graph) {
      inputs <- lapply(self$connected_nodes, function(n) graph[[n]])
      grads <- graph[[self$name]]$backward_intern(
        self$deriv, inputs
      )
      for (i in seq_along(self$connected_nodes)) {
        graph[[self$connected_nodes[i]]]$deriv <-
          graph[[self$connected_nodes[i]]]$deriv + grads[[i]]
      }
    },
    backward_intern = function(grad, inputs) {
      list(grad * inputs[[2]]$value, grad * inputs[[1]]$value)
    }
  )
)

node_div <- R6::R6Class(
  "node_div",
  inherit = node,
  public = list(
    initialize = function(name, connected_nodes, value) {
      super$initialize(
        name = name,
        value = value,
        connected_nodes = connected_nodes
      )
    },
    forward = function(l) {
      inputs <- lapply(self$connected_nodes, function(n) l[[n]]$value)
      self$value <- inputs[[1]] / inputs[[2]]
    },
    backward = function(graph) {
      inputs <- lapply(self$connected_nodes, function(n) graph[[n]])
      grads <- graph[[self$name]]$backward_intern(
        self$deriv, inputs
      )
      for (i in seq_along(self$connected_nodes)) {
        graph[[self$connected_nodes[i]]]$deriv <-
          graph[[self$connected_nodes[i]]]$deriv + grads[[i]]
      }
    },
    backward_intern = function(grad, inputs) {
      list(
        grad / inputs[[2]]$value,
        -grad * inputs[[1]]$value / (inputs[[2]]$value^2)
      )
    }
  )
)

node_forward <- R6::R6Class(
  "node_forward",
  inherit = node,
  public = list(
    initialize = function(name, connected_nodes, value) {
      super$initialize(
        name = name,
        value = value,
        connected_nodes = connected_nodes
      )
    },
    forward = function(l) {
      inputs <- lapply(self$connected_nodes, function(n) l[[n]]$value)
      if (length(self$connected_nodes) == 0) {
      } else if (length(self$connected_nodes) == 1) {
        self$value <- inputs[[1]]
      } else if (length(self$connected_nodes) > 1) {
        stop("Error: invalid assignment")
      }
    },
    backward = function(graph) {
      inputs <- lapply(self$connected_nodes, function(n) graph[[n]])
      grads <- graph[[self$name]]$backward_intern(
        self$deriv, inputs
      )
      for (i in seq_along(self$connected_nodes)) {
        graph[[self$connected_nodes[i]]]$deriv <-
          graph[[self$connected_nodes[i]]]$deriv + grads
        # NOTE: not grads[[i]].
        # As this only propagates a subset of the derivative
      }
    },
    backward_intern = function(grad, inputs) {
      grad
    }
  )
)

# TODO: Earlier propagation:
# It would be better when the derivative of y is
# the same length as the value attribute
# NOTE: is grad[indices] <- graph[[self$name]]$deriv
# still correct?
# TODO: How to handle literals in c()
# TODO: How to handle calls in c()
node_concatenate <- R6::R6Class(
  "node_concatenate",
  inherit = node,
  public = list(
    initialize = function(name, connected_nodes, value) {
      super$initialize(
        name = name,
        value = value,
        connected_nodes = connected_nodes
      )
    },
    forward = function(l) {
      inputs <- lapply(self$connected_nodes, function(n) l[[n]]$value)
      self$value <- unlist(inputs)
    },
    backward = function(graph) {
      inputs <- lapply(self$connected_nodes, function(n) graph[[n]])
      grads <- lapply(unique(self$connected_nodes), function(x) {
        indices <- self$connected_nodes %in% x
        grad <- rep(0, length(indices))
        grad[indices] <- graph[[self$name]]$deriv
        grad
      })
      names(grads) <- unique(self$connected_nodes)
      sapply(unique(self$connected_nodes), function(x) {
        graph[[x]]$deriv <- graph[[x]]$deriv + grads[[x]]
      })
    }
  )
)
