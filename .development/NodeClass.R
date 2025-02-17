Node <- R6::R6Class(
  "Node",
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

NodeLiteral <- R6::R6Class(
  "NodeLiteral",
  inherit = Node,
  public = list(
    initialize = function(name, connected_nodes, value) {
      self$name <- name
      self$connected_nodes <- NULL
      self$value <- value
      self$deriv <- 0
    }
  )
)

NodePlusArithmetic <- R6::R6Class(
  "NodePlusArithmetic",
  inherit = Node,
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

NodeSubArithmetic <- R6::R6Class(
  "NodeSubArithmetic",
  inherit = Node,
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

NodeTimesArithmetic <- R6::R6Class(
  "NodeTimesArithmetic",
  inherit = Node,
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

NodeDivArithmetic <- R6::R6Class(
  "NodeDivArithmetic",
  inherit = Node,
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

NodeForward <- R6::R6Class(
  "NodeForward",
  inherit = Node,
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
          graph[[self$connected_nodes[i]]]$deriv + grads[[i]]
      }
    },
    backward_intern = function(grad, inputs) {
      grad
    }
  )
)

NodeConcatenate <- R6::R6Class(
  "NodeConcatenate",
  inherit = Node,
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
      grads <- graph[[self$name]]$backward_intern(
        self$deriv, inputs
      )
      # TODO:Loop over a, b, a, b
      # But then the grad is added on a and b twice
      # Therefore, here the entries in a and b have to be elongated
      # Assumption that grad must have a length which is
      # a multiple of the length of a and b

      for (i in seq_along(self$connected_nodes)) {
        graph[[self$connected_nodes[i]]]$deriv <-
          graph[[self$connected_nodes[i]]]$deriv + grads[[i]]
      }
    },
    backward_intern = function(grad, inputs) {
      if (identical(grad, numeric(0))) {
        return(rep(0, length(inputs)))
      }
      rep(grad, length(inputs)) |> as.list()
    }
  )
)
