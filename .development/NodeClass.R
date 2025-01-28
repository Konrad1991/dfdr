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
            rhs <- operations[[operation]]$forward(gr[[name]]$value, inputs)
            return(rhs)
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
