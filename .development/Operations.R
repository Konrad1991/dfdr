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
