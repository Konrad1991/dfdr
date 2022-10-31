fct <- setClass(
  "fct",
  slots = list(
    name = "character",
    dfdx = "function"
  )
)

setGeneric(
  name = "add_fct",
  def = function(obj, name, dfdx_new) {
    standardGeneric("add_fct")
  } 
)

setGeneric(
  name = "get_derivative",
  def = function(obj, name) {
    standardGeneric("get_derivative")
  } 
)

fcts <- setClass(
  "fcts",
   slots = c(funs = "list")
)

setMethod(
  "initialize",
  signature = "fcts",
  def = function(.Object) {
    .Object@funs <- list()
    .Object
  }
)

setMethod(
  f = "add_fct",
  signature = "fcts",
  definition = function(obj, name, dfdx_new) {
    obj@funs[[name]] = fct(name = name, dfdx=dfdx_new)
    obj
  }
)

setMethod(
  f = "get_derivative",
  signature = "fcts",
  definition = function(obj, name) {
    obj@funs[[name]]@dfdx
  } 
)

setGeneric(
  name = "get_names",
  def = function(obj) {
    standardGeneric("get_names")
  } 
)

setMethod(
  f = "get_names",
  signature = "fcts",
  definition = function(obj) {
    l <- obj@funs
    sapply(l, function(x) x@name)
  }
)

init_fct_list <- function() {
  f <- fcts()
  f <- add_fct(f, "sin",  function(x) bquote(cos(.(x))) )
  f <- add_fct(f, "sinh", function(x) bquote(cosh(.(x))) )
  f <- add_fct(f, "asin", function(x) bquote( 1/(sqrt(1 - .(x)^2) )) ) 
  f <- add_fct(f, "cos",  function(x)bquote(-sin(.(x))) ) 
  f <- add_fct(f, "cosh", function(x) bquote(sinh(.(x))) )
  f <- add_fct(f, "acos", function(x) bquote(-asin(.(x))) )
  f <- add_fct(f, "tan",  function(x) bquote(tan(.(x))^2 + 1) )
  f <- add_fct(f, "tanh", function(x) bquote(1 - tanh(.(x))^2 ) )
  f <- add_fct(f, "atan", function(x) bquote(1 / (1 + .(x)^2) ) )
  f <- add_fct(f, "exp",  function(x) bquote(exp(.(x))) )
  f <- add_fct(f, "log",  function(x) bquote(1/.(x)))
  f <- add_fct(f, "sqrt", function(x) bquote(0.5 * .(x)^(-0.5)))
  return(f)
}