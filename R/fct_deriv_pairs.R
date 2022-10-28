fct <- setClass(
  "fct",
  slots = list(
    name = "character",
    f = "function",
    dfdx = "function"
  )
)

setGeneric(
  name = "add_fct",
  def = function(obj, name, f_new, dfdx_new) {
    standardGeneric("add_fct")
  } 
)

setGeneric(
  name = "get_fct",
  def = function(obj, name) {
    standardGeneric("get_fct")
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
  definition = function(obj, name, f_new, dfdx_new) {
    obj@funs[[name]] = fct(name = name, f=f_new, dfdx=dfdx_new)
    obj
  }
)

setMethod(
  f = "get_fct",
  signature = "fcts",
  definition = function(obj, name) {
    obj@funs[[name]]
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
  f <- add_fct(f, "sin",  sin,  cos)
  f <- add_fct(f, "sinh", sinh, cosh)
  f <- add_fct(f, "asin", asin, function(x) 1/(sqrt(1 - x^2)))
  f <- add_fct(f, "cos",  cos,  function(x) -sin(x)) # \ is cool. But only supported starting from R 4.2. 
  f <- add_fct(f, "cosh", cosh, sinh)
  f <- add_fct(f, "acos", acos, function(x) -asin(x))
  f <- add_fct(f, "tan",  tan,  function(x) tan(x)^2 + 1)
  f <- add_fct(f, "tanh", tanh, function(x) 1 - tanh(x)^2)  
  f <- add_fct(f, "atan", atan, function(x) 1 / (1 + x^2) )
  f <- add_fct(f, "exp",  exp,  exp)
  f <- add_fct(f, "log",  log,  function(x) 1/x)
  f <- add_fct(f, "sqrt", sqrt, function(x) 0.5 * x^(-0.5))
  return(f)
}