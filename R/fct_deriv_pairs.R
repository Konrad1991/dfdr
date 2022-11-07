fct <- setClass(
  "fct",
  slots = list(
    name = "character",
    dfdx = "function",
    name_deriv = "character"
  )
)

setGeneric(
  name = "add_fct",
  def = function(obj, name, dfdx_new) {
    standardGeneric("add_fct")
  } 
)

# used only internally
setGeneric(
  name = "append_fct",
  def = function(obj, name, dfdx_new, name_deriv_new) {
    standardGeneric("append_fct")
  } 
)

setGeneric(
  name = "get_derivative",
  def = function(obj, name) {
    standardGeneric("get_derivative")
  } 
)

setGeneric(
  name = "get_derivative_name",
  def = function(obj, name) {
    standardGeneric("get_derivative_name")
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
    obj@funs[[name]] = fct(name = name, dfdx = dfdx_new,
                           name_deriv = as.character(deparse(substitute(dfdx_new))) )
    obj
  }
)

setMethod(
  f = "append_fct",
  signature = "fcts",
  definition = function(obj, name, dfdx_new, name_deriv_new) {
    obj@funs[[name]] = fct(name = name, dfdx = dfdx_new, name_deriv =  name_deriv_new)
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

setMethod(
  f = "get_derivative_name",
  signature = "fcts",
  definition = function(obj, name) {
    obj@funs[[name]]@name_deriv
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

#' Derivative function for sin
#'
#' @export
sin_deriv <- function(x) { cos(x) }

#' Derivative function for sinh
#'
#' @export
sinh_deriv <- function(x) { cosh(x) }

#' Derivative function for asin
#'
#' @export
asin_deriv <- function(x) { 1/(sqrt(1 - x^2) ) }

#' Derivative function for cos
#'
#' @export
cos_deriv <- function(x) { -sin(x) }

#' Derivative function for cosh
#'
#' @export
cosh_deriv <- function(x) { sinh(x) }

#' Derivative function for acos
#'
#' @export
acos_deriv <- function(x) { -asin(x) }

#' Derivative function for tan
#'
#' @export
tan_deriv <- function(x) { tan(x)^2 + 1 }

#' Derivative function for tanh
#'
#' @export
tanh_deriv <- function(x) { 1 - tanh(x)^2 }

#' Derivative function for atan
#'
#' @export
atan_deriv <- function(x) { 1 / (1 + x^2) }

#' Derivative function for exp
#'
#' @export
exp_deriv <- function(x) { exp(x) }

#' Derivative function for log
#'
#' @export
log_deriv <- function(x) { 1/x }

#' Derivative function for sqrt
#'
#' @export
sqrt_deriv <- function(x) { 0.5 * (x)^(-0.5) }

init_fct_list <- function() {
  f <- fcts()
  f <- add_fct(f, "sin",  sin_deriv)
  f <- add_fct(f, "sinh", sinh_deriv)
  f <- add_fct(f, "asin", asin_deriv)
  f <- add_fct(f, "cos",  cos_deriv)
  f <- add_fct(f, "cosh", cosh_deriv)
  f <- add_fct(f, "acos", acos_deriv)
  f <- add_fct(f, "tan",  tan_deriv)
  f <- add_fct(f, "tanh", tanh_deriv)
  f <- add_fct(f, "atan", atan_deriv)
  f <- add_fct(f, "exp",  exp_deriv)
  f <- add_fct(f, "log",  log_deriv)
  f <- add_fct(f, "sqrt", sqrt_deriv)
  return(f)
}