fct <- setClass(
  "fct",
  slots = list(
    name = "character",
    dfdx = "function",
    name_deriv = "character",
    keep = "logical"
  )
)

setGeneric(
  name = "add_fct",
  def = function(obj, name, dfdx, keep = FALSE) {
    standardGeneric("add_fct")
  } 
)

# used only internally
setGeneric(
  name = "append_fct",
  def = function(obj, name, dfdx_new, name_deriv_new, keep_new) {
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

setGeneric(
  name = "get_keep",
  def = function(obj, name) {
    standardGeneric("get_keep")
  }
)

sin_deriv <- function(x) cos(x)
sinh_deriv <- function(x)  cosh(x) 
asin_deriv <- function(x) 1/(sqrt(1 - x^2)) 
cos_deriv <- function(x) -sin(x) 
cosh_deriv <- function(x) sinh(x) 
acos_deriv <- function(x) -asin(x) 
tan_deriv <- function(x) tan(x)^2 + 1 
tanh_deriv <- function(x) 1 - tanh(x)^2 
atan_deriv <- function(x) 1 / (1 + x^2) 
exp_deriv <- function(x) exp(x) 
log_deriv <- function(x) 1/x 
sqrt_deriv <- function(x) 0.5 * (x)^(-0.5) 
c_deriv <- function(...) c(...)
vector_deriv <- function(x) vector(length = x)
numeric_deriv <- function(x) numeric(x)
rep_deriv <- function(x, y) rep(x, y)
matrix_deriv <- function(val = 0, x, y) matrix(val, x, y)

#' @title S4 class \emph{fcts}
#'
#' @importFrom R6 R6Class
#' @importFrom stats setNames
#' @importFrom methods formalArgs
#' @importFrom methods new
#' @importFrom pryr modify_lang
#' @description 
#' A S4 class containing additional functions which can be used for calculating derivatives with \code{\link{d}()}. \cr
#' To create a class the function \emph{fcts()} should be used. \cr
#' Adding functions is only possible \emph{via} the function \emph{add_fct}.
#' 
#' @slot funs A list containing the specified functions. This slot should not be accessed and is used only internally.
#'
#' @details
#' The following functions are already supported: \cr
#' sin, sinh, asin, cos, cosh, acos, tan, tanh, atan, exp, log, sqrt, c, vector, numeric, rep and matrix. \cr
#' Notably, for the functions: c, vector, numeric, rep and matrix the function is ignored during differentiation.
#'
#' @seealso 
#' \code{\link{d}()}
#' @name fcts
#' @examples
#' library(dfdr)
#' # Initialize list
#' lst <- dfdr::fcts()
#' 
#' # The function which should be added
#' f <- function(x) x^2
#' # The dervative function of f
#' f_deriv <- function(x) 2*x
#' 
#' # add new entry to list
#' lst <- fcts_add_fct(lst, f, f_deriv)
#' 
#' g <- function(z) f(z)
#' df <- d(g, z, lst)
#' df
#' @export fcts
fcts <- setClass(
  "fcts",
   slots = c(funs = "list")
)

#' appending a S4 class of type \emph{fcts} 
#'
#' @description 
#' A function which appends a S4 class of type \emph{fcts} with a new function-derivative pair.
#'
#' @param lst is the S4 class of type \emph{fcts}. Newly created by \code{\link{fcts}()} 
#' @param f is the function which should be differentiated. The argument has to be of type function.
#' @param f_deriv is a function defining the derivative of \emph{f}. The argument has to be of type function.
#' @param keep is a logical value. If set to TRUE the function \emph{f} is ignored of \code{\link{d}()}. The default value is FALSE.
#' 
#' @details
#' The following functions are already supported: \cr
#' sin, sinh, asin, cos, cosh, acos, tan, tanh, atan, exp, log, sqrt, c, vector, numeric, rep and matrix. \cr
#' Notably, for the functions: c, vector, numeric, rep and matrix the function is ignored during differentiation.
#' 
#' @return a S4 class of type \emph{fcts} extended by the new function-derivative pair.
#' 
#' @note The body of \emph{f} and \emph{f_deriv} have to be defined without curly brackets.
#' @examples
#' library(dfdr)
#' # Initialize list
#' lst <- dfdr::fcts()
#' 
#' # The function which should be added
#' f <- function(x) x^2
#' # The dervative function of f
#' f_deriv <- function(x) 2*x
#' 
#' # add new entry to list
#' lst <- fcts_add_fct(lst, f, f_deriv)
#' 
#' g <- function(z) f(z)
#' df <- d(g, z, lst)
#' df
#' @export
fcts_add_fct <- function(lst, f, f_deriv, keep = FALSE) {
  name <- as.character(deparse(substitute(f)))
  lst@funs[[name]] = fct(name = name,
                         dfdx = f_deriv, keep = keep,
                         name_deriv = as.character(deparse(substitute(f_deriv))) )
  lst
}


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
  definition = function(obj, name, dfdx, keep = FALSE) {
    obj@funs[[name]] = fct(name = name, dfdx = dfdx, keep = keep,
                           name_deriv = as.character(deparse(substitute(dfdx))) )
    obj
  }
)

setMethod(
  f = "append_fct",
  signature = "fcts",
  definition = function(obj, name, dfdx_new, name_deriv_new, keep_new) {
    obj@funs[[name]] = fct(name = name, dfdx = dfdx_new, name_deriv =  name_deriv_new, keep = keep_new)
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

setMethod(
  f = "get_keep",
  signature = "fcts",
  definition = function(obj, name) {
    obj@funs[[name]]@keep
  }
)


sin_deriv <- function(x) cos(x)
sinh_deriv <- function(x)  cosh(x) 
asin_deriv <- function(x) 1/(sqrt(1 - x^2)) 
cos_deriv <- function(x) -sin(x) 
cosh_deriv <- function(x) sinh(x) 
acos_deriv <- function(x) -asin(x) 
tan_deriv <- function(x) tan(x)^2 + 1 
tanh_deriv <- function(x) 1 - tanh(x)^2 
atan_deriv <- function(x) 1 / (1 + x^2) 
exp_deriv <- function(x) exp(x) 
log_deriv <- function(x) 1/x 
sqrt_deriv <- function(x) 0.5 * (x)^(-0.5) 
c_deriv <- function(...) c(...)
vector_deriv <- function(x) vector(length = x)
numeric_deriv <- function(x) numeric(x)
rep_deriv <- function(x, y) rep(x, y)
matrix_deriv <- function(val = 0, x, y) matrix(val, x, y)



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
  f <- add_fct(f, "c", c_deriv, TRUE)
  f <- add_fct(f, "vector", vector_deriv, TRUE)
  f <- add_fct(f, "numeric", numeric_deriv, TRUE)
  f <- add_fct(f, "rep", rep_deriv, TRUE)
  f <- add_fct(f, "matrix", matrix_deriv, TRUE)
  return(f)
}