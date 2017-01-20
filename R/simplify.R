#' Simplify an expression by computing the values for constant expressions
#'
#' @param expr An expression
#' @return a simplified expression
#' @export
simplify_expr <- function(expr) {
  if (is.numeric(expr)) {
    expr

  } else if (is.name(expr)) {
    expr # FIXME: we can do some partial evaluation here...

  } else if (is.call(expr)) {
    simplify_call(expr)

  } else {
    stop(paste0("Unexpected expression ", deparse(expr), " in simplifying")) # nocov
  }
}

simplify_addition <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0) return(right)
  if (right == 0) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left + right)
  call("+", left, right)
}

simplify_subtraction <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0) return(right)
  if (right == 0) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left - right)
  call("-", left, right)
}

simplify_multiplication <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0 || right == 0) return(0)
  if (left == 1) return(right)
  if (right == 1) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left * right)
  call("*", left, right)
}

simplify_division <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (right == 1) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left / right)
  call("/", left, right)
}

simplify_exponentiation <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (right == 0) return(1)
  if (left == 0) return(0)
  if (left == 1) return(1)
  if (right == 1) return(left)
  if (is.numeric(left) && is.numeric(right)) return(left ^ right)
  call("^", left, right)
}

.simplify_built_in_functions <- c("sin", "cos", "exp")
simplify_built_in_function <- function(expr, x) {
  call_name <- as.character(expr[[1]])
  arguments <- Map(simplify_expr, expr[[-1]])
  if (all(is.numeric(arguments))) do.call(call_name, arguments)
  else do.call("call", call_name, arguments)
}

simplify_call <- function(expr) {
  if (expr[[1]] == as.name("+")) return(simplify_addition(expr[[2]], expr[[3]]))
  if (expr[[1]] == as.name("-")) {
    if (length(expr) == 2) return(simplify_subtraction(expr[[2]], 0))
    else return(simplify_subtraction(expr[[2]], expr[[3]]))
  }

  if (expr[[1]] == as.name("*")) return(simplify_multiplication(expr[[2]], expr[[3]]))
  if (expr[[1]] == as.name("/")) return(simplify_division(expr[[2]], expr[[3]]))

  if (expr[[1]] == as.name("^")) return(simplify_exponentiation(expr[[2]], expr[[3]]))

  if (as.character(expr[[1]]) %in% .simplify_built_in_functions) return(simplify_built_in_function(expr))

  stop(paste0("Unexpected call ", deparse(expr)))
}
