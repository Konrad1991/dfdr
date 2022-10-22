# Literals should be numbers or variables. Not any atomic type will do
# if we expect to differentiate.
is_literal <- function(expr) {
  rlang::is_scalar_double(expr) ||
  rlang::is_scalar_integer(expr) ||
  is.name(expr)
}

# Error handling (rudimentary as it is)
simplify_error <- function(expr) {
  stop(paste0("Unexpected expression ", deparse(expr), " in simplifying"))
}

#' Simplify an expression by computing the values for constant expressions
#'
#' @param expr An expression
#' @return a simplified expression
#' @export
simplify_expr <- function(expr) {
  # Pick function to dispatch to
  dispatch <- expr |> purrr::when(
    is_literal(.)     ~ identity,
    rlang::is_call(.) ~ simplify_call,
                      ~ simplify_error
    )
  dispatch(expr) # and dispatch
}

simplify_addition <- function(lhs, rhs) {
  lhs <- simplify_expr(lhs)
  rhs <- simplify_expr(rhs)
  lhs |> purrr::when( # not actually using first arg, but doing a case
    lhs == 0                               ~ rhs,
    rhs == 0                               ~ lhs,
    is.numeric(lhs) && is.numeric(rhs)     ~ lhs + rhs,
                                           ~ bquote( .(lhs) + .(rhs) )
  )
}

call_name <- function(expr)      { expr[[1]] }
call_arg  <- function(expr, arg) { expr[[1+arg]] }

simplify_unary_subtraction <- function(f) {
  simplify_expr(f) |> purrr::when(
    # If numeric, just change the sign right now
    is.numeric(.)                              ~ (-.),
    # If the expression starts with -, remove it
    (rlang::is_call(.) && call_name(.) == "-") ~ call_arg(., 1),
    # In the general case, put a - in front of the expression
                                               ~ bquote( - .(.) )
  )
}

simplify_subtraction <- function(f, g) {
  left <- simplify_expr(f)
  right <- simplify_expr(g)
  if (left == 0) {
    if (is.numeric(right)) return(-right)
    else return(bquote(-.(right)))
  }
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

simplify_function_call <- function(expr) {
  function_name <- expr[[1]]
  arguments <- vector("list", length(expr) - 1)
  for (i in seq_along(arguments)) {
    arguments[i] <- list(simplify_expr(expr[[i + 1]]))
  }

  # if we have simplified all expressions we might as well try calling the function
  # if it is a function we know...
  if (all(unlist(Map(is.numeric, arguments)))) {
    if (is.name(function_name) &&
        as.character(function_name) %in% c("sin", "cos", "exp", "log")) {
      result <- do.call(as.character(function_name), arguments)
      names(result) <- names(expr)
      return(result)
    }
  }
  result <- as.call(c(list(function_name), arguments))
  names(result) <- names(expr)
  result
}

simplify_call <- function(expr) {
  if (is.name(expr[[1]])) {
    if (expr[[1]] == as.name("+")) return(simplify_addition(expr[[2]], expr[[3]]))
    if (expr[[1]] == as.name("-")) {
      if (length(expr) == 2) return(simplify_unary_subtraction(expr[[2]]))
      else return(simplify_subtraction(expr[[2]], expr[[3]]))
    }

    if (expr[[1]] == as.name("*")) return(simplify_multiplication(expr[[2]], expr[[3]]))
    if (expr[[1]] == as.name("/")) return(simplify_division(expr[[2]], expr[[3]]))

    if (expr[[1]] == as.name("^")) return(simplify_exponentiation(expr[[2]], expr[[3]]))

    if (expr[[1]] == as.name("(")) {
      subexpr <- simplify_expr(expr[[2]])
      if (is.atomic(subexpr) || is.name(subexpr)) return(subexpr)
      else if (is.call(subexpr) && subexpr[[1]] == as.name("(")) return(subexpr)
      else return(call("(", subexpr))
    }
  }

  simplify_function_call(expr)
}
