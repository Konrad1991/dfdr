# Literals should be numbers or variables. Not any atomic type will do
# if we expect to differentiate.
is_literal <- function(expr) {
  rlang::is_scalar_double(expr) ||
  rlang::is_scalar_integer(expr) ||
  is.name(expr)
}

# For simplifying arguments before we simplify a call
simplify_args <- function(expr) purrr::map(expr[2:length(expr)], simplify_expr)

# Get the operands from a call
lhs <- function(args) args[[1]]
rhs <- function(args) args[[2]]
call_name <- function(expr)      { expr[[1]] }
call_arg  <- function(expr, arg) { expr[[1+arg]] }


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
  expr |> purrr::when(
    is_literal(.)     ~ identity(.),
    rlang::is_call(.) ~ simplify_call(.),
                      ~ simplify_error(.)
    )
}


simplify_addition <- function(expr) {
  expr |> simplify_args() |> purrr::when(
    lhs(.) == 0                               ~ rhs(.),
    rhs(.) == 0                               ~ lhs(.),
    is.numeric(lhs(.)) && is.numeric(rhs(.))  ~ lhs(.) + rhs(.),
                                              ~ bquote( .(lhs(.)) + .(rhs(.)) )
  )
}

simplify_unary_subtraction <- function(expr) {
  expr |> simplify_args() |> lhs() |> purrr::when(
    is.numeric(.)                                 ~ (-.),
    (rlang::is_call(.) && call_name(.) == "-")    ~ call_arg(., 1), # - - = +
                                                  ~ bquote( - .(.) )
  )
}

simplify_subtraction <- function(expr) {
  expr |> simplify_args() |> purrr::when(
    is.numeric(lhs(.)) && is.numeric(rhs(.))   ~ lhs(.) - rhs(.),
    lhs(.) == 0                                ~ bquote(-.(rhs(.))),
    rhs(.) == 0                                ~ lhs(.),
                                               ~ bquote( .(lhs(.)) - .(rhs(.)) )
  )
}

simplify_multiplication <- function(expr) {
  expr |> simplify_args() |> purrr::when(
    is.numeric(lhs(.)) && is.numeric(rhs(.))    ~ lhs(.) * rhs(.),
    lhs(.) == 0 || rhs(.) == 0                  ~ 0,
    lhs(.) == 1                                 ~ rhs(.),
    rhs(.) == 1                                 ~ lhs(.),
                                                ~ bquote( .(lhs(.)) * .(rhs(.)) )
  )
}

simplify_division <- function(expr) {
  expr |> simplify_args() |> purrr::when(
    is.numeric(lhs(.)) && is.numeric(rhs(.))    ~ lhs(.) / rhs(.),
    rhs(.) == 1                                 ~ lhs(.),
                                                ~ bquote( .(lhs(.)) / .(rhs(.)) )
  )
}

simplify_exponentiation <- function(expr) {
  expr |> simplify_args() |> purrr::when(
    is.numeric(lhs(.)) && is.numeric(rhs(.))    ~ lhs(.) ^ rhs(.),
    rhs(.) == 0                                 ~ 1,
    lhs(.) == 0                                 ~ 0,
    lhs(.) == 1                                 ~ 1,
    rhs(.) == 1                                 ~ lhs(.),
                                                ~ bquote( .(lhs(.)) ^ .(rhs(.)) )
  )
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

simplify_parens <- function(expr) {
  expr |> call_arg(1) |> simplify_expr() |> purrr::when(
    is.atomic(.) || is.name(.) || (rlang::is_call(.) && call_name(.) == "(") ~ .,
    ~ bquote( ( .(.) ) )
  )
}

simplify_call <- function(expr) {
  simplifier <- call_name(expr) |> purrr::when(
    is.name(.) ~ . |> purrr::when(
      . == "+"                         ~ simplify_addition,
      . == "-" && length(expr) == 2    ~ simplify_unary_subtraction,
      . == "-"                         ~ simplify_subtraction,
      . == "*"                         ~ simplify_multiplication,
      . == "/"                         ~ simplify_division,
      . == "^"                         ~ simplify_exponentiation,
      . == "**"                        ~ simplify_exponentiation,
      . == "("                         ~ simplify_parens,
                                       ~ simplify_function_call
    ),
    ~ simplify_function_call
  )
  simplifier(expr)
}
