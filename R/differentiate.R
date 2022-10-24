
#' Differentiate a function for a single variable.
#'
#' @param f  The function to differentiate.
#' @param x  The variable that f should be differentiated with respect to.
#' @return \deqn{\frac{\mathrm{d}f}{\mathrm{d}x}} if called with function f and symbol x.
#' @export
d <- function(f, x) {

  # Primitive functions, we have to treat carefully. They don't have a body.
  # This is just a short list of such built-in arithmetic functions, it is
  # not exhaustive.
  if (is.null(body(f))) {
    if (identical(f, sin)) return(cos)
    if (identical(f, cos)) return(function(x) -sin(x))
    if (identical(f, exp)) return(exp)

    stop("unknown primitive") # nocov

  } else {
    # for other functions we have to parse the body
    # and differentiate it.
    df <- f
    e <- environment(f)
    body(df) <- simplify_expr(diff_expr(body(f), x, e))
    df
  }
}

diff_expr <- function(expr, x, e) {
  expr |> purrr::when(
    is.numeric(.)              ~ quote(0),
    is.name(.) && . == x       ~ quote(1),
    is.name(.)                 ~ quote(0),
    is.call(.)                 ~ diff_call(expr, x, e),
    ~ stop(paste0("Unexpected expression ", deparse(expr), " in parsing.")) # nocov
  )
}

diff_addition <- function(f, g, x, e) {
  lhs <- diff_expr(f, x, e)
  rhs <- diff_expr(g, x, e)
  bquote( .(lhs) + .(rhs) )
}

diff_subtraction <- function(f, g, x, e) {
  lhs <- diff_expr(f, x, e)
  rhs <- diff_expr(g, x, e)
  bquote( .(lhs) - .(rhs) )
}

diff_multiplication <- function(f, g, x, e) {
  # f' g + f g'
  df <- diff_expr(f, x, e)
  dg <- diff_expr(g, x, e)
  bquote( .(df)*.(g) + .(f)*.(dg) )
}

diff_division <- function(f, g, x, e) {
  # (f' g − f g' )/g**2
  df <- diff_expr(f, x, e)
  dg <- diff_expr(g, x, e)
  bquote( ( .(df)*.(g) - .(f)*.(dg) ) / .(g)**2 )
}

diff_exponentiation <- function(f, g, x, e) {
  # Using the chain rule to handle this generally.
  # if y = f**g then dy/dx = dy/df df/dx = g * f**(g-1) * df/dx
  df <- diff_expr(f, x, e)
  bquote( .(g) * .(f)**(.(g)-1) * .(df) )
}

# FIXME: This is a mess with only a few functions I could think of handled...
.built_in_functions <- c("sin", "cos", "exp")
diff_built_in_function_call <- function(expr, x, e) {
  # chain rule with a known function to differentiate. df/dx = df/dy dy/dx
  y <- call_arg(expr, 1)
  dy_dx <- diff_expr(call_arg(expr, 1), x, e)
  call_name(expr) |> purrr::when(
    . == "sin" ~ bquote(  cos(.(y)) * .(dy_dx)),
    . == "cos" ~ bquote( -sin(.(y)) * .(dy_dx)),
    . == "exp" ~ bquote(  exp(.(y)) * .(dy_dx))
  )
}


diff_general_function_call <- function(expr, x, e) {
  function_name <- call_name(expr)
  if (!is.name(function_name))
    stop(paste0("Unexpected call ", deparse(expr)))

  func <- get(function_name, e)
  full_call <- match.call(func, expr)
  variables <- names(full_call)

  arguments <- vector("list", length(full_call) - 1)
  for (i in seq_along(arguments)) {
    var <- variables[i + 1]
    dfdz <- full_call
    dfdz[[1]] <- bquote(d(.(function_name), .(var)))
    dzdx <- diff_expr(expr[[i + 1]], x, e)
    arguments[[i]] <- bquote(.(dfdz) * .(dzdx))
  }
  as.call(c(list(sum), arguments))
}

diff_call <- function(expr, x, e) {
  # FIXME: extend this to be the results...
  # print(call_name(expr))
  # call_name(expr) |> purrr::when(
  #   . == "+" ~ iff_addition(call_arg(expr, 1), call_arg(expr, 2), x, e),
  #   #(. == "-" && length(expr) == 2) ~ bquote(- .(diff_expr(call_arg(expr, 1), x, e))),
  #   #. == "-" ~ diff_addition(call_arg(expr, 1), call_arg(expr, 2), x, e),
  #   #. == "*" ~ diff_multiplication(call_arg(expr, 1), call_arg(expr, 2), x, e),
  #   #. == "/" ~ diff_division(call_arg(expr, 1), call_arg(expr, 2), x, e),
  #   #. == "^" ~ diff_exponentiation(call_arg(expr, 1), call_arg(expr, 2), x, e),
  #   # . == "(" ~  {
  #   #   subexpr <- diff_expr(expr[[2]], x, e)
  #   #   if (is.atomic(subexpr) || is.name(subexpr)) subexpr
  #   #   else if (is.call(subexpr) && subexpr[[1]] == as.name("("))
  #   #     subexpr
  #   #   else
  #   #     call("(", subexpr)
  #   # }
  #   ~ expr # FIXME: fake final case
  # )
  # FIXME end

  if (is.name(expr[[1]])) {
    if (expr[[1]] == as.name("+"))
      return(diff_addition(expr[[2]], expr[[3]], x, e))

    if (expr[[1]] == as.name("-")) {
      if (length(expr) == 2)
        return(call("-", diff_expr(expr[[2]], x, e)))
      else
        return(diff_subtraction(expr[[2]], expr[[3]], x, e))
    }

    if (expr[[1]] == as.name("*"))
      return(diff_multiplication(expr[[2]], expr[[3]], x, e))
    if (expr[[1]] == as.name("/"))
      return(diff_division(expr[[2]], expr[[3]], x, e))

    if (expr[[1]] == as.name("^"))
      return(diff_exponentiation(expr[[2]], expr[[3]], x, e))

    if (expr[[1]] == as.name("(")) {
      subexpr <- diff_expr(expr[[2]], x, e)
      if (is.atomic(subexpr) || is.name(subexpr))
        return(subexpr)
      else if (is.call(subexpr) && subexpr[[1]] == as.name("("))
        return(subexpr)
      else
        return(call("(", subexpr))
    }
  }

  if (is.name(expr[[1]]) && as.character(expr[[1]]) %in% .built_in_functions)
    return(diff_built_in_function_call(expr, x, e))
  else
    return(diff_general_function_call(expr, x, e))
}
