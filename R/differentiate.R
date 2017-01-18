
#' Differentiate a function for a single variable.
#'
#' @param f  The function to differentiate.
#' @param x  The variable that f should be differentiated with respect to.
#' @return \deqn{\frac{\mathrm{d}f}{\mathrm{d}x}}
#' @export
d <- function(f, x) {
  df <- f
  body(df) <- diff_expr(body(f), as.name(x))
  df
}

diff_expr <- function(expr, x) {
  if (is.numeric(expr)) {
    quote(0)

  } else if (is.name(expr)) {
    if (expr == x) quote(1)
    else quote(0)

  } else if (is.call(expr)) {
    diff_call(expr, x)

  } else {
    stop(paste0("Unexpected expression ", deparse(expr), " in parsing.")) # nocov
  }
}


diff_addition <- function(f, g, x) {
  call("+", diff_expr(f, x), diff_expr(g, x))
}

diff_subtraction <- function(f, g, x) {
  call("-", diff_expr(f, x), diff_expr(g, x))
}

diff_multiplication <- function(f, g, x) {
  # f' g + f g'
  call("+",
       call("*", diff_expr(f, x), g),
       call("*", f, diff_expr(g, x)))
}

diff_division <- function(f, g, x) {
  # (f' g − f g' )/g**2
  call("/",
       call("-",
        call("*", diff_expr(f, x), g),
        call("*", f, diff_expr(g, x))),
       call("^", g, 2))
}

diff_exponentiation <- function(f, g, x) {
  # Using the chain rule to handle this generally.
  # if y = f**g then dy/dx = dy/df df/dx = g * f**(g-1) * df/dx
  dydf <- call("*", g, call("^", f, substitute(n - 1, list(n = g))))
  dfdx <- diff_expr(f, x)
  call("*", dydf, dfdx)
}

diff_call <- function(expr, x) {
  if (expr[[1]] == as.name("+")) return(diff_addition(expr[[2]], expr[[3]], x))
  if (expr[[1]] == as.name("-")) {
    if (length(expr) == 2) return(call("-", diff_expr(expr[[2]], x)))
    else return(diff_subtraction(expr[[2]], expr[[3]], x))
  }

  if (expr[[1]] == as.name("*")) return(diff_multiplication(expr[[2]], expr[[3]], x))
  if (expr[[1]] == as.name("/")) return(diff_division(expr[[2]], expr[[3]], x))

  if (expr[[1]] == as.name("^")) return(diff_exponentiation(expr[[2]], expr[[3]], x))

  stop(paste0("Unexpected call ", deparse(expr)))
}
