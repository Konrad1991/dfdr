# missing functions
#   sinh, cosh, tanh
#   asin, acos, atan
#   sqrt
#   if, else if, else
#   cmr
#   [

# other functions --> currently don't know what to do with them
#   :      
#   c
#   matrix, vector
#   =, <- 

# forbidden functions
#   for-loop
#   RNG functions



# Lifts a function so it will propagate NULL and otherwise do its thing
lift <- function(f) {
  function(x, ...) if (rlang::is_null(x)) x else f(x, ...)
}


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
    if (identical(f, sinh)) return(cosh)
    #if (identical(f, asin)) 
    if (identical(f, cos)) return(function(x) -sin(x))
    if (identical(f, cosh)) return(sinh)
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

diff_vector_out <- function(expr, x, e) {
  d_args <- expr |>
    rlang::call_args() |>
    purrr::map(\(ex) diff_expr(ex, x, e))
  as.call(c(as.name("c"), d_args))
}

diff_expr <- lift(function(expr, x, e) {
  expr |> purrr::when(
    is.numeric(.)              ~ quote(0),
    is.name(.) && . == x       ~ quote(1),
    is.name(.)                 ~ quote(0),
    is.call(.)                 ~ diff_call(expr, x, e),
    ~ stop(paste0("Unexpected expression ", deparse(expr), " in parsing.")) # nocov
  )
})

diff_addition <- function(expr, x, e) {
  lhs <- call_arg(expr, 1) |> diff_expr(x, e)
  rhs <- call_arg(expr, 2) |> diff_expr(x, e)
  bquote( .(lhs) + .(rhs) )
}

diff_subtraction <- function(expr, x, e) {
  lhs <- call_arg(expr, 1) |> diff_expr(x, e)
  rhs <- call_arg(expr, 2) |> diff_expr(x, e)
  if (rlang::is_null(rhs)) bquote( -.(lhs) )
  else bquote( .(lhs) - .(rhs) )
}

diff_multiplication <- function(expr, x, e) {
  # f' g + f g'
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, e)
  dg <- diff_expr(g, x, e)
  bquote( .(df)*.(g) + .(f)*.(dg) )
}

diff_division <- function(expr, x, e) {
  # (f' g − f g' )/g**2
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, e)
  dg <- diff_expr(g, x, e)
  bquote( ( .(df)*.(g) - .(f)*.(dg) ) / .(g)**2 )
}

diff_exponentiation <- function(expr, x, e) {
  # Using the chain rule to handle this generally.
  # if y = f**g then dy/dx = dy/df df/dx = g * f**(g-1) * df/dx
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, e)
  bquote( .(g) * .(f)**(.(g)-1) * .(df) )
}

# FIXME: This is a mess with only a few functions I could think of handled...
.built_in_functions <- c("sin", "cos", "exp")
diff_built_in_function_call <- lift(function(expr, x, e) {
  # chain rule with a known function to differentiate. df/dx = df/dy dy/dx
  y <- call_arg(expr, 1)
  dy_dx <- diff_expr(call_arg(expr, 1), x, e)
  call_name(expr) |> purrr::when(
    . == "sin" ~ bquote(  cos(.(y)) * .(dy_dx)),
    . == "cos" ~ bquote( -sin(.(y)) * .(dy_dx)),
    . == "exp" ~ bquote(  exp(.(y)) * .(dy_dx))
  )
})


diff_general_function_call <- lift(function(expr, x, e) {
  function_name <- call_name(expr)
  if (!is.name(function_name))
    stop(paste0("Unexpected call ", deparse(expr)))

  func <- get(function_name, e)
  full_call <- rlang::call_match(expr, func)
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
})

diff_parens <- function(expr, x, e) {
  subexpr <- diff_expr(call_arg(expr, 1), x, e)
  if (is.atomic(subexpr) || is.name(subexpr)) subexpr
  else if (is.call(subexpr) && call_name(subexpr) == "(")
    subexpr
  else
    call("(", subexpr)
}


diff_call <- lift(function(expr, x, e) {
  arg1 <- call_arg(expr, 1)
  arg2 <- call_arg(expr, 2)
  call_name(expr) |> purrr::when(
    is.name(.) ~ . |> purrr::when(
      . == "+" ~ diff_addition(expr, x, e),
      . == "-" ~ diff_subtraction(expr, x, e),
      . == "*" ~ diff_multiplication(expr, x, e),
      . == "/" ~ diff_division(expr, x, e),
      . == "^" ~ diff_exponentiation(expr, x, e),
      . == "(" ~ diff_parens(expr, x, e),
      (as.character(.) %in% .built_in_functions) ~ diff_built_in_function_call(expr, x, e),
      . == "c" ~ diff_vector_out(expr, x, e),
      ~ diff_general_function_call(expr, x, e)
    ),
    ~ diff_general_function_call(expr, x, e)
  )
})
