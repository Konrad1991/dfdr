# check valid input for `[`
check_bracket <- function(ex) {
  if(length(ex) == 3) return(ex[[3]]%%1==0)
  if(length(ex) == 4) return( (ex[[3]]%%1==0) && (ex[[4]]%%1==0) )
}

# Lifts a function so it will propagate NULL and otherwise do its thing
lift <- function(f) {
  function(x, ...) if (rlang::is_null(x)) x else f(x, ...)
}

#' Differentiate a function for a single variable.
#'
#' @param f The function to differentiate.
#' @param x The variable that f should be differentiated with respect to.
#' @param derivs An S4 class of type \emph{fcts} that defines additional derivatives. See \code{\link{fcts}} for details.
#' @param const is a variable which is used internally by \code{\link{jacobian}()}. A environment is expected which holds a logical value called \emph{const}. \cr
#'              In case a function is found which should be ignored the value of \emph{const} is set to TRUE.
#'              
#' @details 
#' The following functions are already supported: \cr
#' sin, sinh, asin, cos, cosh, acos, tan, tanh, atan, exp, log, sqrt, c, vector, numeric, rep and matrix. \cr
#' Notably, for the functions: c, vector, numeric, rep and matrix the function is ignored during differentiation.              
#'              
#' @return For example function f and symbol x: \cr
#' \emph{df/dx}
#' @examples 
#' library(dfdr)
#' d(sin, x)
#' 
#' f <- function(x) -sin(x)
#' d(f, x)
#' 
#' # Initialize list
#' lst <- dfdr::fcts()
#' # The function which should be added
#' f <- function(x) x^2
#' # The dervative function of f
#' f_deriv <- function(x) 2*x
#' # add new entry to list
#' lst <- fcts_add_fct(lst, f, f_deriv)
#' g <- function(z) f(z)
#' d(g, z, lst)
#' @export
d <- function(f, x, derivs = NULL, const = NULL) {
  x <- rlang::enexpr(x)
  fl <- init_fct_list()
  if(!is.null(derivs)) {
    fd <- derivs@funs
    for(i in seq_along(fd)) {
      fl <- append_fct(fl, fd[[i]]@name, fd[[i]]@dfdx, fd[[i]]@name_deriv, fd[[i]]@keep)
    }
  }
  
  if(is.null(const)) const <- new.env()
  const$const <- FALSE

  # Primitive functions, we have to treat carefully. They don't have a body.
  # This is just a short list of such built-in arithmetic functions, it is
  # not exhaustive.
  if (is.null(body(f))) {
    if (identical(f, sin))  return(cos)
    if (identical(f, sinh)) return(cosh)
    if (identical(f, asin)) return(function(x) 1/sqrt(1 - x^2))
    if (identical(f, cos))  return(function(x) -sin(x))
    if (identical(f, cosh)) return(sinh)
    if (identical(f, acos)) return(function(x) -asin(x))
    if (identical(f, exp))  return(exp)
    if (identical(f, tan))  return(function(x) 1/cos(x)^2)
    if (identical(f, tanh)) return(function(x) 1 - tanh(x^2))
    if (identical(f, atan)) return(function(x) 1/(1 + x^2) )
    if (identical(f, sqrt)) return(function(x) 1/(2*sqrt(x)) )
    if (identical(f, log))  return(function(x) 1/x)
    stop("unknown primitive") # nocov

  } else {
    # for other functions we have to parse the body
    # and differentiate it.
    df <- f
    body(df) <- simplify_expr(diff_expr(body(f), x, fl, const)) 
    df
  }
}

diff_vector_out <- function(expr, x, fl, const) {
  d_args <- expr |>
    rlang::call_args() |>
    purrr::map(\(ex) diff_expr(ex, x, fl, const))
  as.call(c(as.name("c"), d_args))
}

diff_expr <- lift(function(expr, x, fl, const) {
  if(is.call(expr)) {
    if(as.name("[") == expr[[1]]) {
      stopifnot("Only integers in [] allowed"=check_bracket(expr))
      if(expr == x) {
        return(quote(1))
      } else {
        return(quote(0))
      }
      
    }
  }

  expr |> purrr::when(
    is.numeric(.)              ~ quote(0),
    is.name(.) && . == x       ~ quote(1), 
    is.name(.)                 ~ quote(0),
    is.call(.)                 ~ diff_call(expr, x, fl, const),
    ~ stop(paste0("Unexpected expression ", deparse(expr), " in parsing.")) # nocov
  )
})

diff_addition <- function(expr, x, fl, const) {
  lhs <- call_arg(expr, 1) |> diff_expr(x, fl, const)
  rhs <- call_arg(expr, 2) |> diff_expr(x, fl, const)
  bquote( .(lhs) + .(rhs) )
}

diff_subtraction <- function(expr, x, fl, const) {
  lhs <- call_arg(expr, 1) |> diff_expr(x, fl, const)
  rhs <- call_arg(expr, 2) |> diff_expr(x, fl, const)
  if (rlang::is_null(rhs)) bquote( -.(lhs) )
  else bquote( .(lhs) - .(rhs) )
}

diff_multiplication <- function(expr, x, fl, const) {
  # f' g + f g'
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl, const)
  dg <- diff_expr(g, x, fl, const)
  bquote( .(df)*.(g) + .(f)*.(dg) )
}

diff_division <- function(expr, x, fl, const) {
  # (f' g − f g' )/g**2
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl, const)
  dg <- diff_expr(g, x, fl, const)
  bquote( ( .(df)*.(g) - .(f)*.(dg) ) / .(g)**2 )
}

diff_exponentiation <- function(expr, x, fl, const) {
  # Using the chain rule to handle this generally.
  # if y = f**g then dy/dx = dy/df df/dx = g * f**(g-1) * df/dx
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl, const)
  bquote( .(g) * .(f)**(.(g)-1) * .(df) )
}

diff_built_in_function_call <- lift(function(expr, x, fl, const) {
  # chain rule with a known function to differentiate. df/dx = df/dy dy/dx
  name <- call_name(expr)
  keep <- get_keep(fl, name)
  if(keep) {
    warning(paste("Found function", name,  "which should be kept constant. This function is not considered for calculating the derivatives. Notably, also the arguments of the functions are ignored!") )
    const$const <- TRUE 
    return(0)
  }
  
  name_deriv <- get_derivative_name(fl, name)
  len <- length(expr)
  args <- sapply(seq_along(2:len), function(x) call_arg(expr, x))
  dy_dx <- sapply(args, function(as) diff_expr(as, x, fl, const) )
  if(!is.list(args)) args <- as.list(args)
  deriv_args <- formalArgs(get_derivative(fl, name))
  deriv_args <- lapply(deriv_args, str2lang)
  if(length(args)!=length(deriv_args)) stop(paste("wrong number of args for function", name))
  outer_deriv <- get_derivative(fl, name) |>  body()
  outer_deriv <- deparse(outer_deriv)
  outer_deriv <- parse(text = outer_deriv)[[1]]
  od <- list()
  counter <- 1
  for(i in seq_along(args)) {
    od[[counter]] <- pryr::modify_lang(outer_deriv, substi, bquote( .(deriv_args[[i]]) ), args[[i]] )
    counter <- counter + 1
  }
  outer_deriv <- od
  entire_deriv <- NULL
  for(i in seq_along(args)) {
    id <- dy_dx[[i]]
    od <- outer_deriv[[i]]
    entire_deriv = c(entire_deriv, bquote(.(id) * .(od)) )  
  }
  
  for(i in seq_along(entire_deriv)) {
    deriv_current <- entire_deriv[[i]]
    deriv_current <- simplify_expr(deriv_current)
    if(deriv_current == 0) {
      entire_deriv[[i]] <- NA
    } else {
      entire_deriv[[i]] <- deriv_current
    }
  }
  entire_deriv <- entire_deriv[!is.na(entire_deriv)]
  if(len > 2) {
    entire_deriv <- paste(entire_deriv, collapse = "+")
  } else {
    entire_deriv <- paste(entire_deriv)
  }
  if(identical(entire_deriv, character(0))) {
    return(str2lang("0"))
  }
  if(entire_deriv == "") {
    return(str2lang("0"))
  }

  str2lang(entire_deriv)
})

diff_parens <- function(expr, x, fl, const) {
  subexpr <- diff_expr(call_arg(expr, 1), x, fl, const)
  if (is.atomic(subexpr) || is.name(subexpr)) subexpr
  else if (is.call(subexpr) && call_name(subexpr) == "(")
    subexpr
  else
    call("(", subexpr)
}

diff_call <- lift(function(expr, x, fl, const) {
  arg1 <- call_arg(expr, 1)
  arg2 <- call_arg(expr, 2)
  call_name(expr) |> purrr::when(
    is.name(.) ~ . |> purrr::when(
      . == "+" ~ diff_addition(expr, x, fl, const),
      . == "-" ~ diff_subtraction(expr, x, fl, const),
      . == "*" ~ diff_multiplication(expr, x, fl, const),
      . == "/" ~ diff_division(expr, x, fl, const),
      . == "^" ~ diff_exponentiation(expr, x, fl, const),
      . == "(" ~ diff_parens(expr, x, fl, const),
      (as.character(.) %in% get_names(fl)) ~ diff_built_in_function_call(expr, x, fl, const),
      . == "c" ~ diff_vector_out(expr, x, fl, const),
      ~ stop(paste("The function", ., "is not supported"))
    ),
    ~ stop(paste("The function", ., "is not supported"))
  )
})