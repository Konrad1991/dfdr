#' Compute the gradient-function of a function.
#'
#' Creates a function that computes the derivative of a function with respect to each parameter
#' and return a vector of these.
#'
#' @param f  A function
#' @param vars The variables to compute the derivatives of. If NULL
#'             the gradient will be computed for all formal arguments
#'             of f.
#' @param use_names Should the gradient add variable names to the output of the function?
#' @return  A function that computes the gradient of f at any point.
#' @export
gradient <- function(f, use_names, ...) {
  args <- rlang::ensyms(...) 
  vars <- purrr::map(args, rlang::as_string)
  stopifnot("Variable names for which gradients should be calculated are missing"=length(vars)>0)
  derivatives <- list()
  for(i in vars) {
    k <- rlang::enexpr(i)
    derivatives <- c(derivatives, body(d(f, !!k)))
  }
  if (use_names) {
    derivatives <- derivatives |> setNames(vars)
  }
  rlang::new_function(
    formals(f),
    rlang::call_modify(quote(c()), !!!derivatives),
    environment(f))
}

#' Compute the Hessian-function of a function.
#'
#' Creates a function that computes the second-order derivatives of a function with respect to each pair of parameters
#' and return a vector of these.
#'
#' @param f  A function
#' @param vars The variables to compute the derivatives of. If NULL
#'             the gradient will be computed for all formal arguments
#'             of f.
#' @param use_names Should the gradient add variable names to the output of the function?
#' @return  A function that computes the gradient of f at any point.
#' @export
hessian <- function(f, use_names = FALSE, ...) {
  args <- rlang::ensyms(...) 
  vars <- purrr::map(args, rlang::as_string)
  stopifnot("Variable names for which gradients should be calculated are missing"=length(vars)>0)
  
  first_derivatives <- Map(function(v) d(f, !!rlang::enexpr(v)), vars)
  names(first_derivatives) <- vars
  second_derivatives <- first_derivatives # just an easy hack to get the right length with right names
  for (var in vars) {
    df <- first_derivatives[[var]]
    second_derivatives[[var]] <- Map(function(v) d(df, !!rlang::enexpr(v)), vars)
    names(second_derivatives[[var]]) <- vars
  }
  function(...) {
    H <- matrix(nrow = length(vars), ncol = length(vars))
    if (use_names) rownames(H) <- colnames(H) <- as.character(unlist(vars))
    for (i in seq_along(vars)) {
      v1 <- vars[i]
      for (j in seq_along(vars)) {
        v2 <- vars[j]
        df <- second_derivatives[[unlist(v1)]][[unlist(v2)]]
        H[i, j] <- df(...)
      }
    }
    H
  }
}

