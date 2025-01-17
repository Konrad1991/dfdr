# check valid input for `[`
check_bracket <- function(ex) {
  if (length(ex) == 3) {
    return(ex[[3]] %% 1 == 0)
  }
  if (length(ex) == 4) {
    return((ex[[3]] %% 1 == 0) && (ex[[4]] %% 1 == 0))
  }
}

# Lifts a function so it will propagate NULL and otherwise do its thing
lift <- function(f) {
  function(x, ...) if (rlang::is_null(x)) x else f(x, ...)
}

handle_primitives <- function(f) {
  if (identical(f, sin)) {
    return(function(x) cos(x))
  }
  if (identical(f, sinh)) {
    return(cosh)
  }
  if (identical(f, asin)) {
    return(function(x) 1 / sqrt(1 - x^2))
  }
  if (identical(f, cos)) {
    return(function(x) -sin(x))
  }
  if (identical(f, cosh)) {
    return(function(x) sinh(x))
  }
  if (identical(f, acos)) {
    return(function(x) -asin(x))
  }
  if (identical(f, exp)) {
    return(function(x) exp(x))
  }
  if (identical(f, tan)) {
    return(function(x) 1 / cos(x)^2)
  }
  if (identical(f, tanh)) {
    return(function(x) 1 - tanh(x^2))
  }
  if (identical(f, atan)) {
    return(function(x) 1 / (1 + x^2))
  }
  if (identical(f, sqrt)) {
    return(function(x) 1 / (2 * sqrt(x)))
  }
  if (identical(f, log)) {
    return(function(x) 1 / x)
  }
  stop("unknown primitive") # nocov
}

d <- function(f, x, derivs = NULL) {
  x <- rlang::enexpr(x)
  fl <- init_fct_list()
  if (!is.null(derivs)) {
    fd <- derivs@funs
    for (i in seq_along(fd)) {
      fl <- append_fct(fl, fd[[i]]@name, fd[[i]]@dfdx, fd[[i]]@name_deriv, fd[[i]]@keep)
    }
  }

  # Primitive functions, we have to treat carefully. They don't have a body.
  # This is just a short list of such built-in arithmetic functions, it is
  # not exhaustive.
  if (is.null(body(f))) {
    return(handle_primitives(f))
  } else {
    # for other functions we have to parse the body
    # and differentiate it.
    df <- f
    body(df) <- simplify_expr(diff_expr(body(f), x, fl, FALSE, FALSE))
    df
  }
}

d_internal <- function(f, x, derivs = NULL, Tape) {
  x <- rlang::enexpr(x)
  fl <- init_fct_list()
  if (!is.null(derivs)) {
    fd <- derivs@funs
    for (i in seq_along(fd)) {
      fl <- append_fct(fl, fd[[i]]@name, fd[[i]]@dfdx, fd[[i]]@name_deriv, fd[[i]]@keep)
    }
  }

  # Primitive functions, we have to treat carefully. They don't have a body.
  # This is just a short list of such built-in arithmetic functions, it is
  # not exhaustive.
  if (is.null(body(f))) {
    return(handle_primitives(f))
  } else {
    # for other functions we have to parse the body
    # and differentiate it.
    df <- f
    body(df) <- simplify_expr(diff_expr(body(f), x, fl, Tape, FALSE))
    df
  }
}

diff_vector_out <- function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  d_args <- expr |>
    rlang::call_args() |>
    purrr::map(\(ex) diff_expr(ex, x, fl, Tape, Subsetted))
  as.call(c(as.name("c"), d_args))
}

diff_variable <- function(var, Tape, Subsetted) {
  if (Subsetted) {
    index <- Tape[[deparse(var)]]
    str2lang(paste0("Tape[[", index, "]]"))
  } else {
    index <- Tape[[deparse(var)]]
    str2lang(paste0("Tape[[", index, "]][", Tape[["IDX"]], ", TRUE]"))
  }
}

diff_expr <- lift(function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  if (is.list(Tape)) {
    if (is.numeric(expr)) {
      quote(0)
    } else if (is.name(expr)) {
      diff_variable(expr, Tape, Subsetted)
    } else if (is.call(expr)) {
      diff_call(expr, x, fl, Tape, Subsetted)
    } else {
      stop(paste0("Unexpected expression ", deparse(expr), " in parsing.")) # nocov
    }
  } else {
    if (is.call(expr)) {
      if (as.name("[") == expr[[1]]) {
        stopifnot("Only integers in [] allowed" = check_bracket(expr))
        if (expr == x) {
          return(quote(1))
        } else {
          return(quote(0))
        }
      }
    }
    if (is.numeric(expr)) {
      quote(0)
    } else if (is.name(expr) && expr == x) {
      quote(1)
    } else if (is.name(expr)) {
      quote(0)
    } else if (is.call(expr)) {
      diff_call(expr, x, fl, Tape, Subsetted)
    } else {
      stop(paste0("Unexpected expression ", deparse(expr), " in parsing.")) # nocov
    }
  }
})

diff_addition <- function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  lhs <- call_arg(expr, 1) |> diff_expr(x, fl, Tape, Subsetted)
  rhs <- call_arg(expr, 2) |> diff_expr(x, fl, Tape, Subsetted)
  bquote(.(lhs) + .(rhs))
}

diff_subtraction <- function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  lhs <- call_arg(expr, 1) |> diff_expr(x, fl, Tape, Subsetted)
  rhs <- call_arg(expr, 2) |> diff_expr(x, fl, Tape, Subsetted)
  if (rlang::is_null(rhs)) {
    bquote(-.(lhs))
  } else {
    bquote(.(lhs) - .(rhs))
  }
}

diff_multiplication <- function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  # f' g + f g'
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl, Tape, Subsetted)
  dg <- diff_expr(g, x, fl, Tape, Subsetted)
  bquote(.(df) * .(g) + .(f) * .(dg))
}

diff_division <- function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  # (f' g − f g' )/g**2
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl, Tape, Subsetted)
  dg <- diff_expr(g, x, fl, Tape, Subsetted)
  bquote((.(df) * .(g) - .(f) * .(dg)) / .(g)**2)
}

diff_exponentiation <- function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  # Using the chain rule to handle this generally.
  # if y = f**g then dy/dx = dy/df df/dx = g * f**(g-1) * df/dx
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl, Tape, Subsetted)
  bquote(.(g) * .(f)**(.(g) - 1) * .(df))
}

diff_built_in_function_call <- lift(function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  # chain rule with a known function to differentiate. df/dx = df/dy dy/dx
  name <- call_name(expr)
  keep <- get_keep(fl, name)
  if (keep) {
    message(paste("Found function", name, "which should be kept constant. This function is not considered for calculating the derivatives. Notably, also the arguments of the functions are ignored!"))
    return(0)
  }

  name_deriv <- get_derivative_name(fl, name)
  len <- length(expr)
  args <- sapply(seq_along(2:len), function(x) call_arg(expr, x))
  dy_dx <- sapply(args, function(as) diff_expr(as, x, fl, Tape, Subsetted))
  if (!is.list(args)) args <- as.list(args)
  deriv_args <- formalArgs(get_derivative(fl, name))
  deriv_args <- lapply(deriv_args, str2lang)
  if (length(args) != length(deriv_args)) stop(paste("wrong number of args for function", name))
  outer_deriv <- get_derivative(fl, name) |> body()
  outer_deriv <- deparse(outer_deriv)
  outer_deriv <- parse(text = outer_deriv)[[1]]
  od <- list()
  counter <- 1
  for (i in seq_along(args)) {
    od[[counter]] <- pryr::modify_lang(outer_deriv, substi, bquote(.(deriv_args[[i]])), args[[i]])
    counter <- counter + 1
  }
  outer_deriv <- od
  entire_deriv <- NULL
  for (i in seq_along(args)) {
    id <- dy_dx[[i]]
    od <- outer_deriv[[i]]
    entire_deriv <- c(entire_deriv, bquote(.(id) * .(od)))
  }

  for (i in seq_along(entire_deriv)) {
    deriv_current <- entire_deriv[[i]]
    deriv_current <- simplify_expr(deriv_current)
    if (deriv_current == 0) {
      entire_deriv[[i]] <- NA
    } else {
      entire_deriv[[i]] <- deriv_current
    }
  }
  entire_deriv <- entire_deriv[!is.na(entire_deriv)]
  if (len > 2) {
    entire_deriv <- paste(entire_deriv, collapse = "+")
  } else {
    entire_deriv <- paste(entire_deriv)
  }
  if (identical(entire_deriv, character(0))) {
    return(str2lang("0"))
  }
  if (entire_deriv == "") {
    return(str2lang("0"))
  }

  str2lang(entire_deriv)
})

diff_parens <- function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  subexpr <- diff_expr(call_arg(expr, 1), x, fl, Tape, Subsetted)
  if (is.atomic(subexpr) || is.name(subexpr)) {
    subexpr
  } else if (is.call(subexpr) && call_name(subexpr) == "(") {
    subexpr
  } else {
    call("(", subexpr)
  }
}

diff_bracket <- function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  # f'[g]
  f <- call_arg(expr, 1)
  g <- call_arg(expr, 2)
  df <- diff_expr(f, x, fl, Tape, TRUE)
  row_index <- Tape[["IDX"]] |> str2lang()
  bquote(.(df)[.(row_index), .(g)])
}

diff_call <- lift(function(expr, x, fl, Tape = FALSE, Subsetted = FALSE) {
  arg1 <- call_arg(expr, 1)
  arg2 <- call_arg(expr, 2)
  if (is.list(Tape)) {
    e <- call_name(expr)
    if (is.name(e)) {
      if (e == "+") {
        diff_addition(expr, x, fl, Tape, Subsetted)
      } else if (e == "-") {
        diff_subtraction(expr, x, fl, Tape, Subsetted)
      } else if (e == "*") {
        diff_multiplication(expr, x, fl, Tape, Subsetted)
      } else if (e == "/") {
        diff_division(expr, x, fl, Tape, Subsetted)
      } else if (e == "^") {
        diff_exponentiation(expr, x, fl, Tape, Subsetted)
      } else if (e == "(") {
        diff_parens(expr, x, fl, Tape, Subsetted)
      } else if (e == "[") {
        diff_bracket(expr, x, fl, Tape, Subsetted)
      } else if ((as.character(e) %in% get_names(fl))) {
        diff_built_in_function_call(expr, x, fl, Tape, Subsetted)
      } else {
        stop(paste("The function", expr, "is not supported"))
      }
    }

  } else {
    e <- call_name(expr)
    if (is.name(e)) {
      if (e == "+") {
        diff_addition(expr, x, fl, Tape, Subsetted)
      } else if (e == "-") {
        diff_subtraction(expr, x, fl, Tape, Subsetted)
      } else if (e == "*") {
        diff_multiplication(expr, x, fl, Tape, Subsetted)
      } else if (e == "/") {
        diff_division(expr, x, fl, Tape, Subsetted)
      } else if (e == "^") {
        diff_exponentiation(expr, x, fl, Tape, Subsetted)
      } else if (e == "(") {
        diff_parens(expr, x, fl, Tape, Subsetted)
      } else if ((as.character(e) %in% get_names(fl))) {
        diff_built_in_function_call(expr, x, fl, Tape, Subsetted)
      } else {
        stop(paste("The function", expr, "is not supported"))
      }
    }
  }
})
