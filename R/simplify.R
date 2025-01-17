# Literals should be numbers or variables. Not any atomic type will do
# if we expect to differentiate.
is_literal <- function(expr) {
  rlang::is_scalar_double(expr) ||
    rlang::is_scalar_integer(expr) ||
    rlang::is_scalar_logical(expr) || # TODO: check whether this is a problem;
    # INFO: This was added to enable subsetting the Tape entries
    is.name(expr)
}

# For simplifying arguments before we simplify a call
simplify_args <- function(expr) lapply(expr[2:length(expr)], simplify_expr)

# Get the operands from a call
lhs <- function(args) args[[1]]
rhs <- function(args) args[[2]]
call_name <- function(expr) {
  expr[[1]]
}
call_arg <- function(expr, arg) {
  # We allow this to return NULL when out of bounds, as that some
  # places simplifies the code. Just make sure that functions that can
  # get its result as an argument can deal with NULL
  if (arg < length(expr)) expr[[1 + arg]] else NULL
}
call_args <- function(expr) expr[2:length(expr)]

# Lifts a function so it will propagate NULL and otherwise do its thing
lift <- function(f) {
  function(x, ...) if (rlang::is_null(x)) x else f(x, ...)
}


# Error handling (rudimentary as it is)
simplify_error <- function(expr) {
  stop(paste0("Unexpected expression ", deparse(expr), " in simplifying"))
}


simplify <- function(expr) {
  if (is_literal(expr)) {
    identity(expr)
  } else if (rlang::is_call(expr)) {
    simplify_call(expr)
  } else {
    simplify_error(expr)
  }
}

simplify_expr <- lift(function(expr) {
  if (is_literal(expr)) {
    identity(expr)
  } else if (rlang::is_call(expr)) {
    simplify_call(expr)
  } else {
    simplify_error(expr)
  }
})


simplify_addition <- lift(function(expr) {
  expr <- simplify_args(expr)
  if (lhs(expr) == 0) {
    rhs(expr)
  } else if (rhs(expr) == 0) {
    lhs(expr)
  } else if (is.numeric(lhs(expr)) && is.numeric(rhs(expr))) {
    lhs(expr) + rhs(expr)
  } else {
    bquote(.(lhs(expr)) + .(rhs(expr)))
  }
})

simplify_unary_subtraction <- lift(function(expr) {
  expr <- simplify_args(expr) |> lhs()
  if (is.numeric(expr)) {
    (-expr)
  } else if (rlang::is_call(expr) && call_name(expr) == "-") {
    call_arg(expr, 1) # - - = +
  } else {
    bquote(-.(expr))
  }
})

simplify_subtraction <- lift(function(expr) {
  expr <- simplify_args(expr)
  if (is.numeric(lhs(expr)) && is.numeric(rhs(expr))) {
    lhs(expr) - rhs(expr)
  } else if (lhs(expr) == 0) {
    bquote(-.(rhs(expr)))
  } else if (rhs(expr) == 0) {
    lhs(expr)
  } else {
    bquote(.(lhs(expr)) - .(rhs(expr)))
  }
})

simplify_multiplication <- lift(function(expr) {
  expr <- simplify_args(expr)
  if (is.numeric(lhs(expr)) && is.numeric(rhs(expr))) {
    lhs(expr) * rhs(expr)
  } else if (lhs(expr) == 0 || rhs(expr) == 0) {
    0
  } else if (lhs(expr) == 1) {
    rhs(expr)
  } else if (rhs(expr) == 1) {
    lhs(expr)
  } else {
    bquote(.(lhs(expr)) * .(rhs(expr)))
  }
})

simplify_division <- lift(function(expr) {
  expr <- simplify_args(expr)
  if (is.numeric(lhs(expr)) && is.numeric(rhs(expr))) {
    lhs(expr) / rhs(expr)
  } else if (rhs(expr) == 1) {
    lhs(expr)
  } else {
    bquote(.(lhs(expr)) / .(rhs(expr)))
  }
})

simplify_exponentiation <- lift(function(expr) {
  expr <- simplify_args(expr)
  if (is.numeric(lhs(expr)) && is.numeric(rhs(expr))) {
    lhs(expr)^rhs(expr)
  } else if (rhs(expr) == 0) {
    1
  } else if (lhs(expr) == 0) {
    0
  } else if (lhs(expr) == 1) {
    1
  } else if (rhs(expr) == 1) {
    lhs(expr)
  } else {
    bquote(.(lhs(expr))^.(rhs(expr)))
  }
})

# FIXME: This might not be the best approach... I'm not sure how well exists() and get()
# will work for user-defined functions
known_function <- function(name) {
  exists(name) && is.function(get(name))
}

replace_arguments <- function(expr, new_args) {
  result <- as.call(c(call_name(expr), new_args))
  names(result) <- names(expr)
  result
}

simplify_function_call <- lift(function(expr) {
  function_name <- call_name(expr)
  arguments <- simplify_args(expr)
  if (all(purrr::map_lgl(arguments, is.numeric)) && known_function(function_name)) {
    # if we have simplified all expressions we might as well try calling the function
    # if it is a function we know...
    do.call(get(function_name), arguments)
  } else {
    replace_arguments(expr, arguments)
  }
})

simplify_parens <- lift(function(expr) {
  expr <- call_arg(expr, 1) |> simplify_expr()
  if (is.atomic(expr) || is.name(expr) ||
    (rlang::is_call(expr) && call_name(expr) == "(")) {
    expr
  } else {
    bquote((.(expr)))
  }
})

simplify_call <- lift(function(expr) {
  simplifier <- function(expr) {
    e <- call_name(expr)
    if (is.name(e)) {
      if (e == "+") {
        simplify_addition
      } else if (e == "-" && length(expr) == 2) {
        simplify_unary_subtraction
      } else if (e == "-") {
        simplify_subtraction
      } else if (e == "*") {
        simplify_multiplication
      } else if (e == "/") {
        simplify_division
      } else if (e == "^") {
        simplify_exponentiation
      } else if (e == "**") {
        simplify_exponentiation
      } else if (e == "(") {
        simplify_parens
      } else {
        simplify_function_call
      }
    }
  }
  simplifier <- simplifier(expr)
  simplifier(expr)
})
