is_assign <- function(list) {
  if (length(list) == 3) {
    if (deparse(list[[1]]) %in% c("=", "<-")) {
      return(TRUE)
    }
  }
  return(FALSE)
}

add_var <- function(line, env) {
  if (!is_assign(line)) {
    return()
  }
  variable <- line[[2]] |>
    all.vars() # NOTE: to ignore subsetting
  env$variable_list <- union(env$variable_list, variable)
}

find_vars_in_line <- function(line, env) {
  if (!is.call(line)) {
    return(line)
  }
  line <- as.list(line)
  add_var(line, env)
  lapply(line, function(x) {
    find_vars_in_line(x, env)
  })
}

find_all_vars_ls <- function(f) {
  body <- body(f)
  env <- new.env()
  env$variable_list <- character()
  for (i in seq_along(body)) {
    find_vars_in_line(body[[i]], env)
  }
  return(env$variable_list)
}
test_fct <- function() {
  a <- b + sin(c)
  y[1] <- x
  if (1 == 2) {
    d <- 3
    a <- 1
  }
}
find_all_vars_ls(test_fct)

create_tape <- function(f) {
  all_vars <- all.vars(body(f))
  stopifnot("Tape cannot be used as variable name" = !("Tape" %in% all_vars))
  all_vars_left <- find_all_vars_ls(f)
  tape_list <- lapply(all_vars_left, function(x) {
    0
  })
  names(tape_list) <- all_vars_left
  assign("Tape", tape_list)
  return(Tape)
}
Tape <- create_tape(test_fct)
Tape

diff_test_fct <- function() x * x
dfdr:::d_internal(diff_test_fct, "x")

jacobian_new <- function(f, y, x, derivs = NULL, num_functions = NULL) {
  y <- rlang::ensym(y)
  y <- rlang::as_string(y)
  x <- rlang::ensym(x)
  x <- rlang::as_string(x)

  stopifnot("the function is missing" = !is.null(f))
  stopifnot("the variable y is missing" = !is.null(y))
  stopifnot("the variable x is missing" = !is.null(x))

  if (!is.null(num_functions)) {
    stopifnot("the num_functions arguments has to be of type numeric" = is.numeric(num_functions))
  }

  # create function list
  # ============================================================================
  fl <- init_fct_list()
  if (!is.null(derivs)) {
    fd <- derivs@funs
    for (i in seq_along(fd)) {
      fl <- append_fct(fl, fd[[i]]@name, fd[[i]]@dfdx, fd[[i]]@name_deriv, fd[[i]]@keep)
    }
  }
  fct_list <- get_names(fl)

  const_fcts <- character()
  for (i in fct_list) {
    if (get_keep(fl, fct_list[i])) const_fcts <- c(const_fcts, fct_list[i])
  }

  # Find all variables which occure in a statement involving an assignment
  # ============================================================================

  # Create tape variable and initialise it for each variable (take the length of x)
  # ============================================================================

  # Create diff function which does not
  # if (expr == x) {
  #   return(quote(1))
  # } else {
  #   return(quote(0))
  # } But instead
  # return(quote(tape[["x"]]))
  # ============================================================================

  # Loop over body and replace each assignment expression with
  # the assignment itself (not modified) and the result of diff.
  # The result of diff should be stored in the corresponding tape index
  # In case a variable other than "y" itself is found either the value of the variable
  # or the entry of the tape is used.
  # ============================================================================
  # INFO: Idea
  # tape <- list(
  #   "y" = list(value = y_value, derivative = dy_dx),
  #   "x" = list(value = x_value, derivative = dx_dx)
  # )

  return(f)
}
