# INFO: Find vars at left side of assignment
find_assignment_variables <- function(f) {
  # INFO: Check if the line represents an assignment operation
  is_assign <- function(list) {
    if (length(list) == 3) {
      if (deparse(list[[1]]) %in% c("=", "<-")) {
        return(TRUE)
      }
    }
    return(FALSE)
  }

  # INFO: Add variables found on the left-hand side of assignments to the environment
  add_var <- function(line, env) {
    if (!is_assign(line)) {
      return()
    }
    variable <- line[[2]] |>
      all.vars() # ignoring subsetting
    env$variable_list <- union(env$variable_list, variable)
  }

  # INFO: Recursively find variables in an expression
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

  # INFO: Collect all variables assigned to in a function body
  body <- body(f)
  env <- new.env()
  env$variable_list <- character()
  for (i in seq_along(body)) {
    find_vars_in_line(body[[i]], env)
  }
  return(env$variable_list)

}

# INFO: Create the info required during calculation of derivatives
# INFO: Create a symbol table for variables used in the function
initialize_symbol_table <- function(vars_with_deriv) {
  symbol_table <- setNames(vector("list", length(vars_with_deriv)), vars_with_deriv)
  for (i in seq_along(vars_with_deriv)) symbol_table[[i]] <- i
  return(symbol_table)
}

create_symbol_table <- function(f) {
  var_args <- formalArgs(f)
  vars_body <- all.vars(body(f))
  all_vars <- union(var_args, vars_body)
  stopifnot("Tape cannot be used as variable name" = !("Tape" %in% all_vars))
  stopifnot("IDX cannot be used as variable name" = !("IDX" %in% all_vars))
  all_vars_left <- find_assignment_variables(f)
  vars_with_deriv <- union(var_args, all_vars_left)
  return(initialize_symbol_table(vars_with_deriv))
}

# INFO: Construct the full derivative line
calc_deriv_line <- function(line, x, symbol_table) {
  # INFO: calculate the derivative and retrieve derivatives from Tape
  # 
  # J = [
  #   df1/dx1 ... df1/dxn
  #   .            .
  #   .            .
  #   .            .
  #   dfm/dx1 ... dfm/dxn
  # ]
  # - f corresponds to variables which possess derivatives
  # - Variables which are passed to the function or are found at
  #   the left side of an assignment possess derivatives
  # - The Tape is a list containing the derivatives for each variable
  #     * Each entry in the Tape is a m x n matrix
  # - f is a vector [f1 ... fm]
  # - If a variable on the left side is subsetted:
  #   * then the index is used as row index for
  #     each Tape entry in the entire line
  # - If a variable on the right side is subsetted:
  #   * then the index is used as column index for the
  #     corresponding Tape entry of the subsetted variable.

  # INFO: Extract the derivative for the RHS of an assignment
  calc_deriv_rhs <- function(line, x, symbol_table) {
    rhs_fct <- function(x) {}
    body(rhs_fct) <- line[[3]]
    rhs <- d_internal(rhs_fct, x, Tape = symbol_table) |>
      body() |>
      deparse()
    rhs <- paste(rhs, collapse = "")
    rhs
  }

  # INFO: Retrieve the row index from the LHS of an assignment
  get_row_index <- function(line) {
    lhs <- line[[2]]
    if (length(lhs) == 1) {
      return("")
    } else {
      lhs_list <- as.list(lhs)
      row_index <- lhs_list[[3]] |> deparse()
      return(row_index)
    }
  }

  # INFO: Generate the LHS Tape expression for the derivative
  calc_lhs <- function(line, symbol_table) {
    lhs <- line[[2]]
    if (length(lhs) == 1) {
      index <- symbol_table[[deparse(lhs)]]
      str2lang(paste0("Tape[[", index, "]][]"))
    } else {
      lhs_list <- as.list(lhs)
      lhs_var <- lhs_list[[2]]
      row_index <- lhs_list[[3]] |> deparse()
      index <- symbol_table[[deparse(lhs_var)]]
      if (is.null(index)) {
        stop(paste0("Did not found variable in Tape: ", deparse(lhs_var)))
      }
      str2lang(sprintf("Tape[[%s]][%s, ]", index, row_index))
    }
  }

  lhs <- calc_lhs(line, symbol_table)
  row_index <- get_row_index(line)
  if (row_index == "") row_index <- TRUE
  symbol_table[["IDX"]] <- row_index
  rhs <- calc_deriv_rhs(line, x, symbol_table)
  str2lang(paste0(deparse(lhs), " <- ", rhs))
}

diff_test <- function(f, x) {
  b <- 1
  a <- b * x
  f[a] <- x[2] * x[1] + a
}
symbol_table <- create_symbol_table(diff_test)
calc_deriv_line(body(diff_test)[[3]], "x", symbol_table)
calc_deriv_line(body(diff_test)[[4]], "x", symbol_table)

source("./R/simplify.R")
source("./R/differentiate.R")
source("./R/fct_deriv_pairs.R")
source("./R/helper.R")


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

  return(f)
}
