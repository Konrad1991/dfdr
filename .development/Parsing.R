transf_fct_name <- function(name) {
  if (name == "+") {
    return("add")
  }
  if (name == "-") {
    return("sub")
  }
  if (name == "*") {
    return("mul")
  }
  if (name == "/") {
    return("div")
  }
  if (name == "[") {
    return("subsetting")
  }
  if (name == "c") {
    return("concatenate")
  }
  name
}

is_assign <- function(line) {
  deparse(line[[1]]) %in% c("<-", "=")
}

is_concatenate <- function(line) {
  deparse(line[[1]]) == "c"
}

is_subset <- function(line) {
  deparse(line[[1]]) == "["
}

is_call <- function(line) {
  is.call(line)
}

# INFO: test for scalar literals.
# TRUE: Logical, Integer and double
is_literal <- function(obj) {
  is.atomic(obj) && (is.logical(obj) || is.integer(obj) || is.double(obj))
}

# INFO: Creates idx_name
create_name_var <- function(name, env) {
  name <- gsub("[0-9]_", "", name)
  if (!(name %in% names(env$variable_list))) {
    env$variable_list[[name]] <- 0
  } else {
    env$variable_list[[name]] <- env$variable_list[[name]] + 1
  }
  res <- paste(env$variable_list[[name]], "_", name, sep = "")
  res
}

# INFO: Get the names with IDX_Name
get_name <- function(name, env) {
  all_names <- names(env$variable_list)
  if (length(env$variable_list) == 0) {
    return(name)
  }
  if (!(name %in% all_names)) {
    return(name)
  }
  paste(env$variable_list[[name]], "_", name, sep = "")
}

# INFO: add the counter to the function name
create_name_fct <- function(line, env) {
  name <- deparse(line[[1]])
  name <- transf_fct_name(name)
  res <- paste(env$function_list[[name]], "_", name, sep = "")
  env$function_list[[name]] <- env$function_list[[name]] + 1
  res
}

# INFO: Forward name <- Call
add_forward_call <- function(line, env) {
  operation <- "forward"
  parse_line(line[[3]], env)
  prev_node1 <- env$graph$l[[env$graph$last_assigned]]
  connected_nodes <- prev_node1$name
  name <- deparse(line[[2]])
  name <- create_name_var(name, env)
  if (is_call(line[[2]])) {
    stopifnot("Only subsetted lhs is allowed" = is_subset(line[[2]]))
    parse_line(line[[2]], env)
    prev_node2 <- env$graph$l[[env$graph$last_assigned]]
    connected_nodes <- union(connected_nodes, prev_node2$name)
    # TODO: union could be wrong if one subset with itself var[var]
    name <- prev_node2$connected_nodes[1] |> create_name_var(env)
    # NOTE: First what is subsetted then the index
    operation <- "forward_subsetting"
  }
  env$graph$add_node(name,
    connected_nodes = connected_nodes,
    value = NA, operation = operation
  )
}

# INFO: Foward name <- Constant/variable
add_forward <- function(line, env) {
  name <- deparse(line[[2]]) |> create_name_var(env)
  value <- NA
  operation <- NULL
  value <- line[[3]]
  operation <- "forward"
  if (rlang::is_symbol(value)) {
    env$graph$add_node(name,
      connected_nodes = deparse(value) |> get_name(env),
      value = NA, operation = operation
    )
  } else {
    env$graph$add_node(name, value = value, operation = operation)
  }
}

# INFO: Create literal entry
create_literal <- function(value, env) {
  name <- paste0("LITERAL_", env$literal_counter + 1)
  env$literal_counter <- env$literal_counter + 1
  operation <- "forward"
  env$graph$add_node(name, value = value, operation = operation)
  return(name)
}

# INFO: add the last assigned node to connected nodes
elongate_connected_nodes <- function(env, connected_nodes) {
  if (length(env$graph$l) >= 1) {
    connected_nodes <- union(
      connected_nodes,
      env$graph$last_assigned
    )
  }
  connected_nodes
}

handle_expr <- function(expr, env) {
  connected_nodes <- NULL
  if (!is_call(expr)) {
    if (is.symbol(expr)) {
      connected_nodes <- deparse(expr)
    } else if (is_literal(expr)) {
      connected_nodes <- create_literal(expr, env)
    } else {
      stop("For now only literals and symbols are allowed")
    }
  } else {
    parse_line(expr, env)
    connected_nodes <- elongate_connected_nodes(env, connected_nodes)
  }
  return(connected_nodes)
}

add_binary <- function(line, env) {
  fct <- create_name_fct(line, env)
  connected_nodes <- handle_expr(line[[2]], env)
  connected_nodes <- c(
    connected_nodes,
    handle_expr(line[[3]], env)
  )
  connected_nodes <- sapply(connected_nodes, function(x) {
    get_name(x, env)
  })
  env$graph$add_node(fct,
    connected_nodes = connected_nodes,
    operation = deparse(line[[1]]) |> transf_fct_name()
  )
}

add_concatenate <- function(line, env) {
  fct <- create_name_fct(line, env)
  connected_nodes <- NULL
  connected_nodes <- lapply(line[-1], function(x) {
    if (!is_call(x)) {
      connected_nodes <- c(connected_nodes, deparse(x))
    } else {
      parse_line(x, env)
      connected_nodes <- elongate_connected_nodes(env, connected_nodes)
    }
    return(connected_nodes)
  })
  connected_nodes <- sapply(connected_nodes, function(x) {
    get_name(x, env)
  })
  env$graph$add_node(fct,
    connected_nodes = unlist(connected_nodes),
    operation = "concatenate"
  )
}

parse_line <- function(line, env) {
  if (!is.call(line)) {
    return(line)
  }
  line <- as.list(line)
  if (is_assign(line)) {
    if (!is_call(line[[3]])) {
      add_forward(line, env)
    } else {
      add_forward_call(line, env)
    }
  } else if (is_concatenate(line)) {
    add_concatenate(line, env)
  } else if (length(line) == 3) {
    add_binary(line, env)
  } # TODO: add unary operations
}

create_graph <- function(fct) {
  stopifnot(is.function(fct))
  b <- body(fct)
  if (deparse(b[[1]]) == "{") {
    b <- b[-1]
  }
  env <- new.env()
  env$graph <- Graph$new()
  env$function_list <- list(
    add = 0, sub = 0, mul = 0, div = 0, forward = 0,
    subsetting = 0, concatenate = 0,
    forward_subsetting = 0
  )
  env$variable_list <- list()
  env$literal_counter <- 0
  for (i in seq_along(b)) {
    parse_line(b[[i]], env)
  }
  return(env)
}
