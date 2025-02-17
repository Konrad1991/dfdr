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

# INFO: Creates name_ITER_IDX
create_name_forward <- function(name, env) {
  name <- gsub("_ITER_[0-9]+", "", name)
  if (!(name %in% names(env$variable_list))) {
    env$variable_list[[name]] <- 0
  } else {
    env$variable_list[[name]] <- env$variable_list[[name]] + 1
  }
  res <- paste(name, "_ITER_", env$variable_list[[name]], sep = "")
  res
}

# INFO: Forward name <- Call
add_forward_call <- function(line, env) {
  operation <- "forward"
  parse_line(line[[3]], env)
  prev_node1 <- env$graph$l[[length(env$graph$l)]]
  connected_nodes <- prev_node1$name
  name <- deparse(line[[2]])
  name <- create_name_forward(name, env)
  if (is_call(line[[2]])) {
    stopifnot("Only subsetted lhs is allowed" = is_subset(line[[2]]))
    env$left_subset <- TRUE
    parse_line(line[[2]], env)
    prev_node2 <- env$graph$l[[length(env$graph$l)]]
    connected_nodes <- union(connected_nodes, prev_node2$name)
    name <- prev_node2$connected_nodes[1] |> create_name_forward(env)
    # First what is subsetted then the index
    operation <- "forward_subsetting"
  }
  env$graph$add_node(name,
    connected_nodes = connected_nodes,
    value = NA, operation = operation
  )
}

# INFO: Foward name <- Constant
add_forward <- function(line, env) {
  name <- deparse(line[[2]])
  value <- NA
  operation <- NULL
  value <- line[[3]]
  operation <- "forward"
  if (rlang::is_symbol(value)) {
    env$graph$add_node(name,
      connected_nodes = deparse(value),
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

elongate_connected_nodes <- function(env, connected_nodes) {
  if (length(env$graph$l) >= 1) {
    connected_nodes <- union(
      connected_nodes,
      env$graph$l[[length(env$graph$l)]]$name
    )
  }
  connected_nodes
}

# INFO: Get the names with ITER_IDX
get_names_with_ITER <- function(name, env) {
  all_names <- names(env$variable_list)
  if (length(env$variable_list) == 0) {
    return(name)
  }
  if (!(name %in% all_names)) {
    return(name)
  }
  paste(name, "_ITER_", env$variable_list[[name]], sep = "")
}

create_name <- function(line, env) {
  name <- deparse(line[[1]])
  name <- transf_fct_name(name)
  res <- paste(name, env$counter_list[[name]], sep = "")
  env$counter_list[[name]] <- env$counter_list[[name]] + 1
  res
}

add_binary <- function(line, env) {
  fct <- create_name(line, env)
  connected_nodes <- NULL
  if (!is_call(line[[2]])) {
    if (is.symbol(line[[3]])) {
      connected_nodes <- deparse(line[[2]])
    } else if (is_literal(line[[2]])) {
      connected_nodes <- create_literal(line[[2]], env)
    } else {
      stop("For now only literals and symbols are allowed")
    }
  } else {
    parse_line(line[[2]], env)
    connected_nodes <- elongate_connected_nodes(env, connected_nodes)
  }
  if (!is_call(line[[3]])) {
    if (is.symbol(line[[3]])) {
      connected_nodes <- union(
        connected_nodes,
        deparse(line[[3]])
      )
    } else if (is_literal(line[[3]])) {
      connected_nodes <- union(
        connected_nodes,
        create_literal(line[[3]], env)
      )
    } else {
      stop("For now only literals and symbols are allowed")
    }
  } else {
    parse_line(line[[3]], env)
    connected_nodes <- elongate_connected_nodes(env, connected_nodes)
  }
  connected_nodes <- sapply(connected_nodes, function(x) {
    get_names_with_ITER(x, env)
  })
  env$graph$add_node(fct,
    connected_nodes = connected_nodes,
    operation = deparse(line[[1]]) |> transf_fct_name()
  )
}

add_concatenate <- function(line, env) {
  fct <- create_name(line, env)
  connected_nodes <- NULL
  connected_nodes <- lapply(line[-1], function(x) {
    if (!is_call(x)) {
      connected_nodes <- union(connected_nodes, deparse(x))
    } else {
      parse_line(x, env)
      connected_nodes <- elongate_connected_nodes(env, connected_nodes)
    }
    return(connected_nodes)
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
  env$counter_list <- list(
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
