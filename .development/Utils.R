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

# set deriv to 1 for last assign node
set_1_last_assign <- function(from_what, node_list) {
  idx <- which(grepl(from_what, ls(node_list)))
  idx <- idx[length(idx)]
  if (length(idx) == 0) {
    stop("Node not found")
  } else if (length(idx) == 1) {
    node_list[[ls(node_list)[idx]]]$deriv <- 1
  } else {
    stop("test")
  }
}
