if (!grepl(".development", getwd())) {
  setwd("./.development")
}
source("NodeClass.R")
source("GraphClass.R")
source("Parsing.R")
# Lines instead of operations
g <- graph$new()
g$node_list[["0_a"]] <- node_forward$new("0_a", NULL, 2)
g$node_list[["0_b"]] <- node_forward$new("0_b", NULL, 3)
value_fct <- function(l) {
  l[[1]] - l[[2]]
}
deriv_fct <- function(l) {
  c(l, -l)
}
g$node_list[["0_line"]] <- node_line$new(
  "0_line", c("0_a", "0_b"),
  value_fct, deriv_fct
)
g$node_list[["0_y"]] <- node_forward$new("0_y", "0_line", NA)
g$forward_pass()
g$backward_pass("0_y")
sapply(ls(g$node_list), function(x) {
  g$node_list[[x]]
})

# Lines instead of operations
g <- graph$new()
g$node_list[["a"]] <- node_forward$new("a", NULL, 2)
g$node_list[["b"]] <- node_forward$new("b", NULL, 3)
expr <- quote(a - b + a * a)
g$node_list[["line"]] <- node_line2$new("line", c("a", "b"), expr)
g$node_list[["y"]] <- node_forward$new("y", "line", NA)
g$forward_pass()
g$backward_pass("y")
sapply(ls(g$node_list), function(x) g$node_list[[x]])

# In C++:
# Store expr as a std::function<Vec<double>(const std::vector<Node*>&)>
# This keeps it as a lambda function that operates on a vector of pointers to Node.
# Pass a std::vector<Node*> as inputs
# Each Node* contains a Vec<double> value, so the lambda can extract and compute based on it.
