if (!grepl(".development", getwd())) {
  setwd("./.development")
}
source("NodeClass.R")
source("GraphClass.R")
source("Parsing.R")

# Concatenation with call
f <- function() {
  a <- 2
  b <- 3
  c <- c(a * a, a + b)
  y <- c * c
  y
}
env <- create_graph(f)
env$graph$forward_pass()
env$graph$backward_pass("y")
# fct is y = [a^2, a + b] * [a^2, a + b] = [a^4, a^2 + b^2 + 2ab]
# dy/da is [4a^3, 2a + 2b] = [32, 10]
# dy/db is [0, 2b + 2a] = [0, 10]
stopifnot(env$graph$get_value("y") == f())
stopifnot(env$graph$get_deriv("a") == c(32, 10))
stopifnot(env$graph$get_deriv("b") == c(0, 10))

# Considerung thaat graph can be translated to C++
sapply(ls(env$graph$node_list), function(x) {
  env$graph$node_list[[x]] |> class()
})
env$graph$sorted_nodes

# Basic case
f <- function() {
  a <- 2
  b <- 3
  c <- 0
  a <- a + b
  y <- a * b + c
  y <- a * b
  y
}
# fct is y = (a + b) * b + 3 = a*b + b^2 + 3
# dy/da is b = 3
# dy/db is a + 2*b = 2 + 2*3 = 8
env <- create_graph(f)
env$graph$forward_pass()
env$graph$backward_pass("y")
stopifnot(env$graph$get_value("y") == f())
stopifnot(env$graph$get_deriv("a") == 3)
stopifnot(env$graph$get_deriv("b") == 8)
stopifnot(env$graph$get_deriv("c") == 0)

# Basic concatenation
f <- function() {
  a <- 2
  b <- 3
  c <- c(a, b, a, b)
  y <- a * b + c
  y
}
# fct is y = a*b + c = a * b + [a, b, a, b] =
# dy/da is b + [1, 0, 1, 0] = [4, 3, 4, 3]
# dy/db is a + [0, 1, 0, 1] = [2, 3, 2, 3]
env <- create_graph(f)
env$graph$forward_pass()
env$graph$backward_pass("y")
stopifnot(env$graph$get_value("y") == c(8, 9, 8, 9))
stopifnot(env$graph$get_deriv("a") == c(4, 3, 4, 3))
stopifnot(env$graph$get_deriv("b") == c(2, 3, 2, 3))
