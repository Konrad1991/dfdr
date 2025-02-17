setwd("./.development")
source("Utils.R")
source("NodeClass.R")
source("GraphClass.R")
source("Parsing.R")

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
env$graph$get_value("y") == f()
env$graph$get_deriv("a") == 3
env$graph$get_deriv("b") == 8
env$graph$get_deriv("c") == 0

source("Utils.R")
source("NodeClass.R")
source("GraphClass.R")
source("Parsing.R")
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
f()
env <- create_graph(f)
env$graph$forward_pass()
env$graph$backward_pass("y")
env$graph
