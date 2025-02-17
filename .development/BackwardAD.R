# setwd("./.development")
source("NodeClass.R")
source("GraphClass.R")
source("Parsing.R")

f <- function() {
  a <- 2
  b <- 3
  c <- 0
  a <- a + b
  y <- a * b + c
  y
}

f()
# y = (a + b) * b + 3 = a*b + b^2 + 3
# dy/da = b = 3
# dy/db = a + 2*b = 2 + 2*3 = 8
env <- create_graph(f)
env$graph$forward_pass()
env$graph$backward_pass("y_ITER_0")
env$graph
env$graph$assignments
