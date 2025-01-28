# PLAN:
# Code --> GRAPH --> AST_DERIV --> Code_DERIV
# When parsing GRAPH to AST_DERIV the RHS is assembled to one call
# This only works in ast2ast. As here each variable holds a
# value and a derivative. Thus, variables type resembles the
# Node class in R.
# 1. Add unary operations
# 2. How to handle if
#  - each if branch is stored in a special Node class
#  - the logical; the branch stored in a graph
# 3. add Node for constants
# 4. Subsetted lhs
# setwd("./.development")
source("Operations.R")
source("NodeClass.R")
source("GraphClass.R")
source("Parsing.R")

f <- function() {
  zero <- 0
  idx1 <- 3
  y <- c(zero, zero, zero)
  x1 <- 4
  x2 <- 2
  x3 <- 3
  a <- c(x1, x2, x3)
  b <- x1 + x2
  y[idx1] <- a * b + a[idx1]
  # y <- a * b + a[idx1]
}
env <- create_graph(f)
env$graph$forward_pass()
env$graph$backward_pass("y_ITER_1")
env$graph


f <- function() {
  # Expression is: y = (a + b) * b = a*b + b^2
  # Deriv with respect to a: dy/da = b = 3
  # Deriv with respect to b: dy/db = a + 2*b = 8
  a <- 2
  b <- 3
  a <- a + b
  y <- a * b
}
env <- create_graph(f)
env$graph$forward_pass()
env$graph$backward_pass("y_ITER_0")
env$graph
