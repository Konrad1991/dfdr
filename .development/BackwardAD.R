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
source("Utils.R")
source("Operations.R")
source("NodeClass.R")
source("GraphClass.R")
source("Parsing.R")

f <- function() {
  # Expression is: y = (a + b) * b + c =
  # results in a*b + b^2 + c(a, b, a, b) = 2*3 + 3^2 + c(2, 3, 2, 3) =
  # => c(17, 18, 17, 18)
  # Deriv with respect to a: dy/da = b + c = b + c(a, b, a, b) = c(4, 3, 4, 3)
  # Deriv with respect to b: dy/db = a + 2*b + c = c(8, 9, 8, 9)
  a <- 2
  b <- 3
  c <- 4 # c(a, b, a, b)
  a <- a + b + 3
  y <- a * b + c
  y
}
f()
env <- create_graph(f)
env$graph$forward_pass()
env$graph$backward_pass("y_ITER_0")
env$graph
