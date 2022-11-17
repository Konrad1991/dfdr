context("Error handling")

test_that("We get an error with an unexpected call", {
  f <- function(x) rnorm(1, x)
  expect_error(dfdr::d(f, "x"), "The function rnorm is not supported")
})
