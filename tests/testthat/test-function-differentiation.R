context("Differentiation of function calls")

test_that("we can differentiate known functions", {
  df <- d(sin, x)
  expect_equal(df, cos)

  df <- d(cos, x)
  expect_equal(body(df), quote(-sin(x)))

  df <- d(exp, x)
  expect_equal(df, exp)
})

test_that("we can differentiate expressions with functions", {
  f <- function(x) -sin(x)
  df <- d(f, x)
  expect_equal(body(df), quote(-(cos(x) * 1)))

  f <- function(x) -cos(x)
  df <- d(f, x)
  expect_equal(body(df), quote(-(-sin(x) * 1)))

  f <- function(x) -exp(x)
  df <- d(f, x)
  expect_equal(body(df), quote(-(exp(x) * 1)))
})
