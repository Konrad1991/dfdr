context("Constants and single variable expressions")

test_that("we can differentiate constants", {
  f <- function(x) 4
  df <- d(f, "x")
  expect_equal(body(df), quote(0))

  f <- function(x) y
  df <- d(f, "x")
  expect_equal(body(df), quote(0))
})

test_that("we can differentiate variables", {
  f <- function(x) x
  df <- d(f, "x")
  expect_equal(body(df), quote(1))

  f <- function(x) y
  df <- d(f, "x")
  expect_equal(body(df), quote(0))
})

test_that("we can differentiate variables times constants", {
  f <- function(x) 2*x
  df <- d(f, "x")
  expect_equal(body(df), quote(0 * x + 2 * 1))

  f <- function(x) -2*x
  df <- d(f, "x")
  expect_equal(body(df), quote(-0 * x + -2 * 1))

  f <- function(x) y*x
  df <- d(f, "x")
  expect_equal(body(df), quote(0 * x + y * 1))

  f <- function(x) 1/x
  df <- d(f, "x") # df/dx = -1/x**2
  expect_equal(body(df), parse(text = "(0 * x - 1 * 1)/`**`(x, 2)")[[1]])

})

test_that("we can differentiate exponetiation", {
  f <- function(x) x**2
  df <- d(f, "x")
  expect_equal(body(df), parse(text = "2 * `**`(x, 2 - 1) * 1")[[1]])

  f <- function(x) x**-2
  df <- d(f, "x")
  expect_equal(body(df), parse(text = "-2 * `**`(x, -2 - 1) * 1")[[1]])
})

test_that("we can differentiate addition and subtraction", {
  f <- function(x) x + x
  df <- d(f, "x")
  expect_equal(body(df), parse(text = "1 + 1")[[1]])

  f <- function(x) x - x
  df <- d(f, "x")
  expect_equal(body(df), parse(text = "1 - 1")[[1]])

  f <- function(x) x + y
  df <- d(f, "x")
  expect_equal(body(df), parse(text = "1 + 0")[[1]])

  f <- function(x) x - y
  df <- d(f, "x")
  expect_equal(body(df), parse(text = "1 - 0")[[1]])

  f <- function(x) -x
  df <- d(f, "x")
  expect_equal(body(df), parse(text = "-1")[[1]])
})

