context("Expression simplification")

test_that("We can simplify numeric expressions", {
  expect_equal(simplify_expr(quote(1)), 1)
  expect_equal(simplify_expr(quote(0)), 0)

  expect_equal(simplify_expr(quote(1 + 0)), 1)
  expect_equal(simplify_expr(quote(1 + 1)), 2)

  expect_equal(simplify_expr(quote(1 - 0)), 1)
  expect_equal(simplify_expr(quote(3 - 1)), 2)

  expect_equal(simplify_expr(quote(1 * 0)), 0)
  expect_equal(simplify_expr(quote(3 * 1)), 3)

  expect_equal(simplify_expr(quote(2 / 1)), 2)
  expect_equal(simplify_expr(quote(3 / 1)), 3)
  expect_equal(simplify_expr(quote(0 / 1)), 0)

  expect_equal(simplify_expr(quote(0 ** 0)), 1)
  expect_equal(simplify_expr(quote(0 ** 1)), 0)
  expect_equal(simplify_expr(quote(5 ** 0)), 1)
  expect_equal(simplify_expr(quote(5 ** 1)), 5)
  expect_equal(simplify_expr(quote(2 ** 2)), 4)
})
