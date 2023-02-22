f <- function(x) 4
df <- d(f, "x")
expect_equal(body(df), quote(0))

f <- function(x) y
df <- d(f, "x")
expect_equal(body(df), quote(0))



f <- function(x) x
df <- d(f, "x")
expect_equal(body(df), quote(1))

f <- function(x) y
df <- d(f, "x")
expect_equal(body(df), quote(0))



f <- function(x) 2*x
df <- d(f, "x")
expect_equal(body(df), 2)

f <- function(x) -2*x
df <- d(f, "x")
expect_equal(body(df), -2)

f <- function(x) y*x
df <- d(f, "x")
expect_equal(body(df), quote(y))

f <- function(x) 1/x
df <- d(f, "x")
expect_equal(body(df), quote(-1/x^2))




f <- function(x) 2*(x + 5)
df <- d(f, "x")
expect_equal(body(df), 2)

f <- function(x, y) 2*(x + y)
df <- d(f, "x")
expect_equal(body(df), 2)



f <- function(x) x^2
df <- d(f, "x")
expect_equal(body(df), quote(2*x))

f <- function(x) x^-2
df <- d(f, "x")
expect_equal(body(df), quote(-2*x^-3))



f <- function(x) x + x
df <- d(f, "x")
expect_equal(body(df), 2)

f <- function(x) x - x
df <- d(f, "x")
expect_equal(body(df), 0)

f <- function(x) x + y
df <- d(f, "x")
expect_equal(body(df), 1)

f <- function(x) x - y
df <- d(f, "x")
expect_equal(body(df), 1)

f <- function(x) -x
df <- d(f, "x")
expect_equal(body(df), -1)


