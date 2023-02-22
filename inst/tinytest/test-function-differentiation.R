df <- d(sin, x)
expect_equal(df, cos)

df <- d(cos, x)
expect_equal(body(df), quote(-sin(x)))

df <- d(exp, x)
expect_equal(df, exp)


f <- function(x) -sin(x)
df <- d(f, x)
expect_equal(body(df), quote(-cos(x)))

f <- function(x) -cos(x)
df <- d(f, x)
expect_equal(body(df), quote(sin(x)) )

f <- function(x) -exp(x)
df <- d(f, x)
expect_equal(body(df), quote(-exp(x)))



f1 <- function(x) x^2
f2 <- function(y) y
g <- function(z) f1(2*z)*f2(z^2)
h <- function(z) 4*z^4

zs <- seq(1,100,5)
expect_equal(g(zs), h(zs))

l <- dfdr:::fcts()
f1_deriv <- function(x) 2*x
f2_deriv <- function(y) 1
l <- dfdr:::add_fct(l, "f1", f1_deriv)
l <- dfdr:::add_fct(l, "f2", f2_deriv)
df <- dfdr::d(g, z, l)
  
dg <- df 
dh <- d(h, z)
expect_equal(dg(zs), dh(zs))


