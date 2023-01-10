context("Jacobian")

test_that("we can compute the jacobian of a function", {
  
  # basic case
  f <- function(x) {
    y <- numeric(2)
    y[1] <- x[1]^2 + sin(4)
    y[2] <- x[2]*7
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  res <- jac(x)
  d <- c(4, 0, 0, 7)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  # replacement at left side
  f <- function(x) {
    y <- numeric(2)
    y1 <- y[1]
    y1 <- x[1]^2
    y[2] <- x[2]*7
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  res <- jac(x)
  d <- c(4, 0, 0, 7)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  # replacement at right side
  f <- function(x) {
    y <- c(0, 0)
    a <- 7 
    y1 <- y[1]
    y1 <- x[1]^2
    y[2] <- x[2]*a
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  res <- jac(x)
  d <- c(4, 0, 0, 7)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  # several replacements at left side and right side
  f <- function(x) {
    y <- rep(0, 2)
    a <- 8
    y1 <- y[1]
    y1 <- x[1]^2
    y[2] <- x[2]*a
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  res <- jac(x)
  d <- c(4, 0, 0, 8)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  # replacements at rs
  f <- function(x) {
    y <- vector(length = length(x))
    a <- 4*x[1]
    y[1] <- x[1]^2*a
    y[2] <- x[2]*a
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  res <- jac(x)
  d <- c(48, 0, 20, 8)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  # replacements at rs & ls
  f <- function(x) {
    y <- c(0, 0)
    a <- 4*x[1]
    y1 <- y[1]
    y2 <- y[2]
    y1 <- x[1]^2*a
    y2 <- x[2]*a
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  res <- jac(x)
  d <- c(48, 0, 20, 8)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  # if
  f <- function(x, t) {
    y <- numeric(2)
    y[1] <- 2*x[1]^3
    if(t < 3) {
      y[2] <- x[2]^2
    } else if(t < 5) {
      y[2] <- x[2]^3
    } else {
      y[2] <- x[2]^4
    }
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  t <- 4
  res <- jac(x, t)
  d <- c(24, 0, 0, 75)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  # if and replace at ls
  f <- function(x, t) {
    y <- numeric(2)
    y2 <- y[2]
    y[1] <- 2*x[1]^3
    if(t < 3) {
      y2 <- x[2]^2
    } else if(t < 5) {
      y[2] <- x[2]^3
    } else {
      y[2] <- x[2]^4
    }
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  t <- 2.5
  res <- jac(x, t)
  d <- c(24, 0, 0, 10)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  
  # if and replace at ls & rs
  f <- function(x, t) {
    y <- numeric(2)
    y2 <- y[2]
    a <- x[1]*3
    y[1] <- 2*x[1]^3
    if(t < 3) {
      y2 <- x[2]^2
    } else if(t < 5) {
      y2 <- x[2]^3
    } else {
      y2 <- x[2]^4*a
    }
    return(y)
  }
  jac <- dfdr::jacobian(f, y, x)
  x <- c(2, 5)
  t <- 10
  res <- jac(x, t)
  d <- c(24, 0, 1875, 3000)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  # using a and b instead of x and y
  f <- function(a, t) {
    b <- vector(length = 2)
    b[1] <- a[1]*2
    b[2] <- a[2]*a[1]
    return(b)
  }
  jac <- dfdr::jacobian(f, b, a)
  x <- c(2, 5)
  t <- 10
  res <- jac(x, t)
  d <- c(2, 0, 5, 2)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  
  # using own function
  l <- dfdr:::fcts()
  f1 <- function(x) 2*x
  f1_deriv <- function(x) 2.5
  l <- dfdr:::add_fct(l, "f1", f1_deriv)
  f <- function(a, t) {
    b <- vector(length = 2)
    b[1] <- a[1]*2
    b[2] <- a[2]*a[1] + f1(a[1]^2)
    return(b)
  }
  jac <- dfdr::jacobian(f, b, a, l)
  x <- c(2, 5)
  t <- 10
  res <- jac(x, t)
  d <- c(2, 0, 15, 2)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  
  # using own const function
  l <- dfdr:::fcts()
  f1 <- function(x) 2*x
  f1_deriv <- function(x) 0
  l <- dfdr:::add_fct(l, "f1", f1_deriv, TRUE)
  f <- function(a, t) {
    b <- vector(length = 2)
    c <- f1(a[1])
    b[1] <- a[1]*2
    b[2] <- a[2]*a[1]*c*c + f1(a[1])*c - c + (a[1])/(c*a[1])
    return(b)
  }
  jac <- dfdr::jacobian(f, b, a, l)
  x <- c(2, 5)
  t <- 10
  res <- jac(x, t)
  d <- c(2, 0, 80, 32)
  nrow <- 2
  ncol <- 2
  expect_equal(res, matrix(d, nrow, ncol))
  
  
  # not allowed if term
  f <- function(a, t) {
    b <- vector(length = 2)
    
    term <- a[1]*2
    
    if(t < 3) {
      term <- a[1]*3
    }
    
    b[1] <- a[1]*2
    b[2] <- a[2]*a[1]*term
    return(b)
  }
  expect_error(dfdr::jacobian(f, b, a))

})

