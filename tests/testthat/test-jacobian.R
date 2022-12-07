context("Jacobian")

# finite differences
fd <- function(f, x) {
  delta <- 1e-6
  jac <- matrix(0, length(x), length(x))
  for(i in seq_along(1:length(x))) {
    for(j in seq_along(1:length(x))) {
      ej <- rep(0, length(x))
      ej[j] <- 1
      f1 <- f(x[i] + delta*ej)[i]
      f2 <- f(x[i] - delta*ej)[i]
      jac[i, j] <- (f1 - f2)/(2*delta)      
    }
  }
  
  return(jac)
}

test_that("we can compute the jacobian of a function", {
  
  # basic case
  f <- function(x, y) {
    #y <- numeric(2)
    y[1] <- x[1]^2 + sin(4)
    y[2] <- x[2]*7
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)
  
  # replacement at left side
  f <- function(x) {
    y1 <- y[1]
    y1 <- x[1]^2
    y[2] <- x[2]*7
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)
  
  # error
  f <- function(x) {
    a <- 1 
    y1 <- y[1]
    y1 <- x[1]^2
    y[2] <- x[2]*a
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)
  
  # error
  # several replacements at left side
  f <- function(x) {
    a <- 1 
    y1 <- y[1]
    y1 <- x[1]^2
    y[2] <- x[2]*a
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)
  
  # replacements at rs
  f <- function(x) {
    a <- 4*x[1]
    y[1] <- x[1]^2*a
    y[2] <- x[2]*a
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  
  
  # replacements at rs & ls
  f <- function(x) {
    a <- 4*x[1]
    y1 <- y[1]
    y2 <- y[2]
    y1 <- x[1]^2*a
    y2 <- x[2]*a
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)
  
  # if
  f <- function(x) {
    y[1] <- 2*x[1]^3
    if(y[1] < 3) {
      y[2] <- x[2]^2
    } else if(y[1] < 5) {
      y[2] <- x[2]^3
    } else {
      y[2] <- x[2]^4
    }
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)
  
  # if and replace at ls
  f <- function(x) {
    y2 <- y[2]
    y[1] <- 2*x[1]^3
    if(y[1] < 3) {
      y2 <- x[2]^2
    } else if(y[1] < 5) {
      y2 <- x[2]^3
    } else {
      y2 <- x[2]^4
    }
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)
  
  # if and replace at rs
  f <- function(x) {
    y2 <- y[2]
    a <- x[1]*sin(x[1])
    y[1] <- 2*x[1]^3
    if(y[1] < 3) {
      y[2] <- x[2]^2*a
    } else if(y[1] < 5) {
      y[2] <- x[2]^3 + a
    } else {
      y[2] <- x[2]^4 - a
    }
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)
  
  # if and replace at ls & rs
  f <- function(x) {
    y2 <- y[2]
    a <- x[1]*sin(x[1])
    y[1] <- 2*x[1]^3
    if(y[1] < 3) {
      y2 <- x[2]^2*a
    } else if(y[1] < 5) {
      y2 <- x[2]^3 + a
    } else {
      y2 <- x[2]^4 - a
    }
    return(y)
  }
  jac <- dfdr::jacobian(f, "y", "x")
  x <- c(2, 5)
  res <- jac(x)
  resfd <- fd(f, x)
  expect_equal(res, resfd)

})

