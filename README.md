[![Travis-CI Build Status](https://travis-ci.org/mailund/dfdr.svg?branch=master)](https://travis-ci.org/mailund/dfdr) [![Coverage Status](https://img.shields.io/codecov/c/github/mailund/dfdr/master.svg)](https://codecov.io/github/mailund/dfdr?branch=master)

dfdr — Automatic differentiation of simple functions in R
=========================================================

The `dfdr` package implements a simple version of automatic differentiation. It takes functions that consist of a single expression and construct the derivative with respect to a specific variable.

To install `dfdr` you can use `devtools`.

``` r
install.packages("devtools")
devtools::install_github("mailund/dfdr")
```

and then load the library with

``` r
library(dfdr)
```

To compute the derivative of a function, you use the function `d`. It takes two arguments, the function to compute the derivative of and the variable to compute the derivative with respect to.

``` r
f <- function(x) x^2
df <- d(f, "x")
df
```

    ## function (x) 
    ## 2 * x^(2 - 1) * 1

The body of the deriatives are not simplified right now, so they tend to be more complex than you would normally see them. When computing the derivative, `d` simply follows simple rules for computing the derivatives of expressions and handles complex expressions using the chain rule. I plan to add partial evaluation to the function later to simplify expressions.

Currently, it just handles arithmetic expressions, but I will implement handling of functions in the expressions as well, soon.
