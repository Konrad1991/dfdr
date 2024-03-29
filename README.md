<!-- badges: start -->
[![](https://cranlogs.r-pkg.org/badges/last-month/dfdr?color=green)](https://cran.r-project.org/package=dfdr)
[![](https://www.r-pkg.org/badges/version/dfdr?color=green)](https://cran.r-project.org/package=dfdr)
[![License: GPL3](https://img.shields.io/badge/license-GPL3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)
<!-- badges: end -->

## News

- Original code was written by Thomas Mailund (https://github.com/mailund)
- In late 2022 Thomas and I (Konrad Krämer) extanded `dfdr`
- In 2022 maintanance was passed to Konrad Krämer 

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
f <- function(x) x^2 + sin(x)
df <- d(f, "x")
df
```

    ## function (x) 
    ## 2 * x + cos(x)

We can plot a function together with selected tangents to see how it works:

``` r
plot_tangent <- function(f, var, at, L = 1, df = NULL) {
  if (is.null(df)) {
    x <- substitute(var)
    df <- d(f, x)
  }
  a <- df(at)
  w <- L / sqrt(1 + a^2)
  v <- a * w
  x <- c(at - w, at + w)
  y <- c(f(at) - v, f(at) + v)
  lines(x, y, lty = "dashed", col = "darkred")
  points(at, f(at), pch = 20)
}

x <- seq(-1.5, 0.9, length.out = 100)
plot(x, f(x), type = "l", asp = 1)
plot_tangent(f, x, -1)
plot_tangent(f, x, -0.2)
plot_tangent(f, x, 0.1)
plot_tangent(f, x, 0.7)
```

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
plot(x, sin(x), type = "l", asp = 1)
plot_tangent(sin, x, -2)
plot_tangent(sin, x, -1)
plot_tangent(sin, x, 0.5)
plot_tangent(sin, x, 2.5)
```

![](README_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
plot(x, exp(x), type = "l")
plot_tangent(exp, x, -2, L = 1)
plot_tangent(exp, x, 0.0, L = 1)
plot_tangent(exp, x, 1.5, L = 3)
plot_tangent(exp, x, 2.5, L = 4)
```

![](README_files/figure-markdown_github/unnamed-chunk-5-1.png)

The body of the deriatives are simplified to a certain extend, but in a depth-first approach with no rewriting of expressions, so thehy sometimes can be more complex than you would normally see them.

Currently, it just handles arithmetic expressions and a few builtin functions, but I will implement handling of functions in the expressions as well, soon.
