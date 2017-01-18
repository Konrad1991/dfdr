[![Travis-CI Build Status](https://travis-ci.org/mailund/dfdr.svg?branch=master)](https://travis-ci.org/mailund/dfdr) [![Coverage Status](https://img.shields.io/codecov/c/github/mailund/dfdr/master.svg)](https://codecov.io/github/mailund/dfdr?branch=master)

dfdr -- automatic differentiation of simple functions in R
==========================================================

``` r
library(dfdr)

f <- function(x) x^2
df <- d(f, "x")
body(df)
```

    ## 2 * x^(2 - 1) * 1
