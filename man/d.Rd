\name{d}
\alias{d}
\title{Differentiate a function for a single variable.}
\usage{
d(f, x, derivs = NULL)
}
\arguments{
\item{f}{The function to differentiate.}

\item{x}{The variable that f should be differentiated with respect to.}

\item{derivs}{An S4 class of type \emph{fcts} that defines additional derivatives. See \code{\link{fcts}} for details.}
}
\value{
For example function f and symbol x: \cr
\emph{df/dx}
}
\description{
Differentiate a function for a single variable.
}
\details{
The following functions are already supported: \cr
sin, sinh, asin, cos, cosh, acos, tan, tanh, atan, exp, log, sqrt, c, vector, numeric, rep and matrix. \cr
Notably, for the functions: c, vector, numeric, rep and matrix the function is ignored during differentiation.
}
\examples{
library(dfdr)
d(sin, x)

f <- function(x) -sin(x)
d(f, x)

# Initialize list
lst <- dfdr::fcts()
# The function which should be added
f <- function(x) x^2
# The dervative function of f
f_deriv <- function(x) 2*x
# add new entry to list
lst <- fcts_add_fct(lst, f, f_deriv)
g <- function(z) f(z)
d(g, z, lst)
}
