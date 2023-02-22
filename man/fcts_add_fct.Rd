\name{fcts_add_fct}
\alias{fcts_add_fct}
\title{appending a S4 class of type \emph{fcts}}
\usage{
fcts_add_fct(lst, f, f_deriv, keep = FALSE)
}
\arguments{
\item{lst}{is the S4 class of type \emph{fcts}. Newly created by \code{\link{fcts}()}}

\item{f}{is the function which should be differentiated. The argument has to be of type function.}

\item{f_deriv}{is a function defining the derivative of \emph{f}. The argument has to be of type function.}

\item{keep}{is a logical value. If set to TRUE the function \emph{f} is ignored of \code{\link{d}()}. The default value is FALSE.}
}
\value{
a S4 class of type \emph{fcts} extended by the new function-derivative pair.
}
\description{
A function which appends a S4 class of type \emph{fcts} with a new function-derivative pair.
}
\details{
The following functions are already supported: \cr
sin, sinh, asin, cos, cosh, acos, tan, tanh, atan, exp, log, sqrt, c, vector, numeric, rep and matrix. \cr
Notably, for the functions: c, vector, numeric, rep and matrix the function is ignored during differentiation.
}
\note{
The body of \emph{f} and \emph{f_deriv} have to be defined without curly brackets.
}
\examples{
library(dfdr)
# Initialize list
lst <- dfdr::fcts()

# The function which should be added
f <- function(x) x^2
# The dervative function of f
f_deriv <- function(x) 2*x

# add new entry to list
lst <- fcts_add_fct(lst, f, f_deriv)

g <- function(z) f(z)
df <- d(g, z, lst)
df
}
